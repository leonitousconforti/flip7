module public Play

open System
open System.IO
open System.Threading.Channels
open System.Threading.Tasks

open FSharp.Control

open Flip7

let private width = 80
let private playerSlots = 5
let private paceMilliseconds = 400
let private terminalPrompt = "TerminalPrompt"
let private adaptive = "Adaptive"
let private sageName = "Sage"
let private timelinesRoot = "timelines"

type private QuitException() =
    inherit Exception()

// Every previously persisted game is training data: Sage starts each session
// already knowing how its regulars play. A directory that does not read back
// (a session killed mid-write, say) is skipped rather than fatal
let private LoadHistory () : Instant list list =
    if not (Directory.Exists timelinesRoot) then
        []
    else
        Directory.GetDirectories timelinesRoot
        |> Array.sort
        |> Array.toList
        |> List.choose (fun directory ->
            try
                Persistence.ReadTimeline directory
                |> AsyncSeq.toListAsync
                |> Async.RunSynchronously
                |> Some
            with _ ->
                None
        )

type private Prompt = {
    Round: uint
    Turn: uint
    At: int
    Player: Player
    Others: Player list
    Finished: Player list
    Deck: Deck
    Discards: Deck
}

// An action card the human has to give to someone. There is no round or turn
// here: a targeting decider is not told them, which is also why the editor
// cannot be opened mid-aim - it needs both to splice a timeline
type private Aim = {
    At: int
    Ask: Strategy.Ask
    Chooser: Player
    Candidates: Player list
    Deck: Deck
    Discards: Deck
}

type private Decision =
    | Answered of Strategy.HitOrStand
    | Aimed of Player
    | Forked of Editor.Splice
    | Quitting

type private Model = {
    // The open ask, held here from the decider's dispatch until answered
    Prompt: Prompt option
    // The open target ask, held the same way. Only ever one of the two: the
    // engine suspends at whichever it asked
    Aim: Aim option
    // Which candidate the picker sits on, kept by name so that it reads the
    // same way the editor's cursor does
    Choice: string option
    // Who was asked, against the instant their ask arrived at. Keyed by
    // instant rather than holding only the latest: the engine races on once an
    // answer is sent and can raise the next ask while the screen is still on
    // the frame the last one arrived at, so which player a frame is waiting on
    // is a property of that frame, not of whichever ask happens to be open now
    AskedAt: Map<int, string>
    // The editing overlay; only ever opened while a prompt is being asked
    Editor: Editor.Model option
    // The newest instant shown; the screen is always a view of its frame
    Cursor: int
    Snapshot: int * bool * int list
    Frame: Result<Instant, exn> option
    // The index of the one in-flight frame read: declaring it here is what
    // requests it, and the cursor never advances past an unread frame
    Reading: int option
    // Whether a snapshot refresh is in flight: declaring it here is what
    // requests one, re-requested by the heartbeat
    Refreshing: bool
    // The answer to the current prompt: declaring it here is what sends it
    Resolved: Decision option
    Quit: bool
}

type private Msg =
    | Pressed of ConsoleKeyInfo
    | Prompted of Prompt
    | Aiming of Aim
    | Refreshed of (int * bool * int list)
    | Loaded of index: int * result: Result<Instant, exn>
    | Quitted
    | Tick

type private Effect =
    | RefreshSnapshot
    | ReadFrame of index: int
    | Resolve of Decision

let private init: Model = {
    Prompt = None
    Aim = None
    Choice = None
    AskedAt = Map.empty
    Editor = None
    Cursor = 0
    Snapshot = 0, false, []
    Frame = None
    Reading = None
    Refreshing = false
    Resolved = None
    Quit = false
}

// There is no phase machine: what the screen is doing is derived from the
// facts above. The index of the frame on screen is the cursor once its read
// has landed
let private showing (model: Model) : int option =
    match model.Reading, model.Frame with
    | None, Some _ -> Some model.Cursor
    | _ -> None

// A prompt is asked only once every instant preceding it has been shown
let private asking (model: Model) : Prompt option =
    model.Prompt
    |> Option.filter (fun prompt -> showing model = Some(prompt.At - 1))

// ...and so is a target ask
let private aiming (model: Model) : Aim option =
    model.Aim |> Option.filter (fun aim -> showing model = Some(aim.At - 1))

// The candidates in the order they sit on screen, which the store already
// emits seated, so the picker moves the way the eye does. Falls back to the
// engine's rotation order if the frame does not cover every candidate
let private inSeatedOrder (model: Model) (aim: Aim) : Player list =
    let seated =
        match model.Frame with
        | Some(Ok instant) ->
            instant.Players
            |> List.choose (fun seat -> aim.Candidates |> List.tryFind (fun c -> c.Name = seat.Name))
        | _ -> []

    if List.length seated = List.length aim.Candidates then
        seated
    else
        aim.Candidates

// Who the picker is on: the remembered candidate while they are still one,
// otherwise the first on screen
let private chosen (model: Model) (aim: Aim) : Player =
    let order = inSeatedOrder model aim

    model.Choice
    |> Option.bind (fun name -> order |> List.tryFind (fun candidate -> candidate.Name = name))
    |> Option.defaultValue (List.head order)

// Who the frame on screen is waiting on, if anyone. Answered from the moment
// the ask lands until the screen moves past that frame, which outlasts the ask
// itself: answering resumes the engine, but the instant that follows still
// takes a pace interval to be written, ingested and read. Tying this to the
// open ask instead drops the annotation and pops the previous actor's
// highlight back on for that whole gap, one repaint after the answer.
let private parked (model: Model) : string option =
    showing model
    |> Option.bind (fun shown -> Map.tryFind (shown + 1) model.AskedAt)

// The game is over once the final instant of a complete timeline has been
// shown
let private gameOver (model: Model) : bool =
    let count, isComplete, _ = model.Snapshot
    isComplete && count > 0 && showing model = Some(count - 1)

// The editor's players come from the prompt in rotation order, but its rows
// follow the shown frame's order, which the store already emits seated
let private editorFor (model: Model) (prompt: Prompt) : Editor.Model =
    let seating =
        match model.Frame with
        | Some(Ok instant) -> Some(instant.Players |> List.map (fun player -> player.Name))
        | _ -> None

    Editor.Make
        prompt.Round
        prompt.Turn
        seating
        (prompt.Player :: prompt.Others)
        prompt.Finished
        prompt.Deck
        prompt.Discards

let private update (msg: Msg) (model: Model) : Model =
    match msg with
    | Tick ->
        let count, _, _ = model.Snapshot
        let model = { model with Refreshing = true }

        // No guard against advancing past an open prompt is needed: the
        // engine is suspended at the ask, so no instant past it exists yet
        if model.Reading.IsSome then
            model
        elif model.Cursor < count - 1 then
            // The store meters ingestion to the pace, so chasing the
            // newest instant is what unfolds the game at watchable speed
            {
                model with
                    Cursor = model.Cursor + 1
                    Reading = Some(model.Cursor + 1)
            }
        else
            model

    | Refreshed((count', _, _) as snapshot) ->
        let model' = { model with Snapshot = snapshot; Refreshing = false }

        if count' > 0 && model.Frame.IsNone && model.Reading.IsNone then
            // The first instants arrived: fetch the cursor's frame
            { model' with Reading = Some model.Cursor }
        else
            model'

    | Loaded(index, result) ->
        if index = model.Cursor then
            { model with Reading = None; Frame = Some result }
        else
            { model with Reading = Some model.Cursor }

    // A new question has no answer yet. The reset is load-bearing: two
    // prompts in a row answered identically must still each produce a
    // Resolved change for the effects diff to send
    | Prompted prompt -> {
        model with
            Prompt = Some prompt
            AskedAt = Map.add prompt.At prompt.Player.Name model.AskedAt
            Resolved = None
      }

    | Aiming aim -> {
        model with
            Aim = Some aim
            Choice = None
            AskedAt = Map.add aim.At aim.Chooser.Name model.AskedAt
            Resolved = None
      }

    | Quitted -> { model with Quit = true }

    | Pressed key ->
        match model.Editor, asking model, aiming model with
        | Some editor, Some _, _ ->
            match Editor.Key key editor with
            | Editor.Editing editor' -> { model with Editor = Some editor' }
            | Editor.Cancelled -> { model with Editor = None }
            | Editor.Committed splice -> {
                model with
                    Editor = None
                    Prompt = None
                    Resolved = Some(Forked splice)
              }

        | _, _, Some aim ->
            let order = inSeatedOrder model aim
            let current = chosen model aim

            let move (step: int) =
                let index = order |> List.findIndex (fun candidate -> candidate.Name = current.Name)
                let index' = (index + step + List.length order) % List.length order
                { model with Choice = Some (List.item index' order).Name }

            match key.Key with
            | ConsoleKey.UpArrow
            | ConsoleKey.LeftArrow -> move -1
            | ConsoleKey.DownArrow
            | ConsoleKey.RightArrow -> move 1
            | ConsoleKey.Enter -> {
                model with
                    Aim = None
                    Choice = None
                    Resolved = Some(Aimed current)
              }
            | ConsoleKey.Q
            | ConsoleKey.Escape -> {
                model with
                    Aim = None
                    Choice = None
                    Resolved = Some Quitting
              }
            | _ -> model

        | _, Some prompt, _ ->
            match key.Key with
            | ConsoleKey.H -> {
                model with
                    Prompt = None
                    Resolved = Some(Answered Strategy.Hit)
              }
            | ConsoleKey.S -> {
                model with
                    Prompt = None
                    Resolved = Some(Answered Strategy.Stand)
              }
            | ConsoleKey.Q
            | ConsoleKey.Escape -> { model with Prompt = None; Resolved = Some Quitting }
            | ConsoleKey.E -> { model with Editor = Some(editorFor model prompt) }
            | _ -> model

        | _, _, _ when gameOver model -> { model with Quit = true }
        | _ -> model

let private effects (before: Model) (after: Model) : Effect list = [
    if after.Refreshing && not before.Refreshing then
        RefreshSnapshot

    if after.Reading <> before.Reading then
        match after.Reading with
        | Some index -> ReadFrame index
        | None -> ()

    if after.Resolved <> before.Resolved then
        match after.Resolved with
        | Some decision -> Resolve decision
        | None -> ()
]

let private viewKey (model: Model) =
    let _, _, roundEnds = model.Snapshot

    model.Editor,
    asking model,
    aiming model,
    parked model,
    model.Choice,
    gameOver model,
    model.Frame,
    Persistence.TimelineStore.RoundOf roundEnds model.Cursor

let private promptFooter (prompt: Prompt) : string =
    let bust =
        Simulation.probabilityToBust prompt.Deck prompt.Discards prompt.Player.Hand (List.isEmpty prompt.Others)
        * 100.0

    let ev =
        Simulation.expectedValueOfHit prompt.Deck prompt.Discards prompt.Player.Hand

    $"{prompt.Player.Name}: %d{Hand.Score prompt.Player.Hand}pts in hand, %.0f{bust}%% bust, EV %+.1f{ev}   [h]it   [s]tand   [e]dit   [q]uit"
    |> centered width
    |> styled [ Ansi.Bright ]

let private aimFooter (aim: Aim) (choice: Player) : string =
    let card =
        match aim.Ask with
        | Strategy.Ask.WhoToFreeze -> "Freeze"
        | Strategy.Ask.WhoReceivesDeal3 -> "Deal3"
        | Strategy.Ask.WhoReceivesSecondChance -> "SecondChance"

    let verb =
        if choice.Name = aim.Chooser.Name then
            "keep it yourself"
        else
            $"give it to {choice.Name}"

    // Every row already carries its own points and bust odds, and the footer
    // has to stay inside the 80 columns or it wraps and leaves a torn line
    // behind on the next frame
    $"{aim.Chooser.Name} flipped {card}: {verb}?   [↕] pick   [enter] give   [q]uit"
    |> centered width
    |> styled [ Ansi.Bright ]

let private view (directory: string) (model: Model) : unit =
    match model.Editor with
    | Some editor when editor.Help -> Editor.RenderHelp()
    | Some editor -> Editor.Render editor
    | None ->
        match model.Frame with
        | None -> ()
        | Some(Error error) -> RenderError directory model.Snapshot model.Cursor error
        | Some(Ok instant) ->
            let _, _, roundEnds = model.Snapshot
            let round = Persistence.TimelineStore.RoundOf roundEnds model.Cursor
            let prompting = parked model
            let aimedAt = aiming model |> Option.map (fun aim -> (chosen model aim).Name)

            let footer =
                match asking model, aiming model with
                | Some prompt, _ -> promptFooter prompt
                | _, Some aim -> aimFooter aim (chosen model aim)
                | _ when gameOver model ->
                    let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)

                    $"game over: {winner.Name} wins with {winner.FirmScore}pts!   [any key]"
                    |> centered width
                    |> styled [ Ansi.BrightGreen ]
                | _ -> "[q at your turn] quit" |> centered width |> styled [ Ansi.Dim; Ansi.Cyan ]

            RenderPlay round instant prompting aimedAt footer

let public Run (humanNames: string list) (seed: int option) (pace: int option) : Async<unit> = async {
    if humanNames.Length < 1 || humanNames.Length > 5 then
        raise (ArgumentException "Please provide one to five player names as command-line arguments.")
    if humanNames |> List.exists String.IsNullOrWhiteSpace then
        raise (ArgumentException "Player names must not be empty.")
    if humanNames |> List.distinct |> List.length <> humanNames.Length then
        raise (ArgumentException "Player names must be unique.")

    let botNames =
        [ sageName; "Alice"; "Bob"; "Chloe"; "Dave" ]
        |> List.take (playerSlots - humanNames.Length)

    match humanNames |> List.tryFind (fun name -> botNames |> List.contains name) with
    | Some taken -> raise (ArgumentException $"{taken} is taken by one of the AIs, please pick another name.")
    | None -> ()

    // Sage takes the first free seat, so it only exists (and only pays for
    // reading the persisted games) when at least one seat is open
    let sage =
        if List.isEmpty botNames then
            None
        else
            Some(Sage(LoadHistory()))

    let random =
        match seed with
        | Some value -> Random(value)
        | None -> Random()

    let paceMs = defaultArg pace paceMilliseconds
    let pool = [
        Strategy.HitUntilScore 22u
        Strategy.HitUntilScore 26u
        Strategy.HitUntilNumCards 5u
        Strategy.HitUntilBustProbability 0.35
        Strategy.HitUntilNaiveBustProbability 0.4
        Strategy.SoftHitUntilScore(24u, 3.0)
        Strategy.ChasesFlip7(20u, 5u)
        Strategy.EmboldenedBySecondChance 22u
        Strategy.HitWhileBehindLeader 12u
        Strategy.MaximizesExpectedValue
    ]

    let naiveNames = botNames |> List.filter (fun name -> name <> sageName)

    let naive =
        pool |> List.sortBy (fun _ -> random.Next()) |> List.take naiveNames.Length

    // Bots aim their action cards at random; the humans are asked. Sage
    // decides its hits and stands by Monte Carlo best response against its
    // fitted models of everyone else
    let players =
        (humanNames
         |> List.map (fun name -> name, Strategy.Custom terminalPrompt, Targeting.ChoosesExternally terminalPrompt))
        @ (botNames
           |> List.filter (fun name -> name = sageName)
           |> List.map (fun name -> name, Strategy.Custom adaptive, Targeting.ChoosesRandomly))
        @ (List.zip naiveNames naive
           |> List.map (fun (name, strategy) -> name, strategy, Targeting.ChoosesRandomly))
        |> List.sortBy (fun _ -> random.Next())

    let now = DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss"
    let directory = Path.Join(timelinesRoot, now)
    use store =
        new Persistence.TimelineStore(
            directory,
            ingestDelayMilliseconds = int64 paceMs,
            emitPlayersInSeatedOrder = true
        )

    // Instants written so far: the driver advances it, and the decider
    // stamps each prompt with it so the viewer can catch up before asking
    let mutable written = 0

    // The one reply channel. The engine suspends at each prompt, so prompts
    // are strictly sequential and a single shared channel is safe by
    // construction; and channels never run continuations synchronously by
    // default, so answering a prompt from the runtime loop never re-enters
    // the engine inline
    let replies = Channel.CreateUnbounded<Decision>()

    let decider (dispatch: Msg -> unit) : Strategy.Decider = {
        Target =
            fun targeting ask chooser candidates finished decks ->
                match targeting with
                | Targeting.ChoosesExternally name when name = terminalPrompt -> async {
                    let deck, discards = decks

                    dispatch (
                        Aiming {
                            At = written
                            Ask = ask
                            Chooser = chooser
                            Candidates = candidates
                            Deck = deck
                            Discards = discards
                        }
                    )

                    match! replies.Reader.ReadAsync().AsTask() |> Async.AwaitTask with
                    | Aimed player -> return player
                    | Quitting -> return raise (QuitException())
                    // The engine is suspended at this one ask, and the picker
                    // is the only thing the keyboard can answer it with
                    | decision -> return raise (InvalidOperationException $"answered a target ask with {decision}")
                  }
                | targeting -> Strategy.DecideTargetWith random targeting ask chooser candidates finished decks
        HitOrStand =
            fun strategy round turn player others finished decks ->
                match strategy with
                | Strategy.Custom name when name = terminalPrompt -> async {
                    let deck, discards = decks

                    dispatch (
                        Prompted {
                            Round = round
                            Turn = turn
                            At = written
                            Player = player
                            Others = others
                            Finished = finished
                            Deck = deck
                            Discards = discards
                        }
                    )

                    match! replies.Reader.ReadAsync().AsTask() |> Async.AwaitTask with
                    | Answered decision -> return decision
                    | Forked splice -> return raise (Editor.EditException splice)
                    | Quitting -> return raise (QuitException())
                    | decision ->
                        return raise (InvalidOperationException $"answered a hit-or-stand ask with {decision}")
                  }
                | Strategy.Custom name when name = adaptive ->
                    // The engine awaits the rollouts like any other decider;
                    // the viewer keeps showing the last frame
                    (Option.get sage).Decide random round turn player others finished decks
                | strategy -> Strategy.DecideHitOrStandWith random strategy round turn player others finished decks
    }

    // The driver: drains the engine to disk until the game ends on its own. A
    // quit or an edit unwinds the paused engine with an exception, which async
    // propagates here with its original type - an edit continues the timeline
    // in the same directory, right behind the instants already written
    let drive (dispatch: Msg -> unit) : Task<unit> =
        let decide = decider dispatch

        // The game so far, mirroring what is on disk: an edit continues right
        // behind the instants already written, so the record is append-only.
        // Sage refits its models of everyone at every round boundary
        let recorded = ResizeArray<Instant>()

        let record (instant: Instant) : unit =
            recorded.Add instant
            written <- written + 1

            match sage, instant.Event with
            | Some ai, RoundEnded _ -> ai.Learn(List.ofSeq recorded)
            | _ -> ()

        let rec drain (timeline: Timeline) : Async<unit> = async {
            try
                do!
                    timeline
                    |> Persistence.WriteTimelineLazyFrom directory written
                    |> AsyncSeq.iter record
            with
            | :? QuitException -> dispatch Quitted
            | :? Editor.EditException as edit ->
                let fork = Editor.Fork random decide edit.Splice
                return! drain fork
        }

        (random, decide, players)
        |||> Timeline.SimulateWithDecider
        |> drain
        |> Async.StartAsTask

    let execute (dispatch: Msg -> unit) : Effect -> unit =
        fun effect ->
            match effect with
            | Resolve decision -> replies.Writer.TryWrite decision |> ignore
            | RefreshSnapshot ->
                async {
                    let! snapshot = store.Snapshot
                    dispatch (Refreshed snapshot)
                }
                |> Async.Start
            | ReadFrame index ->
                async {
                    let! result = store.Read index
                    dispatch (Loaded(index, result))
                }
                |> Async.Start

    let! driver =
        Mvu.run {
            Init = init
            Update = update
            Effects = effects
            Execute = execute
            Subscribe = drive
            ViewKey = viewKey
            View = view directory
            Quit = fun model -> model.Quit
            Key = Pressed
            Tick = Tick
        }

    do! driver |> Async.AwaitTask

    // What Sage made of the humans, once the terminal is back
    match sage with
    | None -> ()
    | Some ai ->
        for name in humanNames do
            match ai.ModelOf name with
            | Some model ->
                let strategy, probability = List.head model.Posterior

                printfn
                    $"Sage read {name} as {strategy} (%.0f{probability * 100.0}%% sure, {model.Observations} decisions)"
            | None -> printfn $"Sage never saw {name} decide"
}
