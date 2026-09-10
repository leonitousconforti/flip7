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

type private QuitException() =
    inherit Exception()

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

type private Decision =
    | Answered of Strategy.HitOrStand
    | Forked of Editor.Splice
    | Quitting

type private Model = {
    // The open ask, held here from the decider's dispatch until answered
    Prompt: Prompt option
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

    Editor.Make seating (prompt.Player :: prompt.Others) prompt.Finished prompt.Deck prompt.Discards

let private update (strategyOf: Map<string, Strategy>) (msg: Msg) (model: Model) : Model =
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
    | Prompted prompt -> { model with Prompt = Some prompt; Resolved = None }

    | Quitted -> { model with Quit = true }

    | Pressed key ->
        match model.Editor, asking model with
        | Some editor, Some prompt ->
            match Editor.Key key editor with
            | Some editor' -> { model with Editor = Some editor' }
            | None when editor.Active |> List.exists (fun player -> Hand.IsBust player.Hand) ->
                // Applying is refused while an active player is busted; the
                // editor's footer already says so
                model
            | None ->
                let initial = editorFor model prompt

                if { editor with Cursor = initial.Cursor; Help = initial.Help } = initial then
                    // Nothing changed: back to the table and keep asking
                    { model with Editor = None }
                else
                    {
                        model with
                            Editor = None
                            Prompt = None
                            Resolved = Some(Forked(Editor.SpliceOf prompt.Round prompt.Turn editor))
                    }

        | _, Some prompt ->
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

        | _ when gameOver model -> { model with Quit = true }
        | _ -> model

// A read starts exactly when Reading changes to a new index, a refresh
// starts exactly when Refreshing turns on, and a prompt is answered exactly
// when Resolved changes to a new reply
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

    model.Editor, asking model, gameOver model, model.Frame, Persistence.TimelineStore.RoundOf roundEnds model.Cursor

let private promptFooter (prompt: Prompt) : string =
    let bust =
        Simulation.probabilityToBust prompt.Deck prompt.Discards prompt.Player.Hand (List.isEmpty prompt.Others)
        * 100.0

    let ev =
        Simulation.expectedValueOfHit prompt.Deck prompt.Discards prompt.Player.Hand

    $"{prompt.Player.Name}: %d{Hand.Score prompt.Player.Hand}pts in hand, %.0f{bust}%% bust, EV %+.1f{ev}   [h]it   [s]tand   [e]dit   [q]uit"
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

            let footer =
                match asking model with
                | Some prompt -> promptFooter prompt
                | None when gameOver model ->
                    let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)

                    $"game over: {winner.Name} wins with {winner.FirmScore}pts!   [any key]"
                    |> centered width
                    |> styled [ Ansi.BrightGreen ]
                | None -> "[q at your turn] quit" |> centered width |> styled [ Ansi.Dim; Ansi.Cyan ]

            RenderPlay round instant footer

let public Run (humanNames: string list) (seed: int option) (pace: int option) : Async<unit> = async {
    if humanNames.Length < 1 || humanNames.Length > 5 then
        raise (ArgumentException "Please provide one to five player names as command-line arguments.")

    if humanNames |> List.exists String.IsNullOrWhiteSpace then
        raise (ArgumentException "Player names must not be empty.")

    if humanNames |> List.distinct |> List.length <> humanNames.Length then
        raise (ArgumentException "Player names must be unique.")

    let botNames =
        [ "Alice"; "Bob"; "Chloe"; "Dave"; "Eve" ]
        |> List.take (playerSlots - humanNames.Length)

    match humanNames |> List.tryFind (fun name -> botNames |> List.contains name) with
    | Some taken -> raise (ArgumentException $"{taken} is taken by one of the AIs, please pick another name.")
    | None -> ()

    let random =
        match seed with
        | Some value -> Random(value)
        | None -> Random()

    let paceMs = defaultArg pace paceMilliseconds
    let pool = [
        HitUntilScore 22u
        HitUntilScore 26u
        HitUntilNumCards 5u
        HitUntilBustProbability 0.35
        HitUntilNaiveBustProbability 0.4
        SoftHitUntilScore(24u, 3.0)
        ChasesFlip7(20u, 5u)
        EmboldenedBySecondChance 22u
        HitWhileBehindLeader 12u
        MaximizesExpectedValue
    ]

    let naive =
        pool |> List.sortBy (fun _ -> random.Next()) |> List.take botNames.Length

    let players =
        (humanNames |> List.map (fun name -> name, Custom terminalPrompt))
        @ List.zip botNames naive
        |> List.sortBy (fun _ -> random.Next())

    let directory = Path.Join("timelines", DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss")
    let strategyOf = players |> Map.ofList

    // The store meters ingestion to the pace, so the viewer side needs no
    // pacing of its own
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

    let decider (dispatch: Msg -> unit) : Strategy.Decider =
        fun strategy round turn player others finished decks ->
            match strategy with
            | Custom name when name = terminalPrompt -> async {
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
              }
            | strategy -> Strategy.DecideWith random strategy round turn player others finished decks

    // The driver: drains the engine to disk until the game ends on its own.
    // A quit or an edit unwinds the paused engine with an exception, which
    // async propagates here with its original type - an edit continues the
    // timeline in the same directory, right behind the instants already
    // written
    let drive (dispatch: Msg -> unit) : Task<unit> =
        let decide = decider dispatch

        let rec drain (timeline: Timeline) : Async<unit> = async {
            try
                do!
                    timeline
                    |> Persistence.WriteTimelineLazyFrom directory written
                    |> AsyncSeq.iter (fun _ -> written <- written + 1)
            with
            | :? QuitException -> dispatch Quitted
            | :? Editor.EditException as edit -> return! drain (Editor.Fork random decide edit.Splice)
        }

        Async.StartAsTask(Timeline.SimulateWithDecider random decide players |> drain)

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
            Update = update strategyOf
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
}
