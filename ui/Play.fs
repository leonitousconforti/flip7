module public Play

open System
open System.IO
open System.Threading.Tasks

open FSharp.Control

open Flip7

let private width = 80
let private playerSlots = 5
let private paceMilliseconds = 400

// The play screen is Simulate's architecture with a human decider: a driver
// task drains the engine to disk, and the screen is a TimelineStore viewer
// whose cursor chases the newest instant - the disk is the only channel
// between them. The engine pauses inside the decider awaiting a reply, and
// the exceptions that unwind it on a quit or an edit live entirely inside
// the driver: a task CE rethrows the original exception, so the catches are
// plain and nothing exception-shaped ever reaches the model.

type private QuitException() =
    inherit Exception()

// A mid-round state to fork the game from after an edit: the active players
// in rotation order (the editing player at the head, about to be re-asked at
// Turn), the finished players, and the amended decks
type private Splice = {
    Round: uint
    Turn: uint
    Active: Player list
    Finished: Player list
    Deck: Deck
    Discards: Deck
}

type private EditException(splice: Splice) =
    inherit Exception()
    member _.Splice = splice

// What a human is being asked, and where to send the answer. At is how many
// instants precede the ask: the prompt is held back until the cursor has
// caught up to them, so a decision is never asked for before the plays
// leading up to it have been shown.
type private Prompt = {
    Round: uint
    Turn: uint
    At: int
    Player: Strategy.StrategyPlayer
    Others: Strategy.StrategyPlayer list
    Finished: Strategy.StrategyPlayer list
    Deck: Deck
    Discards: Deck
    Reply: TaskCompletionSource<Decision>
}

and private Decision =
    | Answered of Strategy.HitOrStand
    | Forked of Splice
    | Quitting

type private Phase =
    | Watching
    | Prompting of Prompt
    | Editing of Prompt * Editor.Model
    | GameOver

type private Model = {
    Phase: Phase
    // A prompt waiting for the cursor to catch up to it
    Pending: Prompt option
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
    // Extra ticks to linger on the shown frame (a round end dwells longer)
    Cooldown: int
    // Whether the engine driver is running: turning this on is what starts it
    Started: bool
    // The answer to the current prompt: declaring it here is what sends it
    Resolved: (TaskCompletionSource<Decision> * Decision) option
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
    | StartEngine
    | RefreshSnapshot
    | ReadFrame of index: int
    | Resolve of TaskCompletionSource<Decision> * Decision

let private init: Model = {
    Phase = Watching
    Pending = None
    Cursor = 0
    Snapshot = 0, false, []
    Frame = None
    Reading = None
    Refreshing = false
    Cooldown = 0
    Started = false
    Resolved = None
    Quit = false
}

let private editorFor (prompt: Prompt) : Editor.Model =
    Editor.Make (prompt.Player :: prompt.Others) prompt.Finished prompt.Deck prompt.Discards

let private spliceOf (strategyOf: Map<string, Strategy>) (prompt: Prompt) (edited: Editor.Model) : Splice =
    let toPlayer (player: Strategy.StrategyPlayer) =
        Player.Make(player.Name, strategyOf[player.Name], player.FirmScore, player.Hand)

    {
        Round = prompt.Round
        Turn = prompt.Turn
        Active = edited.Active |> List.map toPlayer
        Finished = edited.Finished |> List.map toPlayer
        Deck = edited.Deck
        Discards = edited.Discards
    }

let private update (strategyOf: Map<string, Strategy>) (paceTicks: int) (msg: Msg) (model: Model) : Model =
    match msg with
    | Tick ->
        let count, isComplete, _ = model.Snapshot
        let newest = count - 1
        let model = { model with Refreshing = true; Started = true }

        match model.Phase with
        | Watching when model.Reading.IsNone ->
            if model.Cooldown > 0 then
                { model with Cooldown = model.Cooldown - 1 }
            elif model.Cursor < newest then
                // The store meters ingestion to the pace, so chasing the
                // newest instant is what unfolds the game at watchable speed
                { model with Cursor = model.Cursor + 1; Reading = Some(model.Cursor + 1) }
            else
                match model.Pending with
                | Some prompt when model.Cursor = prompt.At - 1 && model.Frame.IsSome ->
                    { model with Phase = Prompting prompt; Pending = None }
                | _ when isComplete && count > 0 && model.Cursor = newest && model.Frame.IsSome ->
                    { model with Phase = GameOver }
                | _ -> model
        | _ -> model

    | Refreshed((count', _, _) as snapshot) ->
        let model' = { model with Snapshot = snapshot; Refreshing = false }

        if count' > 0 && model.Frame.IsNone && model.Reading.IsNone then
            // The first instants arrived: fetch the cursor's frame
            { model' with Reading = Some model.Cursor }
        else
            model'

    | Loaded(index, result) ->
        if index = model.Cursor then
            // A round end dwells an extra pace before play moves on
            let linger =
                match result with
                | Ok instant when instant.Event.IsRoundEnded -> paceTicks
                | _ -> 0

            { model with Reading = None; Frame = Some result; Cooldown = linger }
        else
            { model with Reading = Some model.Cursor }

    | Prompted prompt -> { model with Pending = Some prompt; Resolved = None }

    | Quitted -> { model with Quit = true }

    | Pressed key ->
        match model.Phase with
        | Watching -> model
        | GameOver -> { model with Quit = true }

        | Prompting prompt ->
            match key.Key with
            | ConsoleKey.H -> {
                model with
                    Phase = Watching
                    Resolved = Some(prompt.Reply, Answered Strategy.Hit)
              }
            | ConsoleKey.S -> {
                model with
                    Phase = Watching
                    Resolved = Some(prompt.Reply, Answered Strategy.Stand)
              }
            | ConsoleKey.Q
            | ConsoleKey.Escape -> {
                model with
                    Phase = Watching
                    Resolved = Some(prompt.Reply, Quitting)
              }
            | ConsoleKey.E -> { model with Phase = Editing(prompt, editorFor prompt) }
            | _ -> model

        | Editing(prompt, editor) ->
            match Editor.Key key editor with
            | Some editor' -> { model with Phase = Editing(prompt, editor') }
            | None when Editor.BustedActives editor |> List.isEmpty |> not ->
                // Applying is refused while an active player is busted; the
                // editor's footer already says so
                model
            | None ->
                let initial = editorFor prompt

                if { editor with Cursor = initial.Cursor; Help = initial.Help } = initial then
                    // Nothing changed: back to the table and keep asking
                    { model with Phase = Prompting prompt }
                else
                    {
                        model with
                            Phase = Watching
                            Resolved = Some(prompt.Reply, Forked(spliceOf strategyOf prompt editor))
                    }

// The engine starts exactly when Started turns on, a read starts exactly
// when Reading changes to a new index, a refresh starts exactly when
// Refreshing turns on, and a prompt is answered exactly when Resolved
// changes to a new reply
let private effects (before: Model) (after: Model) : Effect list = [
    if after.Started && not before.Started then
        StartEngine

    if after.Refreshing && not before.Refreshing then
        RefreshSnapshot

    if after.Reading <> before.Reading then
        match after.Reading with
        | Some index -> ReadFrame index
        | None -> ()

    if after.Resolved <> before.Resolved then
        match after.Resolved with
        | Some(reply, decision) -> Resolve(reply, decision)
        | None -> ()
]

let private viewKey (model: Model) =
    let _, _, roundEnds = model.Snapshot
    model.Phase, model.Frame, Persistence.TimelineStore.RoundOf roundEnds model.Cursor

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
    match model.Phase with
    | Editing(_, editor) when editor.Help -> Editor.RenderHelp()
    | Editing(_, editor) -> Editor.Render editor
    | _ ->
        match model.Frame with
        | None -> ()
        | Some(Error error) -> RenderError directory model.Snapshot model.Cursor error
        | Some(Ok instant) ->
            let _, _, roundEnds = model.Snapshot
            let round = Persistence.TimelineStore.RoundOf roundEnds model.Cursor

            let footer =
                match model.Phase with
                | Prompting prompt -> promptFooter prompt
                | GameOver ->
                    let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)

                    $"game over: {winner.Name} wins with {winner.FirmScore}pts!   [any key]"
                    |> centered width
                    |> styled [ Ansi.BrightGreen ]
                | _ -> "[q at your turn] quit" |> centered width |> styled [ Ansi.Dim; Ansi.Cyan ]

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
    let paceTicks = paceMs / Mvu.pollMilliseconds
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
        (humanNames |> List.map (fun name -> name, Custom "TerminalPrompt"))
        @ List.zip botNames naive
        |> List.sortBy (fun _ -> random.Next())

    let directory = Path.Join("timelines", DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss")
    let strategyOf = players |> Map.ofList

    // The store meters ingestion to the pace, so the viewer side needs no
    // pacing of its own beyond the round-end linger
    use store =
        new Persistence.TimelineStore(directory, ingestDelayMilliseconds = int64 paceMs)

    // Instants written so far: the driver advances it, and the decider
    // stamps each prompt with it so the viewer can catch up before asking
    let mutable written = 0

    let decider (dispatch: Msg -> unit) : Strategy.Decider =
        fun strategy round turn player others finished decks ->
            match strategy with
            | Custom name when name = "TerminalPrompt" -> async {
                let deck, discards = decks

                // Completions run asynchronously so answering a prompt
                // from the runtime loop never re-enters the engine inline
                let reply =
                    TaskCompletionSource<Decision>(TaskCreationOptions.RunContinuationsAsynchronously)

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
                        Reply = reply
                    }
                )

                match! reply.Task |> Async.AwaitTask with
                | Answered decision -> return decision
                | Forked splice -> return raise (EditException splice)
                | Quitting -> return raise (QuitException())
              }
            | strategy -> Strategy.DecideWith random strategy round turn player others finished decks

    // A fork after an edit: an Edited instant records the amended table, and
    // the game continues from exactly where it stood. The editing player sits
    // at the head of the rotation about to be re-asked at the same turn, so
    // everyone active is counted one turn behind them.
    let spliced (decide: Strategy.Decider) (splice: Splice) : Timeline =
        let turnsTaken =
            splice.Active
            |> List.map (fun player -> player.Name, splice.Turn - 1u)
            |> Map.ofList

        asyncSeq {
            yield {
                Event = Edited (List.head splice.Active).Name
                Players = splice.Active @ splice.Finished
                Deck = splice.Deck
                Discards = splice.Discards
            }

            yield!
                Timeline.ContinueWith
                    random
                    decide
                    splice.Round
                    turnsTaken
                    splice.Active
                    splice.Finished
                    (splice.Deck, splice.Discards)
        }

    // The driver: drains the engine to disk until the game ends on its own.
    // A quit or an edit unwinds the paused engine with an exception, which
    // arrives here with its original type - an edit continues the timeline
    // in the same directory, right behind the instants already written
    let drive (dispatch: Msg -> unit) : Task<unit> =
        let decide = decider dispatch

        task {
            let mutable timeline = Some(Timeline.SimulateWithDecider random decide players)

            while timeline.IsSome do
                let enumerator =
                    (timeline.Value |> Persistence.WriteTimelineLazyFrom directory written)
                        .GetAsyncEnumerator()

                try
                    let mutable pulled = true

                    while pulled do
                        let! more = enumerator.MoveNextAsync()

                        if more then
                            written <- written + 1

                        pulled <- more

                    timeline <- None
                with
                | :? QuitException ->
                    dispatch Quitted
                    timeline <- None
                | :? EditException as edit -> timeline <- Some(spliced decide edit.Splice)

                do! enumerator.DisposeAsync()
        }

    let mutable driver: Task<unit> option = None

    let execute (dispatch: Msg -> unit) : Effect -> unit =
        fun effect ->
            match effect with
            | StartEngine -> driver <- Some(drive dispatch)
            | Resolve(reply, decision) -> reply.SetResult decision
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

    do!
        Mvu.run {
            Init = init
            Update = update strategyOf paceTicks
            Effects = effects
            Execute = execute
            ViewKey = viewKey
            View = view directory
            Quit = fun model -> model.Quit
            Key = Pressed
            Tick = Tick
        }

    match driver with
    | Some task -> do! task |> Async.AwaitTask
    | None -> ()
}
