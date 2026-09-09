module public Replay

open System

open Flip7

// Maps a key to the cursor position it moves to, or None to quit. Pure, so
// the navigation rules can be exercised on their own.
let private Move (roundEnds: int list) (newest: int) (cursor: int) (key: ConsoleKey) : int option =
    match key with
    | ConsoleKey.Q
    | ConsoleKey.Escape -> None
    | ConsoleKey.LeftArrow -> Some(max 0 (cursor - 1))
    | ConsoleKey.RightArrow -> Some(min newest (cursor + 1))
    | ConsoleKey.UpArrow
    | ConsoleKey.PageUp -> Some(Persistence.TimelineStore.NextRoundEnded roundEnds newest cursor)
    | ConsoleKey.DownArrow
    | ConsoleKey.PageDown -> Some(Persistence.TimelineStore.PrevRoundEnded roundEnds cursor)
    | ConsoleKey.End -> Some newest
    | ConsoleKey.Home -> Some 0
    | _ -> Some cursor

// The viewer is model-view-update: every occurrence - a key, a snapshot
// round trip landing, a frame read landing, a heartbeat - arrives as a
// message, update folds it into the model, and the screen is a function of
// the model. Effects are not returned by update; they are derived from how
// the model changed (see effects below), so a request and the bookkeeping
// that tracks it are one fact that cannot drift apart. The runtime at the
// bottom owns all of the impurity.

type private Model = {
    Cursor: int
    Snapshot: int * bool * int list
    // The cursor frame: None renders as loading until its read lands
    Frame: Result<Instant, exn> option
    // The index of the one in-flight frame read: declaring it here is what
    // requests it. At most one read is ever outstanding, so scrubbing through
    // frames never piles abandoned work onto the store - skipped frames are
    // simply never requested.
    Reading: int option
    // Whether a snapshot refresh is in flight: turning this on is what
    // requests one. Exactly one stays outstanding, re-requested by the
    // heartbeat, so a stalled store leaves the UI live on the last-known
    // values instead of locked up.
    Refreshing: bool
    Spin: int
    Quit: bool
}

type private Msg =
    | Pressed of ConsoleKey
    | Refreshed of (int * bool * int list)
    | Loaded of index: int * result: Result<Instant, exn>
    | Tick

type private Effect =
    | RefreshSnapshot
    | ReadFrame of index: int

// Nothing in flight and nothing on screen; the first heartbeat requests the
// first snapshot through the same path as every later one
let private init: Model = {
    Cursor = 0
    Snapshot = 0, false, []
    Frame = None
    Reading = None
    Refreshing = false
    Spin = 0
    Quit = false
}

let private update (msg: Msg) (model: Model) : Model =
    let count, _, roundEnds = model.Snapshot

    match msg with
    | Pressed key ->
        match Move roundEnds (count - 1) model.Cursor key with
        | None -> { model with Quit = true }
        | Some cursor ->
            // An empty timeline clamps every move back to the start
            let cursor = max 0 cursor

            if cursor = model.Cursor then
                model
            else
                match model.Reading with
                | None -> {
                    model with
                        Cursor = cursor
                        Frame = None
                        Reading = Some cursor
                  }
                | Some _ -> { model with Cursor = cursor; Frame = None }

    | Refreshed((count', isComplete', _) as snapshot) ->
        let model' = { model with Snapshot = snapshot; Refreshing = false }

        if count' = 0 && isComplete' then
            { model' with Quit = true }
        elif count' > 0 && model.Frame.IsNone && model.Reading.IsNone then
            // The first instants arrived: fetch the cursor's frame
            { model' with Reading = Some model.Cursor }
        else
            model'

    | Loaded(index, result) ->
        if index = model.Cursor then
            { model with Reading = None; Frame = Some result }
        else
            // A read for a frame the cursor has since left: its result is
            // already in the store's cache, so fetch the frame for wherever
            // the cursor is now
            { model with Reading = Some model.Cursor }

    | Tick ->
        let spin =
            if count > 0 && model.Frame.IsNone then
                model.Spin + 1
            else
                model.Spin

        { model with Spin = spin; Refreshing = true }

// The one place where in-flight bookkeeping becomes work: a read starts
// exactly when Reading changes to a new index, and a refresh starts exactly
// when Refreshing turns on
let private effects (before: Model) (after: Model) : Effect list = [
    if after.Reading <> before.Reading then
        match after.Reading with
        | Some index -> ReadFrame index
        | None -> ()

    if after.Refreshing && not before.Refreshing then
        RefreshSnapshot
]

// What the screen is a function of: the runtime renders only when this
// changes, so bookkeeping like Refreshing never causes a redraw
let private viewKey (model: Model) =
    model.Cursor, model.Snapshot, model.Frame, model.Spin

let private view (source: string) (model: Model) : unit =
    let count, _, _ = model.Snapshot

    if count > 0 then
        match model.Frame with
        | Some(Ok instant) -> RenderTable source model.Snapshot model.Cursor instant
        | Some(Error error) -> RenderError source model.Snapshot model.Cursor error
        | None -> RenderLoading source model.Snapshot model.Cursor model.Spin

let public Run (source: string) (directory: string) (pace: int option) : Async<unit> = async {
    use store =
        new Persistence.TimelineStore(directory, ?ingestDelayMilliseconds = (pace |> Option.map int64))

    let execute (dispatch: Msg -> unit) (effect: Effect) : unit =
        match effect with
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
            Update = update
            Effects = effects
            Execute = execute
            ViewKey = viewKey
            View = view source
            Quit = fun model -> model.Quit
            Key = fun key -> Pressed key.Key
            Tick = Tick
        }
}
