module public Replay

open System

open Flip7

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
    Quit: bool
} with
    // Nothing in flight and nothing on screen; the first heartbeat requests the
    // first snapshot through the same path as every later one
    static member Init: Model = {
        Cursor = 0
        Snapshot = 0, false, []
        Frame = None
        Reading = None
        Refreshing = false
        Quit = false
    }

type private Msg =
    | Pressed of ConsoleKey
    | Refreshed of (int * bool * int list)
    | Loaded of index: int * result: Result<Instant, exn>
    | Tick

type private Effect =
    | RefreshSnapshot
    | ReadFrame of index: int

let private moveCursor (roundEnds: int list) (newest: int) (cursor: int) (key: ConsoleKey) : int option =
    match key with
    | ConsoleKey.Q -> None
    | ConsoleKey.Escape -> None
    | ConsoleKey.LeftArrow -> Some(max 0 (cursor - 1))
    | ConsoleKey.RightArrow -> Some(min newest (cursor + 1))
    | ConsoleKey.UpArrow -> Some(Persistence.TimelineStore.NextRoundEnded roundEnds newest cursor)
    | ConsoleKey.PageUp -> Some(Persistence.TimelineStore.NextRoundEnded roundEnds newest cursor)
    | ConsoleKey.DownArrow -> Some(Persistence.TimelineStore.PrevRoundEnded roundEnds cursor)
    | ConsoleKey.PageDown -> Some(Persistence.TimelineStore.PrevRoundEnded roundEnds cursor)
    | ConsoleKey.End -> Some newest
    | ConsoleKey.Home -> Some 0
    | _ -> Some cursor

let private update (msg: Msg) (model: Model) : Model =
    let count, _, roundEnds = model.Snapshot

    match msg with
    | Pressed key ->
        match moveCursor roundEnds (count - 1) model.Cursor key with
        | None -> { model with Quit = true }
        | Some cursor ->
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
            { model' with Reading = Some model.Cursor }
        else
            model'

    | Loaded(index, result) ->
        if index = model.Cursor then
            { model with Reading = None; Frame = Some result }
        else
            { model with Reading = Some model.Cursor }

    | Tick -> { model with Refreshing = true }

let private effects (before: Model) (after: Model) : Effect list = [
    if after.Reading <> before.Reading then
        match after.Reading with
        | Some index -> ReadFrame index
        | None -> ()

    if after.Refreshing && not before.Refreshing then
        RefreshSnapshot
]

let private spin () : int =
    int (Environment.TickCount64 / int64 Mvu.pollMilliseconds)

let private viewKey (model: Model) =
    let count, _, _ = model.Snapshot
    let loading = count > 0 && model.Frame.IsNone
    model.Cursor, model.Snapshot, model.Frame, (if loading then spin () else 0)

let private view (source: string) (model: Model) : unit =
    let count, _, _ = model.Snapshot

    if count > 0 then
        match model.Frame with
        | Some(Ok instant) -> RenderTable source model.Snapshot model.Cursor instant
        | Some(Error error) -> RenderError source model.Snapshot model.Cursor error
        | None -> RenderLoading source model.Snapshot model.Cursor (spin ())

let public Run (source: string) (directory: string) (pace: int option) (cacheCapacity: int option) : Async<unit> = async {
    let ingestDelayMilliseconds = pace |> Option.map int64

    use store =
        new Persistence.TimelineStore(
            directory,
            ?ingestDelayMilliseconds = ingestDelayMilliseconds,
            ?cacheCapacity = cacheCapacity
        )

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
            Init = Model.Init
            Update = update
            Effects = effects
            Execute = execute
            Subscribe = ignore
            ViewKey = viewKey
            View = view source
            Quit = fun model -> model.Quit
            Key = fun key -> Pressed key.Key
            Tick = Tick
        }
}
