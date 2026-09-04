module public Replay

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading

open Flip7

let private width = 80
let private playerSlots = 5
let private barWidth = width - 2
let private cacheCapacity = 64
let private pollMilliseconds = 30
let private ingestDelayMilliseconds = 100L

// The round number (starting at 1) of the instant at the cursor, given the
// ascending indices of the RoundEnded instants seen so far. A RoundEnded
// instant belongs to the round it closes.
let private RoundOf (roundEnds: ResizeArray<int>) (cursor: int) : int =
    1 + (roundEnds |> Seq.filter (fun index -> index < cursor) |> Seq.length)

// The index of the nearest RoundEnded instant strictly after the cursor, or
// the newest known index when that round has not ended yet
let private NextRoundEnded (roundEnds: ResizeArray<int>) (newest: int) (cursor: int) : int =
    roundEnds
    |> Seq.tryFind (fun index -> index > cursor)
    |> Option.defaultValue newest

// The index of the nearest RoundEnded instant strictly before the cursor, or
// the start of the timeline when there is none
let private PrevRoundEnded (roundEnds: ResizeArray<int>) (cursor: int) : int =
    roundEnds
    |> Seq.tryFindBack (fun index -> index < cursor)
    |> Option.defaultValue 0

/// <summary>
/// A view of a timeline directory as it fills: instants are published
/// atomically (staged then renamed by the writer), so once {directory}/{index}
/// exists its contents are complete and the store can tail the directory for
/// growth. Only lightweight metadata is held in memory plus a small cache of
/// recently viewed instants: memory stays flat no matter how long the timeline
/// grows, and there is no channel to the producer at all - the disk is the
/// only source.
/// </summary>
type private TimelineStore(directory: string) =
    let ingestPacer = Stopwatch.StartNew()
    let roundEnds = ResizeArray<int>()
    let cache = Dictionary<int, Instant>()
    let cacheOrder = Queue<int>()

    let mutable count: int = 0
    let mutable isComplete: bool = false
    let mutable error: string option = None

    member _.Count = count
    member _.IsComplete = isComplete
    member _.Error = error
    member _.RoundEnds = roundEnds

    member _.Read(index: int) : Instant =
        match cache.TryGetValue index with
        | true, instant -> instant
        | false, _ ->
            let instant =
                Persistence.ReadInstantAsync(Path.Join(directory, $"{index}"))
                |> Async.RunSynchronously

            cache[index] <- instant
            cacheOrder.Enqueue index

            if cacheOrder.Count > cacheCapacity then
                cache.Remove(cacheOrder.Dequeue()) |> ignore

            instant

    member self.Ingest() : unit =
        try
            if
                ingestPacer.ElapsedMilliseconds >= ingestDelayMilliseconds
                && not isComplete
                && Directory.Exists(Path.Join(directory, $"{count}"))
            then
                let instant = self.Read count

                if instant.Event.IsRoundEnded then
                    roundEnds.Add count

                    if instant.Players |> List.exists (fun player -> player.FirmScore >= 200u) then
                        isComplete <- true

                count <- count + 1
                ingestPacer.Restart()
        with exn ->
            error <- Some exn.Message
            isComplete <- true

let private CaptionStyle (event: Event) : string list =
    match event with
    | Busted _ -> [ Ansi.BrightRed ]
    | Flip7Achieved _ -> [ Ansi.BrightMagenta ]
    | RoundEnded _ -> [ Ansi.BrightYellow ]
    | Froze _ -> [ Ansi.BrightCyan ]
    | _ -> []

let private padded (s: string) : string =
    s + String.replicate (max 0 (width - visualLength s)) " "

let private ProgressBar (store: TimelineStore) (cursor: int) : string =
    let cellOf index = index * barWidth / store.Count
    let cells = Array.create barWidth "─"

    for index in store.RoundEnds do
        cells[cellOf index] <- "┊"

    cells[cellOf cursor] <- styled [ Ansi.BrightGreen ] "●"
    "├" + String.concat "" cells + "┤"

let private Render (source: string) (store: TimelineStore) (cursor: int) (instant: Instant) : unit =
    let round = RoundOf store.RoundEnds cursor
    let knownRounds = RoundOf store.RoundEnds (store.Count - 1)
    let actor = instant.Event.Actor()
    let rule = String.replicate width "─"

    let growing = if store.IsComplete then "" else "+"
    let caption, captionStyle =
        if store.IsComplete && store.Error.IsNone && cursor = store.Count - 1 then
            let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)
            $"game over: {winner.Name} wins with {winner.FirmScore}pts!", [ Ansi.BrightGreen ]
        else
            string instant.Event, CaptionStyle instant.Event

    let status =
        let left = $"replay: {source}"
        let right =
            $"round {round}/{knownRounds}{growing}   instant {cursor + 1}/{store.Count}{growing}"
        let leftWidth = visualLength left
        let rightWidth = visualLength right
        let middle = String.replicate (max 0 (width - leftWidth - rightWidth)) " "
        left + middle + right

    let footer =
        match store.Error with
        | Some error -> error |> centered width |> styled [ Ansi.BrightRed ]
        | None ->
            "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
            |> centered width
            |> styled [ Ansi.Dim; Ansi.Cyan ]

    // Overwrite in place rather than clearing, so scrubbing does not flicker;
    // every line is padded to the full width to erase the previous frame
    Console.SetCursorPosition(0, 0)
    printfn "%s" (padded rule)
    printfn "%s" (padded status)
    printfn "%s" (padded (caption |> centered width |> styled captionStyle))
    printfn "%s" (padded rule)

    for player in instant.Players do
        let onlyPlayerNotBusted =
            instant.Players
            |> List.forall (fun p -> Hand.IsBust p.Hand || p.Name = player.Name)

        let probabilityToBust =
            Simulation.probabilityToBust instant.Deck instant.Discards player.Hand onlyPlayerNotBusted
            |> fun p -> p * 100.0

        let tentativeScore =
            if Hand.IsBust player.Hand then
                0u
            else
                Hand.Score player.Hand

        let preamble =
            sprintf
                "%s %s (%dpts + %dpts?, %.2f%%): "
                player.Name
                (bustEmoji probabilityToBust)
                player.FirmScore
                tentativeScore
                probabilityToBust

        handRows 40 preamble player.Hand
        |> fun (top, mid, bot) ->
            let isActor = actor = Some player.Name
            let styles = if isActor then [ Ansi.Inverse ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) ->
            let isBust = Hand.IsBust player.Hand
            let styles = if isBust then [ Ansi.Dim; Ansi.Italic ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) -> [ padded top; padded mid; padded bot ]
        |> String.concat "\n"
        |> printfn "%s"

    for _ in 1 .. (playerSlots - List.length instant.Players) * 3 do
        printfn "%s" (padded "")

    printfn "%s" (padded rule)
    printfn "%s" (padded (ProgressBar store cursor))
    printf "%s" (padded footer)

let public Run (source: string) (directory: string) : unit =
    let store = TimelineStore directory
    let mutable cursor = 0
    let mutable rendered = (-1, false, -1)
    let mutable quit = false

    // Poll rather than block on input, so the frame refreshes as new instants
    // appear even while no key is pressed
    while not quit do
        store.Ingest()

        if store.Count = 0 then
            if store.IsComplete then
                store.Error |> Option.iter (eprintfn "%s")
                quit <- true
            else
                Thread.Sleep pollMilliseconds
        else
            let newest = store.Count - 1

            // Drain every pending key before rendering, so a held-down arrow
            // coalesces into one redraw per tick
            while not quit && Console.KeyAvailable do
                match (Console.ReadKey true).Key with
                | ConsoleKey.Q
                | ConsoleKey.Escape -> quit <- true
                | ConsoleKey.LeftArrow -> cursor <- max 0 (cursor - 1)
                | ConsoleKey.RightArrow -> cursor <- min newest (cursor + 1)
                | ConsoleKey.UpArrow
                | ConsoleKey.PageUp -> cursor <- NextRoundEnded store.RoundEnds newest cursor
                | ConsoleKey.DownArrow
                | ConsoleKey.PageDown -> cursor <- PrevRoundEnded store.RoundEnds cursor
                | ConsoleKey.End -> cursor <- newest
                | ConsoleKey.Home -> cursor <- 0
                | _ -> ()

            if not quit then
                if (store.Count, store.IsComplete, cursor) <> rendered then
                    Render source store cursor (store.Read cursor)
                    rendered <- store.Count, store.IsComplete, cursor

                Thread.Sleep pollMilliseconds
