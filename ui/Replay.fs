module public Replay

open System

open Flip7

let private width = 80
let private playerSlots = 5
let private barWidth = width - 2

let private CaptionStyle (event: Event) : string list =
    match event with
    | Busted _ -> [ Ansi.BrightRed ]
    | Flip7Achieved _ -> [ Ansi.BrightMagenta ]
    | RoundEnded _ -> [ Ansi.BrightYellow ]
    | Froze _ -> [ Ansi.BrightCyan ]
    | _ -> []

let private padded (s: string) : string =
    s + String.replicate (max 0 (width - visualLength s)) " "

let private ProgressBar (timeline: AnnotatedInstant array) (cursor: int) : string =
    let cellOf index = index * barWidth / timeline.Length
    let cells = Array.create barWidth "─"

    timeline
    |> Array.iteri (fun index instant ->
        if instant.Instant.Event.IsRoundEnded then
            cells[cellOf index] <- "┊"
    )

    cells[cellOf cursor] <- styled [ Ansi.BrightGreen ] "●"
    "├" + String.concat "" cells + "┤"

let private Render (source: string) (timeline: AnnotatedInstant array) (cursor: int) : unit =
    let round = timeline[cursor].Round
    let instant = timeline[cursor].Instant
    let lastInstant = Array.last timeline
    let actor = instant.Event.Actor()
    let totalRounds = lastInstant.Round
    let rule = String.replicate width "─"

    let caption, captionStyle =
        if cursor = timeline.Length - 1 then
            let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)
            $"game over: {winner.Name} wins with {winner.FirmScore}pts!", [ Ansi.BrightGreen ]
        else
            string instant.Event, CaptionStyle instant.Event

    let status =
        let left = $"replay: {source}"
        let right = $"round {round}/{totalRounds}   instant {cursor + 1}/{timeline.Length}"
        let leftWidth = visualLength left
        let rightWidth = visualLength right
        let middle = String.replicate (max 0 (width - leftWidth - rightWidth)) " "
        left + middle + right

    let footer =
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
    printfn "%s" (padded (ProgressBar timeline cursor))
    printf "%s" (padded footer)

let public Run (source: string) (maybeTimeline: Instant array option) : unit =
    let loadTimeline = fun () -> source |> Persistence.ReadTimeline |> Seq.toArray
    let timeline = Option.defaultWith loadTimeline maybeTimeline
    let annotated = Timeline.Link timeline

    if Array.isEmpty timeline then
        ()
    else

    let rec loop (cursor: int) : unit =
        Render source annotated cursor
        match (Console.ReadKey true).Key with
        | ConsoleKey.Q
        | ConsoleKey.Escape -> ()
        | ConsoleKey.LeftArrow -> loop (max 0 (cursor - 1))
        | ConsoleKey.RightArrow -> loop (min (timeline.Length - 1) (cursor + 1))
        | ConsoleKey.UpArrow -> loop (annotated[cursor].ForwardsRoundEventIndex)
        | ConsoleKey.PageUp -> loop (annotated[cursor].ForwardsRoundEventIndex)
        | ConsoleKey.DownArrow -> loop (annotated[cursor].BackwardsRoundEventIndex)
        | ConsoleKey.PageDown -> loop (annotated[cursor].BackwardsRoundEventIndex)
        | ConsoleKey.End -> loop (timeline.Length - 1)
        | ConsoleKey.Home -> loop 0
        | _ -> loop cursor

    loop 0
