module public Replay

open System

open Flip7

let private width = 80
let private playerSlots = 5
let private barWidth = width - 2

let private CaptionStyle (event: Event) : string list =
    match event with
    | Busted _ -> [ Ansi.BrightRed ]
    | Flip7Achieved _
    | GameEnded _ -> [ Ansi.BrightGreen ]
    | RoundEnded _ -> [ Ansi.BrightYellow ]
    | Froze _ -> [ Ansi.BrightCyan ]
    | _ -> []

let private padded (s: string) : string =
    s + String.replicate (max 0 (width - visualLength s)) " "

let private ProgressBar (timeline: Instant array) (cursor: int) : string =
    let cellOf index = index * barWidth / timeline.Length
    let cells = Array.create barWidth "─"

    timeline
    |> Array.iteri (fun index instant ->
        if instant.Event.IsRoundEnded then
            cells[cellOf index] <- "┊"
    )

    cells[cellOf cursor] <- styled [ Ansi.BrightGreen ] "●"
    "├" + String.concat "" cells + "┤"

let private Render
    (source: string)
    (timeline: Instant array)
    (roundsBefore: int array)
    (totalRounds: int)
    (cursor: int)
    : unit =
    let instant = timeline[cursor]
    let rule = String.replicate width "─"
    let actor = instant.Event.Actor()

    let status =
        let left = $"replay: {source}"
        let right =
            $"round {roundsBefore[cursor] + 1}/{totalRounds}   instant {cursor + 1}/{timeline.Length}"
        left
        + String.replicate (max 1 (width - visualLength left - visualLength right)) " "
        + right

    let playerRows =
        instant.Players
        |> List.sortBy (fun player -> player.Name)
        |> List.collect (fun player ->
            let onlyPlayerNotBusted =
                instant.Players
                |> List.forall (fun p -> Hand.IsBust p.Hand || p.Hand = player.Hand)

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
        )

    let blankRows =
        List.replicate ((playerSlots - List.length instant.Players) * 3) (String.replicate width " ")

    let footer =
        "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    let lines =
        [
            rule
            padded status
            padded (instant.Event |> string |> centered width |> styled (CaptionStyle instant.Event))
            rule
        ]
        @ playerRows
        @ blankRows
        @ [ rule; ProgressBar timeline cursor; footer ]

    Console.SetCursorPosition(0, 0)
    Console.Out.Write(String.concat "\n" lines)

/// <summary>
/// Interactively scrubs through a timeline: the left and right arrow keys move
/// the cursor instant by instant, up and down jump between rounds, and home
/// and end snap to the start or end of the game.
/// </summary>
let public Run (source: string) (maybeTimeline: Instant array option) : unit =
    let loadTimeline = fun () -> source |> Persistence.ReadTimeline |> Seq.toArray
    let timeline = Option.defaultWith loadTimeline maybeTimeline

    if Array.isEmpty timeline then
        ()
    else

    let roundsBefore =
        timeline
        |> Array.map (fun instant -> if instant.Event.IsRoundEnded then 1 else 0)
        |> Array.scan (+) 0
        |> Array.take timeline.Length

    let totalRounds =
        timeline
        |> Array.filter (fun instant -> instant.Event.IsRoundEnded)
        |> Array.length

    let clamp cursor =
        max 0 (min (timeline.Length - 1) cursor)

    let previousRoundEnd cursor =
        [ 0 .. cursor - 1 ]
        |> List.tryFindBack (fun index -> timeline[index].Event.IsRoundEnded)
        |> Option.defaultValue 0

    let nextRoundEnd cursor =
        [ cursor + 1 .. timeline.Length - 1 ]
        |> List.tryFind (fun index -> timeline[index].Event.IsRoundEnded)
        |> Option.defaultValue (timeline.Length - 1)

    let rec loop (cursor: int) : unit =
        Render source timeline roundsBefore totalRounds cursor

        match (Console.ReadKey true).Key with
        | ConsoleKey.Q
        | ConsoleKey.Escape -> ()
        | ConsoleKey.LeftArrow -> loop (clamp (cursor - 1))
        | ConsoleKey.RightArrow -> loop (clamp (cursor + 1))
        | ConsoleKey.UpArrow
        | ConsoleKey.PageUp -> loop (previousRoundEnd cursor)
        | ConsoleKey.DownArrow
        | ConsoleKey.PageDown -> loop (nextRoundEnd cursor)
        | ConsoleKey.Home -> loop 0
        | ConsoleKey.End -> loop (timeline.Length - 1)
        | _ -> loop cursor

    loop 0
