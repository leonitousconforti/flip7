[<AutoOpen>]
module public Render

open Flip7

let private width = 80
let private playerSlots = 5
let private barWidth = width - 2

/// Pads a string to the full width of the console, so that overwriting in place
/// erases the previous frame instead of leaving remnants behind.
let private padded (s: string) : string =
    s + String.replicate (max 0 (width - visualLength s)) " "

/// How scared a player should be of busting.
let private bustEmoji (probabilityToBust: float) : string =
    if probabilityToBust >= 50.0 then "😵"
    elif probabilityToBust >= 45.0 then "😵‍💫"
    elif probabilityToBust >= 40.0 then "🫪"
    elif probabilityToBust >= 35.0 then "🫣"
    elif probabilityToBust >= 30.0 then "😱"
    elif probabilityToBust >= 25.0 then "😰"
    elif probabilityToBust >= 20.0 then "😬"
    elif probabilityToBust >= 15.0 then "😐"
    elif probabilityToBust >= 10.0 then "🤔"
    elif probabilityToBust >= 5.0 then "🙂"
    else "😎"

/// Compact alternative to Card.ToString for the fixed-width card boxes: the
/// full action card names are too wide and would break the box layout.
let private cardLabel (card: Card) : string =
    match card with
    | ActionCard Card.Deal3 -> "D3"
    | ActionCard Card.Freeze -> "FZ"
    | ActionCard Card.SecondChance -> "SC"
    | _ -> card.ToString()

/// Renders a hand as three rows of card boxes, with the preamble padded to a
/// fixed width on the middle row so the boxes of all players line up.
let private handRows (padTo: int) (preamble: string) (hand: Hand) : string * string * string =
    hand
    |> List.fold
        (fun (topRow, midRow, botRow) card ->
            let c = (cardLabel card).PadRight(2).PadLeft(3)
            topRow + "┌───┐", midRow + $"│{c}│", botRow + "└───┘"
        )
        (String.replicate padTo " ", preamble.PadRight padTo, String.replicate padTo " ")

/// Style for an Event.
let private captionStyle (event: Event) : string list =
    match event with
    | Busted _ -> [ Ansi.BrightRed ]
    | Flip7Achieved _ -> [ Ansi.BrightMagenta ]
    | RoundEnded _ -> [ Ansi.BrightYellow ]
    | Froze _ -> [ Ansi.BrightCyan ]
    | _ -> []

/// Renders a progress bar for the timeline, with round boundaries and the
/// current cursor position marked. Takes plain values rather than the store,
/// so a single Snapshot per frame flows down through the whole render.
let private progressBar (count: int) (roundEnds: int array) (cursor: int) : string =
    let cellOf index = index * barWidth / count
    let cells = Array.create barWidth "─"

    for index in roundEnds do
        cells[cellOf index] <- "┊"

    cells[cellOf cursor] <- styled [ Ansi.BrightGreen ] "●"
    "├" + String.concat "" cells + "┤"

let public RenderTable
    (source: string)
    ((count, isComplete, roundEnds): int * bool * int array)
    (cursor: int)
    (instant: Instant)
    : unit =
    let round = Persistence.TimelineStore.RoundOf roundEnds cursor
    let knownRounds = Persistence.TimelineStore.RoundOf roundEnds (count - 1)
    let actor = instant.Event.Actor()
    let rule = String.replicate width "─"

    let growing = if isComplete then "" else "+"
    let caption, captionStyle =
        if isComplete && cursor = count - 1 then
            let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)
            $"game over: {winner.Name} wins with {winner.FirmScore}pts!", [ Ansi.BrightGreen ]
        else
            string instant.Event, captionStyle instant.Event

    let status =
        let left = $"replay: {source}"
        let right =
            $"round {round}/{knownRounds}{growing}   instant {cursor + 1}/{count}{growing}"
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
    System.Console.SetCursorPosition(0, 0)
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
    printfn "%s" (padded (progressBar count roundEnds cursor))
    printf "%s" (padded footer)

let public RenderError
    (source: string)
    ((count, _isComplete, roundEnds): int * bool * int array)
    (cursor: int)
    (error: exn)
    : unit =
    let rule = String.replicate width "─"

    let status =
        let left = $"replay: {source}"
        let right = $"instant {cursor + 1}/{count}"
        let middle =
            String.replicate (max 0 (width - visualLength left - visualLength right)) " "
        left + middle + right

    let caption =
        $"could not read instant {cursor + 1} - {error.Message}"
        |> centered width
        |> styled [ Ansi.BrightRed ]

    let footer =
        "[↔] scrub   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    System.Console.SetCursorPosition(0, 0)
    printfn "%s" (padded rule)
    printfn "%s" (padded status)
    printfn "%s" (padded caption)
    printfn "%s" (padded rule)

    for _ in 1 .. playerSlots * 3 do
        printfn "%s" (padded "")

    printfn "%s" (padded rule)
    printfn "%s" (padded (progressBar count roundEnds cursor))
    printf "%s" (padded footer)

let public RenderLoading
    (source: string)
    ((count, _isComplete, roundEnds): int * bool * int array)
    (cursor: int)
    (spin: int)
    : unit =
    let rule = String.replicate width "─"
    let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

    let status =
        let left = $"replay: {source}"
        let right = $"instant {cursor + 1}/{count}"
        let middle =
            String.replicate (max 0 (width - visualLength left - visualLength right)) " "
        left + middle + right

    let caption =
        $"{spinner[spin % spinner.Length]} loading instant {cursor + 1}"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    let footer =
        "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    System.Console.SetCursorPosition(0, 0)
    printfn "%s" (padded rule)
    printfn "%s" (padded status)
    printfn "%s" (padded caption)
    printfn "%s" (padded rule)

    for _ in 1 .. playerSlots * 3 do
        printfn "%s" (padded "")

    printfn "%s" (padded rule)
    printfn "%s" (padded (progressBar count roundEnds cursor))
    printf "%s" (padded footer)
