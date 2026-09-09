[<AutoOpen>]
module public Render

open Flip7

let private width = 80
let private playerSlots = 5
let private barWidth = width - 2
let private spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

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
let private progressBar (count: int) (roundEnds: int list) (cursor: int) : string =
    let cellOf index = index * barWidth / count
    let cells = Array.create barWidth "─"

    for index in roundEnds do
        cells[cellOf index] <- "┊"

    cells[cellOf cursor] <- styled [ Ansi.BrightGreen ] "●"
    "├" + String.concat "" cells + "┤"

/// The left and right ends of a status row, justified to the full width.
let private statusLine (left: string) (right: string) : string =
    let leftWidth = visualLength left
    let rightWidth = visualLength right
    let middle = String.replicate (max 0 (width - leftWidth - rightWidth)) " "
    left + middle + right

let public playerRow
    (probabilityToBust: float)
    (highlighted: bool)
    (dimmed: bool)
    (annotation: string)
    (player: Strategy.StrategyPlayer)
    : string =
    let isBusted = Hand.IsBust player.Hand
    let tentativeScore = if isBusted then 0u else Hand.Score player.Hand

    let preamble =
        sprintf
            "%s%s %s (%dpts + %dpts?, %.2f%%): "
            player.Name
            annotation
            (bustEmoji probabilityToBust)
            player.FirmScore
            tentativeScore
            probabilityToBust

    ((String.replicate 40 " ", preamble.PadRight 40, String.replicate 40 " "), player.Hand)
    ||> List.fold (fun (topRow, midRow, botRow) card ->
        let c = (cardLabel card).PadRight(2).PadLeft(3)
        topRow + "┌───┐", midRow + $"│{c}│", botRow + "└───┘"
    )
    |> fun (top, mid, bot) ->
        let styles = if highlighted then [ Ansi.Inverse ] else []
        styled styles top, styled styles mid, styled styles bot
    |> fun (top, mid, bot) ->
        let styles = if dimmed then [ Ansi.Dim; Ansi.Italic ] else []
        styled styles top, styled styles mid, styled styles bot
    |> fun (top, mid, bot) -> [ padded top; padded mid; padded bot ]
    |> String.concat "\n"

let public playerRows (instant: Instant) : string list =
    let actor = instant.Event.Actor()

    instant.Players
    |> List.map (fun player ->
        let onlyPlayerNotBusted =
            instant.Players
            |> List.forall (fun p -> Hand.IsBust p.Hand || p.Name = player.Name)

        let probabilityToBust =
            Simulation.probabilityToBust instant.Deck instant.Discards player.Hand onlyPlayerNotBusted
            * 100.0

        let annotation = ""
        let highlighted = actor = Some player.Name
        let dimmed = Hand.IsBust player.Hand
        let player = player.ToStrategyPlayer()
        playerRow probabilityToBust highlighted dimmed annotation player
    )

let private Frame (status: string) (caption: string) (content: string list) (bottom: string) (footer: string) : unit =
    let rule = String.replicate width "─"

    System.Console.SetCursorPosition(0, 0)
    printfn "%s" (padded rule)
    printfn "%s" (padded status)
    printfn "%s" (padded caption)
    printfn "%s" (padded rule)

    for rows in content do
        printfn "%s" rows

    for _ in 1 .. (playerSlots - List.length content) * 3 do
        printfn "%s" (padded "")

    printfn "%s" (padded rule)
    printfn "%s" (padded bottom)
    printf "%s" (padded footer)

let public RenderTable
    (source: string)
    ((count, isComplete, roundEnds): int * bool * int list)
    (cursor: int)
    (instant: Instant)
    : unit =
    let round = Persistence.TimelineStore.RoundOf roundEnds cursor
    let knownRounds = Persistence.TimelineStore.RoundOf roundEnds (count - 1)
    let growing = if isComplete then "" else "+"

    let statusRight = $"replay: {source}"
    let statusLeftRound = $"round {round}/{knownRounds}{growing}"
    let statusLeftCursor = $"instant {cursor + 1}/{count}{growing}"
    let status = statusLine statusRight $"{statusLeftRound}   {statusLeftCursor}"

    let caption =
        if isComplete && cursor = count - 1 then
            let winner = instant.Players |> List.maxBy (fun player -> player.FirmScore)

            $"game over: {winner.Name} wins with {winner.FirmScore}pts!"
            |> centered width
            |> styled [ Ansi.BrightGreen ]
        else
            string instant.Event |> centered width |> styled (captionStyle instant.Event)

    let content = playerRows instant
    let bottom = $"\n{progressBar count roundEnds cursor}\n"
    let footer =
        "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    Frame status caption content bottom footer

let public RenderError
    (source: string)
    ((count, _isComplete, roundEnds): int * bool * int list)
    (cursor: int)
    (error: exn)
    : unit =
    let status = statusLine $"replay: {source}" $"instant {cursor + 1}/{count}"
    let caption =
        $"could not read instant {cursor + 1} - {error.Message}"
        |> centered width
        |> styled [ Ansi.BrightRed ]

    let content = []
    let bottom = $"\n{progressBar count roundEnds cursor}\n"
    let footer =
        "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    Frame status caption content bottom footer

let public RenderLoading
    (source: string)
    ((count, _isComplete, roundEnds): int * bool * int list)
    (cursor: int)
    (spin: int)
    : unit =
    let status = statusLine $"replay: {source}" $"instant {cursor + 1}/{count}"
    let caption =
        $"{spinner[spin % spinner.Length]} loading instant {cursor + 1}"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    let content = []
    let bottom = $"\n{progressBar count roundEnds cursor}\n"
    let footer =
        "[↔] scrub   [↕] jump rounds   [home/end] start/end   [q/esc] quit"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]

    Frame status caption content bottom footer

let public RenderPlay (round: int) (instant: Instant) (footer: string) : unit =
    let status = statusLine "play: first to 200pts wins" $"round {round}"
    let captionStyle = captionStyle instant.Event
    let caption = string instant.Event |> centered width |> styled captionStyle
    let content = playerRows instant
    let bottom = "\n\n\n"
    Frame status caption content bottom footer
