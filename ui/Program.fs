module Program

open System

open Flip7

let private hi = "\027[7m"
let private lo = "\027[0m"
let private green = "\027[92m"

let mutable private Cursor: Choice<Card, string> = Choice1Of2(ValueCard Card.Zero)

let inline private centered (s: string) (width: int) : string =
    let paddingCount = max 0 ((width - s.Length) / 2)
    let padding = String.replicate paddingCount " "
    padding + s + padding + (if s.Length % 2 = 1 then " " else "")

let inline private sparkline< ^a when ^a: equality>
    (rows: int)
    (cursor: ^a option)
    (series: (^a * float) list)
    : string list =
    let blockChar f =
        if f >= 7.0 / 8.0 then "█"
        elif f >= 6.0 / 8.0 then "▇"
        elif f >= 5.0 / 8.0 then "▆"
        elif f >= 4.0 / 8.0 then "▅"
        elif f >= 3.0 / 8.0 then "▄"
        elif f >= 2.0 / 8.0 then "▃"
        elif f >= 1.0 / 8.0 then "▂"
        elif f >= 0.0 / 8.0 then "▁"
        else " "

    let barCell (f: float) (r: int) =
        let scaled = f * float rows
        if scaled >= float r + 1.0 then "█"
        elif scaled > float r then blockChar (scaled - float r)
        else " "

    [ rows - 1 .. -1 .. 0 ]
    |> List.map (fun row ->
        series
        |> List.map (fun (label, value) ->
            let cell = barCell value row
            if cursor = Some label then $"{green}{cell}{lo}" else cell
        )
        |> String.concat ""
    )

let private renderFrame (deck: Deck) (discards: Deck) (players: Simulation.Player list) : unit =
    let inline normalizeDistribution (series: (^a * float) list) : (^a * float) list =
        let maxProb = series |> List.map snd |> List.max
        series |> List.map (fun (label, prob) -> label, prob / maxProb)

    let maybeCursorCard =
        match Cursor with
        | Choice2Of2 _ -> None
        | Choice1Of2 highlightedCard -> Some highlightedCard

    let pdf = deck |> Deck.pdf
    let cdf = deck |> Deck.cdf

    let gap4: string = String.replicate 4 " "
    let pdf3 = pdf |> Map.toList |> normalizeDistribution |> sparkline 3 maybeCursorCard
    let cdf3 = cdf |> Map.toList |> normalizeDistribution |> sparkline 3 maybeCursorCard

    let ecl = (sprintf "ec:     %s" (Deck.ec deck |> string)).PadRight 20
    let evl = (sprintf "ev:     %.2f" (Deck.ev deck)).PadRight 20
    let var = (sprintf "var:    %.2f" (Deck.var deck)).PadRight 20
    let std = (sprintf "std:    %.2f" (Deck.std deck)).PadRight 20

    let pdfTitle =
        match Cursor with
        | Choice2Of2 _ -> centered "pdf" (pdf |> Map.keys |> Seq.length)
        | Choice1Of2 card ->
            let prob = pdf |> Map.find card
            let title = sprintf "p(%s)=%.2f%%" (string card) (prob * 100.0)
            centered title (pdf |> Map.keys |> Seq.length)

    let cdfTitle =
        match Cursor with
        | Choice2Of2 _ -> centered "cdf" (cdf |> Map.keys |> Seq.length)
        | Choice1Of2 card ->
            let prob = cdf |> Map.find card
            let title = sprintf "P(%s)=%.2f%%" (string card) (prob * 100.0)
            centered title (cdf |> Map.keys |> Seq.length)

    printfn "%s" (String.replicate 80 "─")
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (ecl + pdf3[0] + gap4 + cdf3[0])
    printfn "%s" (evl + pdf3[1] + gap4 + cdf3[1])
    printfn "%s" (var + pdf3[2] + gap4 + cdf3[2])
    printfn "%s" (std + pdfTitle + gap4 + cdfTitle)
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (String.replicate 80 "─")

    for playerName, strategy, hand in players do
        let currentScore = 0
        let tentativeScore = Hand.Score hand

        let probabilityToBust =
            hand
            |> List.filter (fun card -> card.IsValueCard)
            |> List.map (fun card -> Map.find card pdf)
            |> List.sum
            |> fun p -> p * 100.0

        let emojiStatus =
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

        let preamble =
            sprintf
                "%s %s (%dpts + %dpts?, %.2f%%): "
                playerName
                emojiStatus
                currentScore
                tentativeScore
                probabilityToBust

        hand
        |> List.fold
            (fun (lastTopRow, lastMidRow, lastBotRow) card ->
                let c = card.ToString().PadRight(2).PadLeft(3)
                let newTopRow = lastTopRow + $"┌───┐"
                let newMidRow = lastMidRow + $"│{c}│"
                let newBotRow = lastBotRow + $"└───┘"
                newTopRow, newMidRow, newBotRow
            )
            (String.replicate 40 " ", preamble.PadRight 40, String.replicate 40 " ")
        |> fun (top, middle, bottom) ->
            if Cursor = Choice2Of2 playerName then
                [ $"{hi}{top}{lo}"; $"{hi}{middle}{lo}"; $"{hi}{bottom}{lo}" ]
            else
                [ top; middle; bottom ]
        |> String.concat "\n"
        |> printfn "%s"

    printf "%s" (centered "[↔] move along distributions   [↕] rotate through players   [q/esc] quit" 80)

let private readKey (players: Simulation.Player list) (key: ConsoleKeyInfo) : bool =
    let cards = Deck.Empty |> Map.toList |> List.map fst
    let playerNames = players |> List.map (fun (name, strategy, hand) -> name)

    match key.Key, Cursor with
    | ConsoleKey.Q, _ -> false
    | ConsoleKey.Escape, _ -> false
    | ConsoleKey.LeftArrow, Choice1Of2 card ->
        let currentIndex = cards |> List.findIndex (fun c -> c = card)
        let newIndex = (currentIndex - 1 + cards.Length) % cards.Length
        Cursor <- Choice1Of2 cards[newIndex]
        true
    | ConsoleKey.RightArrow, Choice1Of2 card ->
        let currentIndex = cards |> List.findIndex (fun c -> c = card)
        let newIndex = (currentIndex + 1) % cards.Length
        Cursor <- Choice1Of2 cards[newIndex]
        true
    | ConsoleKey.UpArrow, Choice1Of2 _ ->
        Cursor <- Choice2Of2 playerNames[playerNames.Length - 1]
        true
    | ConsoleKey.UpArrow, Choice2Of2 player ->
        let currentIndex = playerNames |> List.findIndex (fun name -> name = player)
        if currentIndex = 0 then
            Cursor <- Choice1Of2(ValueCard Card.Zero)
        else
            Cursor <- Choice2Of2 playerNames[currentIndex - 1]
        true
    | ConsoleKey.DownArrow, Choice1Of2 _ ->
        Cursor <- Choice2Of2 playerNames[0]
        true
    | ConsoleKey.DownArrow, Choice2Of2 player ->
        let currentIndex = playerNames |> List.findIndex (fun name -> name = player)
        if currentIndex = List.length playerNames - 1 then
            Cursor <- Choice1Of2(ValueCard Card.Zero)
        else
            Cursor <- Choice2Of2 playerNames[currentIndex + 1]
        true
    | _ -> true

[<EntryPoint>]
let main args =
    let deck = Deck.Random
    let discards = Deck.Empty

    let players: Simulation.Player list = [
        ("Alice", Strategy.Random, [ Card.ValueCard Card.Ten; Card.ModifierCard Card.Plus4 ])
        ("Bob", Strategy.Random, [ Card.ValueCard Card.Nine; Card.ValueCard Card.Three ])
        ("Charlie", Strategy.Random, [ Card.ValueCard Card.Eight; Card.ValueCard Card.Four ])
        ("Dave", Strategy.Random, [ Card.ValueCard Card.Seven; Card.ModifierCard Card.Plus10 ])
        ("Ethan", Strategy.Random, [ Card.ValueCard Card.Six; Card.ModifierCard Card.Double ])
    ]

    Console.CursorVisible <- false
    let mutable running = true

    while running do
        Console.Clear()
        renderFrame deck discards players
        running <- readKey players (Console.ReadKey true)

    Console.CursorVisible <- true
    Console.Clear()

    0
