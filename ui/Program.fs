module Program

open System

open Flip7

let rec private loop
    (deck: Deck)
    (discards: Deck)
    (players: Map<string, uint * Hand>)
    (cursor: Choice<Card, string>)
    : unit =
    Console.Clear()

    if Deck.IsEmpty deck then
        loop discards Deck.Empty players cursor
    else

    let maybeCursorCard =
        match cursor with
        | Choice2Of2 _ -> None
        | Choice1Of2 hc -> Some hc

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
        match cursor with
        | Choice2Of2 _ -> "pdf"
        | Choice1Of2 hc -> sprintf "p(%s)=%.2f%%" (string hc) (Map.find hc pdf * 100.0)
        |> centered (pdf |> Map.keys |> Seq.length)

    let cdfTitle =
        match cursor with
        | Choice2Of2 _ -> "cdf"
        | Choice1Of2 hc -> sprintf "P(%s)=%.2f%%" (string hc) (Map.find hc cdf * 100.0)
        |> centered (cdf |> Map.keys |> Seq.length)

    printfn "%s" (String.replicate 80 "─")
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (ecl + pdf3[0] + gap4 + cdf3[0])
    printfn "%s" (evl + pdf3[1] + gap4 + cdf3[1])
    printfn "%s" (var + pdf3[2] + gap4 + cdf3[2])
    printfn "%s" (std + pdfTitle + gap4 + cdfTitle)
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (String.replicate 80 "─")

    for playerName, (firmScore, hand) in players |> Map.toList do
        let tentativeScore = Hand.Score hand

        let onlyPlayerNotBusted =
            players |> Map.forall (fun _ (_, h) -> Hand.IsBust h || h = hand)

        let probabilityToBust =
            Simulation.probabilityToBust deck discards hand onlyPlayerNotBusted
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
            sprintf "%s %s (%dpts + %dpts?, %.2f%%): " playerName emojiStatus firmScore tentativeScore probabilityToBust

        hand
        |> List.fold
            (fun (topRow, midRow, botRow) card ->
                let c = card.ToString().PadRight(2).PadLeft(3)
                let topRow' = topRow + $"┌───┐"
                let midRow' = midRow + $"│{c}│"
                let botRow' = botRow + $"└───┘"
                topRow', midRow', botRow'
            )
            (String.replicate 40 " ", preamble.PadRight 40, String.replicate 40 " ")
        |> fun (top, mid, bot) ->
            let isHighlighted = cursor = Choice2Of2 playerName
            let styles = if isHighlighted then [ Ansi.Inverse ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) ->
            let isBust = Hand.IsBust hand
            let styles = if isBust then [ Ansi.Dim; Ansi.Italic ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) -> [ top; mid; bot ]
        |> String.concat "\n"
        |> printfn "%s"

    let errors =
        Simulation.IsValid deck discards (players |> Map.values |> Seq.map snd)
        |> Seq.toList

    match errors with
    | firstError :: _otherErrors ->
        firstError
        |> styled [ Ansi.Underline ]
        |> centered 80
        |> styled [ Ansi.BrightRed ]
    | [] ->
        "[↔] move along distributions   [↕] rotate through players   [q/esc] quit"
        |> centered 80
        |> styled [ Ansi.Dim; Ansi.Cyan ]
    |> printf "%s"

    let addCardToPlayer card player =
        let deck' = Deck.Decrement deck card
        let firmScore, hand = Map.find player players
        let hand' = card :: hand
        let players' = Map.add player (firmScore, hand') players
        loop deck' discards players' cursor

    let popCardFromPlayer player =
        let firmScore, hand = Map.find player players
        match hand with
        | [] -> loop deck discards players cursor
        | card :: rest ->
            let deck' = Deck.Increment deck card
            let players' = Map.add player (firmScore, rest) players
            loop deck' discards players' cursor

    let commitSession () =
        if errors <> [] then
            loop deck discards players cursor
        else

        let discards' =
            players
            |> Map.values
            |> Seq.map snd
            |> Seq.collect id
            |> Seq.fold Deck.Increment discards

        let players' =
            players
            |> Map.map (fun _ (score, hand) ->
                let handScore = if Hand.IsBust hand then 0u else Hand.Score hand
                score + uint handScore, List.empty
            )

        let cursor' = Choice1Of2(ValueCard Card.Zero)
        loop deck discards' players' cursor'

    let cards = Deck.Empty |> Map.toList |> List.map fst
    let playerNames = players |> Map.keys |> Seq.toList
    let key = Console.ReadKey true

    match key.Modifiers, key.Key, cursor with
    // Program flow control
    | ConsoleModifiers.None, ConsoleKey.Q, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Escape, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Enter, _ -> commitSession ()

    // Adding cards to current players hands
    | ConsoleModifiers.None, ConsoleKey.D0, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Zero) player
    | ConsoleModifiers.Shift, ConsoleKey.D1, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Double) player
    | ConsoleModifiers.None, ConsoleKey.D1, Choice2Of2 player -> addCardToPlayer (ValueCard Card.One) player
    | ConsoleModifiers.Shift, ConsoleKey.D2, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Plus2) player
    | ConsoleModifiers.None, ConsoleKey.D2, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Two) player
    | ConsoleModifiers.None, ConsoleKey.D3, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Three) player
    | ConsoleModifiers.Shift, ConsoleKey.D4, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Plus4) player
    | ConsoleModifiers.None, ConsoleKey.D4, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Four) player
    | ConsoleModifiers.None, ConsoleKey.D5, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Five) player
    | ConsoleModifiers.Shift, ConsoleKey.D6, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Plus6) player
    | ConsoleModifiers.None, ConsoleKey.D6, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Six) player
    | ConsoleModifiers.None, ConsoleKey.D7, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Seven) player
    | ConsoleModifiers.Shift, ConsoleKey.D8, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Plus8) player
    | ConsoleModifiers.None, ConsoleKey.D8, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Eight) player
    | ConsoleModifiers.None, ConsoleKey.D9, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Nine) player
    | ConsoleModifiers.Shift, ConsoleKey.X, Choice2Of2 player -> addCardToPlayer (ModifierCard Card.Plus10) player
    | ConsoleModifiers.None, ConsoleKey.X, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Ten) player
    | ConsoleModifiers.None, ConsoleKey.E, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Eleven) player
    | ConsoleModifiers.None, ConsoleKey.T, Choice2Of2 player -> addCardToPlayer (ValueCard Card.Twelve) player
    | ConsoleModifiers.None, ConsoleKey.S, Choice2Of2 player -> addCardToPlayer (ActionCard Card.SecondChance) player
    | ConsoleModifiers.None, ConsoleKey.D, Choice2Of2 player -> addCardToPlayer (ActionCard Card.Deal3) player
    | ConsoleModifiers.None, ConsoleKey.F, Choice2Of2 player -> addCardToPlayer (ActionCard Card.Freeze) player

    // Removing cards from current players hands
    | ConsoleModifiers.None, ConsoleKey.Backspace, Choice2Of2 player -> popCardFromPlayer player

    // Modifying card counts in the deck
    | ConsoleModifiers.None, ConsoleKey.Add, Choice1Of2 card -> loop (Deck.Increment deck card) discards players cursor
    | ConsoleModifiers.None, ConsoleKey.Subtract, Choice1Of2 card ->
        loop (Deck.Decrement deck card) discards players cursor

    // Navigating through players and cards
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice1Of2 _card ->
        loop deck discards players (Choice2Of2 playerNames[playerNames.Length - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice1Of2 _card ->
        loop deck discards players (Choice2Of2 playerNames[playerNames.Length - playerNames.Length])
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of2 player when player = playerNames[0] ->
        loop deck discards players (Choice1Of2(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of2 player when player = playerNames[playerNames.Length - 1] ->
        loop deck discards players (Choice1Of2(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of2 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players (Choice2Of2 playerNames[index - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of2 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players (Choice2Of2 playerNames[index + 1])
    | ConsoleModifiers.None, ConsoleKey.LeftArrow, Choice1Of2 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index - 1 + cards.Length) % cards.Length
        loop deck discards players (Choice1Of2 cards[index'])
    | ConsoleModifiers.None, ConsoleKey.RightArrow, Choice1Of2 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index + 1) % cards.Length
        loop deck discards players (Choice1Of2 cards[index'])
    | _ -> loop deck discards players cursor

[<EntryPoint>]
let main args =
    System.Diagnostics.Debug.Assert(
        args.Length > 0,
        "Please provide at least one player name as a command-line argument."
    )
    System.Diagnostics.Debug.Assert(
        args.Length <= 5,
        "Please provide no more than five player names as command-line arguments."
    )
    System.Diagnostics.Debug.Assert(
        args |> Array.forall (fun name -> not (String.IsNullOrWhiteSpace name)),
        "Player names cannot be empty or whitespace."
    )
    System.Diagnostics.Debug.Assert(
        args |> Array.distinct |> Array.length = args.Length,
        "Player names must be unique."
    )
    System.Diagnostics.Debug.Assert(
        Console.WindowWidth = 80 && Console.WindowHeight = 24,
        "Console window should be 80x24, please resize it."
    )

    let deck: Deck = Deck.Full
    let discards: Deck = Deck.Empty
    let players: Map<string, uint * Hand> =
        Array.map (fun name -> name, (0u, List.empty)) args |> Map.ofArray

    Console.Clear()
    Console.CursorVisible <- false
    loop deck discards players (Choice1Of2(ValueCard Card.Zero))
    Console.CursorVisible <- true
    Console.Clear()

    0
