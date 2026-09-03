module Interactive

open System

open Flip7

// The help page fills the asserted 80x24 window exactly: 24 lines, joined so
// the last one carries no newline and the screen does not scroll.
let private renderHelp () : unit =
    let rule = String.replicate 80 "─"
    let entry (keys: string) (description: string) =
        sprintf "   %s%s" (keys.PadRight 18) description

    let section (title: string) (annotation: string) =
        styled [ Ansi.Bright ] $" {title}" + styled [ Ansi.Dim ] $"  - {annotation}"

    [
        rule
        "help" |> centered 80
        rule
        styled [ Ansi.Bright ] " cursor"
        entry "↑/↓" "rotate between the distributions and each player"
        entry "←/→" "move along the distributions (wraps around)"
        ""
        section "deck" "with the cursor on the distributions"
        entry "+" "return a copy of the highlighted card to the deck"
        entry "-" "remove a copy of the highlighted card from the deck"
        ""
        section "dealing" "with the cursor on a player"
        entry "0-9" "value card 0-9"
        entry "x  e  t" "value card 10, 11, 12"
        entry "shift+2/4/6/8" "modifier card +2, +4, +6, +8"
        entry "shift+1  shift+x" "modifier card x2, +10"
        entry "s  d  f" "action card SecondChance, Deal3, Freeze"
        entry "backspace" "take the player's most recent card back into the deck"
        ""
        styled [ Ansi.Bright ] " program"
        entry "enter" "bank every hand's score and start the next round"
        entry "? or h" "this help page"
        entry "q or esc" "quit"
        "[any key] back to the table" |> centered 80 |> styled [ Ansi.Dim; Ansi.Cyan ]
    ]
    |> String.concat "\n"
    |> printf "%s"

let rec private loop
    (deck: Deck)
    (discards: Deck)
    (players: Map<uint, string * uint * Hand>)
    (cursor: Choice<Card, uint, Choice<Card, uint>>)
    : unit =
    Console.Clear()

    // The third cursor case is the help page, remembering the cursor to put
    // back once any key dismisses it
    match cursor with
    | Choice3Of3 resume ->
        renderHelp ()
        Console.ReadKey true |> ignore

        match resume with
        | Choice1Of2 card -> loop deck discards players (Choice1Of3 card)
        | Choice2Of2 player -> loop deck discards players (Choice2Of3 player)
    | _ ->

    if Deck.IsEmpty deck then
        loop discards Deck.Empty players cursor
    else

    let maybeCursorCard =
        match cursor with
        | Choice1Of3 hc -> Some hc
        | _ -> None

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
        | Choice1Of3 hc -> sprintf "p(%s)=%.2f%%" (string hc) (Map.find hc pdf * 100.0)
        | _ -> "pdf"
        |> centered (pdf |> Map.keys |> Seq.length)

    let cdfTitle =
        match cursor with
        | Choice1Of3 hc -> sprintf "P(%s)=%.2f%%" (string hc) (Map.find hc cdf * 100.0)
        | _ -> "cdf"
        |> centered (cdf |> Map.keys |> Seq.length)

    printfn "%s" (String.replicate 80 "─")
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (ecl + pdf3[0] + gap4 + cdf3[0])
    printfn "%s" (evl + pdf3[1] + gap4 + cdf3[1])
    printfn "%s" (var + pdf3[2] + gap4 + cdf3[2])
    printfn "%s" (std + pdfTitle + gap4 + cdfTitle)
    printfn "%s" (String.replicate 0 " ")
    printfn "%s" (String.replicate 80 "─")

    for tablePosition, (playerName, firmScore, hand) in Map.toList players do
        let isBust, reducedHand, _ = Hand.Reduce hand
        let tentativeScore = if isBust then 0u else Hand.Score reducedHand

        let onlyPlayerNotBusted =
            Map.forall (fun _ (name, _, hand) -> Hand.IsBust hand || name = playerName) players

        let probabilityToBust =
            Simulation.probabilityToBust deck discards reducedHand onlyPlayerNotBusted
            |> fun p -> p * 100.0

        let emojiStatus = bustEmoji probabilityToBust

        let preamble =
            sprintf "%s %s (%dpts + %dpts?, %.2f%%): " playerName emojiStatus firmScore tentativeScore probabilityToBust

        handRows 40 preamble hand
        |> fun (top, mid, bot) ->
            let isHighlighted = cursor = Choice2Of3 tablePosition
            let styles = if isHighlighted then [ Ansi.Inverse ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) ->
            let styles = if isBust then [ Ansi.Dim; Ansi.Italic ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) -> [ top; mid; bot ]
        |> String.concat "\n"
        |> printfn "%s"

    let issues =
        Simulation.Issues deck discards (players |> Map.values |> Seq.map (fun (_, __, hand) -> hand))
        |> Seq.toList

    let winner =
        let name, score, _ = players |> Map.values |> Seq.maxBy (fun (_, score, _) -> score)
        if score >= 200u then Some(name, score) else None

    match issues, winner with
    | firstIssue :: _otherIssues, _ ->
        firstIssue
        |> styled [ Ansi.Underline ]
        |> centered 80
        |> styled [ Ansi.BrightRed ]
    | [], Some(name, score) ->
        $"game over: {name} wins with {score}pts!"
        |> centered 80
        |> styled [ Ansi.BrightGreen ]
    | [], None ->
        "[↔] distributions   [↕] players   [?] help   [q/esc] quit"
        |> centered 80
        |> styled [ Ansi.Dim; Ansi.Cyan ]
    |> printf "%s"

    let addCardToPlayer card player =
        let deck' = Deck.Decrement deck card
        let name, firmScore, hand = Map.find player players
        let players' = Map.add player (name, firmScore, card :: hand) players
        loop deck' discards players' cursor

    let popCardFromPlayer player =
        let name, firmScore, hand = Map.find player players
        match hand with
        | [] -> loop deck discards players cursor
        | card :: rest ->
            let deck' = Deck.Increment deck card
            let players' = Map.add player (name, firmScore, rest) players
            loop deck' discards players' cursor

    let commitRound () =
        if issues <> [] then
            loop deck discards players cursor
        else

        let discards' =
            players
            |> Map.values
            |> Seq.map (fun (_, _, hand) -> hand)
            |> Seq.collect id
            |> Seq.fold Deck.Increment discards

        let players' =
            players
            |> Map.map (fun _ (name, score, hand) ->
                let isBust, reducedHand, _ = Hand.Reduce hand
                let handScore = if isBust then 0u else Hand.Score reducedHand
                name, score + handScore, List.empty
            )

        let cursor' = Choice1Of3(ValueCard Card.Zero)
        loop deck discards' players' cursor'

    let cards = Deck.Empty |> Map.toList |> List.map fst
    let playerNames = players |> Map.keys |> Seq.toList
    let key = Console.ReadKey true

    match key.Modifiers, key.Key, cursor with
    // Program flow control
    | ConsoleModifiers.None, ConsoleKey.Q, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Escape, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Enter, _ -> commitRound ()

    // Opening the help page, remembering the cursor it was opened over
    | _, _, Choice1Of3 card when key.KeyChar = '?' || key.Key = ConsoleKey.H ->
        loop deck discards players (Choice3Of3(Choice1Of2 card))
    | _, _, Choice2Of3 player when key.KeyChar = '?' || key.Key = ConsoleKey.H ->
        loop deck discards players (Choice3Of3(Choice2Of2 player))

    // Adding cards to current players hands
    | ConsoleModifiers.None, ConsoleKey.D0, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Zero) player
    | ConsoleModifiers.Shift, ConsoleKey.D1, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Double) player
    | ConsoleModifiers.None, ConsoleKey.D1, Choice2Of3 player -> addCardToPlayer (ValueCard Card.One) player
    | ConsoleModifiers.Shift, ConsoleKey.D2, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Plus2) player
    | ConsoleModifiers.None, ConsoleKey.D2, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Two) player
    | ConsoleModifiers.None, ConsoleKey.D3, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Three) player
    | ConsoleModifiers.Shift, ConsoleKey.D4, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Plus4) player
    | ConsoleModifiers.None, ConsoleKey.D4, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Four) player
    | ConsoleModifiers.None, ConsoleKey.D5, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Five) player
    | ConsoleModifiers.Shift, ConsoleKey.D6, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Plus6) player
    | ConsoleModifiers.None, ConsoleKey.D6, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Six) player
    | ConsoleModifiers.None, ConsoleKey.D7, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Seven) player
    | ConsoleModifiers.Shift, ConsoleKey.D8, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Plus8) player
    | ConsoleModifiers.None, ConsoleKey.D8, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Eight) player
    | ConsoleModifiers.None, ConsoleKey.D9, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Nine) player
    | ConsoleModifiers.Shift, ConsoleKey.X, Choice2Of3 player -> addCardToPlayer (ModifierCard Card.Plus10) player
    | ConsoleModifiers.None, ConsoleKey.X, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Ten) player
    | ConsoleModifiers.None, ConsoleKey.E, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Eleven) player
    | ConsoleModifiers.None, ConsoleKey.T, Choice2Of3 player -> addCardToPlayer (ValueCard Card.Twelve) player
    | ConsoleModifiers.None, ConsoleKey.S, Choice2Of3 player -> addCardToPlayer (ActionCard Card.SecondChance) player
    | ConsoleModifiers.None, ConsoleKey.D, Choice2Of3 player -> addCardToPlayer (ActionCard Card.Deal3) player
    | ConsoleModifiers.None, ConsoleKey.F, Choice2Of3 player -> addCardToPlayer (ActionCard Card.Freeze) player

    // Removing cards from current players hands
    | ConsoleModifiers.None, ConsoleKey.Backspace, Choice2Of3 player -> popCardFromPlayer player

    // Modifying card counts in the deck
    | ConsoleModifiers.None, ConsoleKey.Add, Choice1Of3 card -> loop (Deck.Increment deck card) discards players cursor
    | ConsoleModifiers.None, ConsoleKey.Subtract, Choice1Of3 card ->
        loop (Deck.Decrement deck card) discards players cursor

    // Navigating through players and cards
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice1Of3 _card ->
        loop deck discards players (Choice2Of3 playerNames[playerNames.Length - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice1Of3 _card ->
        loop deck discards players (Choice2Of3 playerNames[playerNames.Length - playerNames.Length])
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of3 player when player = playerNames[0] ->
        loop deck discards players (Choice1Of3(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of3 player when player = playerNames[playerNames.Length - 1] ->
        loop deck discards players (Choice1Of3(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of3 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players (Choice2Of3 playerNames[index - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of3 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players (Choice2Of3 playerNames[index + 1])
    | ConsoleModifiers.None, ConsoleKey.LeftArrow, Choice1Of3 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index - 1 + cards.Length) % cards.Length
        loop deck discards players (Choice1Of3 cards[index'])
    | ConsoleModifiers.None, ConsoleKey.RightArrow, Choice1Of3 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index + 1) % cards.Length
        loop deck discards players (Choice1Of3 cards[index'])
    | _ -> loop deck discards players cursor

let public Run (playerNames: string list) : unit =
    if playerNames.Length <= 0 then
        raise (ArgumentException "Please provide at least one player name as a command-line argument.")
    if playerNames.Length > 5 then
        raise (ArgumentException "Please provide no more than five player names as command-line arguments.")
    if playerNames |> List.distinct |> List.length <> playerNames.Length then
        raise (ArgumentException "Player names must be unique.")

    let deck: Deck = Deck.Full
    let discards: Deck = Deck.Empty
    let cursor = Choice1Of3(ValueCard Card.Zero)

    let players: Map<uint, string * uint * Hand> =
        playerNames
        |> List.indexed
        |> List.map (fun (tablePosition, name) -> uint tablePosition, (name, 0u, List.empty))
        |> Map.ofList

    loop deck discards players cursor
