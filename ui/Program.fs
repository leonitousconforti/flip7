module Program

open System

open Flip7

// The help page fills the asserted 80x24 window exactly: 24 lines, joined so
// the last one carries no newline and the screen does not scroll.
let private renderHelp () : unit =
    let rule = String.replicate 80 "─"
    let entry (keys: string) (description: string) = sprintf "   %s%s" (keys.PadRight 18) description

    let section (title: string) (annotation: string) =
        styled [ Ansi.Bright ] $" {title}" + styled [ Ansi.Dim ] $"  — {annotation}"

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
        entry "?  h" "this help page"
        entry "q  esc" "quit"
        "[any key] back to the table" |> centered 80 |> styled [ Ansi.Dim; Ansi.Cyan ]
    ]
    |> String.concat "\n"
    |> printf "%s"

let rec private loop
    (deck: Deck)
    (discards: Deck)
    (players: Map<string, (uint * Hand)>)
    (simulation: Timeline)
    (cursor: Choice<Card, string, Choice<Card, string>>)
    : unit =
    Console.Clear()

    // The third cursor case is the help page, remembering the cursor to put
    // back once any key dismisses it
    match cursor with
    | Choice3Of3 resume ->
        renderHelp ()
        Console.ReadKey true |> ignore

        match resume with
        | Choice1Of2 card -> loop deck discards players simulation (Choice1Of3 card)
        | Choice2Of2 player -> loop deck discards players simulation (Choice2Of3 player)
    | _ ->

    if Deck.IsEmpty deck then
        loop discards Deck.Empty players simulation cursor
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

    for playerName, (firmScore, hand) in players |> Map.toList do
        let tentativeScore = Hand.Score hand

        let onlyPlayerNotBusted =
            players |> Map.forall (fun _ (_, h) -> Hand.IsBust h || h = hand)

        let probabilityToBust =
            Simulation.probabilityToBust deck discards hand onlyPlayerNotBusted
            |> fun p -> p * 100.0

        let emojiStatus = bustEmoji probabilityToBust

        let preamble =
            sprintf "%s %s (%dpts + %dpts?, %.2f%%): " playerName emojiStatus firmScore tentativeScore probabilityToBust

        handRows 40 preamble hand
        |> fun (top, mid, bot) ->
            let isHighlighted = cursor = Choice2Of3 playerName
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
        "[↔] distributions   [↕] players   [?] help   [q/esc] quit"
        |> centered 80
        |> styled [ Ansi.Dim; Ansi.Cyan ]
    |> printf "%s"

    let addCardToPlayer card player =
        let deck' = Deck.Decrement deck card
        let firmScore, hand = Map.find player players
        let hand' = card :: hand
        let players' = Map.add player (firmScore, hand') players
        loop deck' discards players' simulation cursor

    let popCardFromPlayer player =
        let firmScore, hand = Map.find player players
        match hand with
        | [] -> loop deck discards players simulation cursor
        | card :: rest ->
            let deck' = Deck.Increment deck card
            let players' = Map.add player (firmScore, rest) players
            loop deck' discards players' simulation cursor

    let commitSession () =
        if errors <> [] then
            loop deck discards players simulation cursor
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

        let cursor' = Choice1Of3(ValueCard Card.Zero)
        loop deck discards' players' simulation cursor'

    let cards = Deck.Empty |> Map.toList |> List.map fst
    let playerNames = players |> Map.keys |> Seq.toList
    let key = Console.ReadKey true

    match key.Modifiers, key.Key, cursor with
    // Program flow control
    | ConsoleModifiers.None, ConsoleKey.Q, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Escape, _ -> ()
    | ConsoleModifiers.None, ConsoleKey.Enter, _ -> commitSession ()

    // Opening the help page, remembering the cursor it was opened over
    | _, _, Choice1Of3 card when key.KeyChar = '?' || key.Key = ConsoleKey.H ->
        loop deck discards players simulation (Choice3Of3(Choice1Of2 card))
    | _, _, Choice2Of3 player when key.KeyChar = '?' || key.Key = ConsoleKey.H ->
        loop deck discards players simulation (Choice3Of3(Choice2Of2 player))

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
    | ConsoleModifiers.None, ConsoleKey.Add, Choice1Of3 card ->
        loop (Deck.Increment deck card) discards players simulation cursor
    | ConsoleModifiers.None, ConsoleKey.Subtract, Choice1Of3 card ->
        loop (Deck.Decrement deck card) discards players simulation cursor

    // Navigating through players and cards
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice1Of3 _card ->
        loop deck discards players simulation (Choice2Of3 playerNames[playerNames.Length - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice1Of3 _card ->
        loop deck discards players simulation (Choice2Of3 playerNames[playerNames.Length - playerNames.Length])
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of3 player when player = playerNames[0] ->
        loop deck discards players simulation (Choice1Of3(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of3 player when player = playerNames[playerNames.Length - 1] ->
        loop deck discards players simulation (Choice1Of3(ValueCard Card.Zero))
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of3 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players simulation (Choice2Of3 playerNames[index - 1])
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of3 player ->
        let index = playerNames |> List.findIndex (fun name -> name = player)
        loop deck discards players simulation (Choice2Of3 playerNames[index + 1])
    | ConsoleModifiers.None, ConsoleKey.LeftArrow, Choice1Of3 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index - 1 + cards.Length) % cards.Length
        loop deck discards players simulation (Choice1Of3 cards[index'])
    | ConsoleModifiers.None, ConsoleKey.RightArrow, Choice1Of3 card ->
        let index = cards |> List.findIndex (fun c -> c = card)
        let index' = (index + 1) % cards.Length
        loop deck discards players simulation (Choice1Of3 cards[index'])
    | _ -> loop deck discards players simulation cursor

let private runReplay (source: string) (timeline: Instant array) : int =
    Console.Clear()
    Console.CursorVisible <- false

    try
        Replay.Run source timeline
    finally
        Console.CursorVisible <- true
        Console.Clear()

    0

[<EntryPoint>]
let main args =
    match Array.toList args with
    // Scrub through a previously persisted timeline
    | [ "--replay"; directory ] -> Persistence.ReadTimeline directory |> Seq.toArray |> runReplay directory

    // Simulate a full game and scrub through it immediately
    | "--simulate" :: names when names.Length > 0 && names.Length <= 5 ->
        let strategies = [
            Strategy.Random
            HitUntilScore 25u
            HitUntilNumCards 4u
            RandomWithProbability 0.75
            AlwaysHits
        ]

        let players =
            names |> List.mapi (fun index name -> name, strategies[index % strategies.Length])

        Timeline.Simulate players None None None None
        |> Seq.toArray
        |> runReplay "simulated game"

    | _ ->

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
    let cursor = Choice1Of3(ValueCard Card.Zero)
    let players: Map<string, uint * Hand> =
        Array.map (fun name -> name, (0u, List.empty)) args |> Map.ofArray

    let simulation =
        Timeline.Simulate
            (players |> Map.toList |> List.map (fun (name, _) -> name, Strategy.Random))
            None
            (Some(players |> Map.map (fun _ (score, _) -> score)))
            None
            None

    Console.Clear()
    Console.CursorVisible <- false
    loop deck discards players simulation cursor
    Console.CursorVisible <- true
    Console.Clear()

    0
