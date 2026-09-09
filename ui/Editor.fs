/// The mid-game table editor: with the engine suspended at a prompt, the
/// deck, the discards, and every hand can be amended before the game forks
/// and plays on. Folding keys into the model is pure, so every editing rule
/// can be exercised on its own.
module public Editor

open System

open Flip7

let private width = 80

type public Cursor =
    | OnCard of Card
    | OnPlayer of string

type public Model = {
    Cursor: Cursor
    Help: bool
    Active: Strategy.StrategyPlayer list
    Finished: Strategy.StrategyPlayer list
    Deck: Deck
    Discards: Deck
}

let public Make
    (active: Strategy.StrategyPlayer list)
    (finished: Strategy.StrategyPlayer list)
    (deck: Deck)
    (discards: Deck)
    : Model = {
    Cursor = OnCard(ValueCard Card.Zero)
    Help = false
    Active = active
    Finished = finished
    Deck = deck
    Discards = discards
}

let private EveryCard: Card list = Deck.Empty |> Map.toList |> List.map fst

// An active player cannot be left busted, because the game's invariant is
// that busted players are finished
let public BustedActives (editor: Model) : string list =
    editor.Active
    |> List.filter (fun player -> Hand.IsBust player.Hand)
    |> List.map (fun player -> player.Name)

let private updateHand (name: string) (edit: Hand -> Hand) (editor: Model) : Model =
    let update =
        List.map (fun (player: Strategy.StrategyPlayer) ->
            if player.Name = name then
                { player with Hand = edit player.Hand }
            else
                player
        )

    {
        editor with
            Active = update editor.Active
            Finished = update editor.Finished
    }

// Dealing takes the card from the deck, and undoing returns it, so every
// edit preserves the accounting of all 94 cards by construction
let private deal (card: Card) (name: string) (editor: Model) : Model =
    if Map.find card editor.Deck > 0u then
        { editor with Deck = Deck.Decrement editor.Deck card }
        |> updateHand name (fun hand -> card :: hand)
    else
        editor

let private unDeal (name: string) (editor: Model) : Model =
    let hand =
        editor.Active @ editor.Finished
        |> List.pick (fun player -> if player.Name = name then Some player.Hand else None)

    match hand with
    | [] -> editor
    | card :: rest ->
        { editor with Deck = Deck.Increment editor.Deck card }
        |> updateHand name (fun _ -> rest)

// Folds one key into the editor, or None to apply the edits; pure
let public Key (key: ConsoleKeyInfo) (editor: Model) : Model option =
    let names =
        editor.Active @ editor.Finished |> List.map (fun player -> player.Name)

    if editor.Help then
        // Any key returns from the help page
        Some { editor with Help = false }
    else

    match key.Modifiers, key.Key, editor.Cursor with
    | ConsoleModifiers.None, ConsoleKey.Enter, _
    | ConsoleModifiers.None, ConsoleKey.Escape, _ -> None

    // The help page
    | _, _, _ when key.KeyChar = '?' || key.Key = ConsoleKey.H -> Some { editor with Help = true }

    // Dealing cards from the deck into the highlighted hand
    | ConsoleModifiers.None, ConsoleKey.D0, OnPlayer name -> Some(deal (ValueCard Card.Zero) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D1, OnPlayer name -> Some(deal (ModifierCard Card.Double) name editor)
    | ConsoleModifiers.None, ConsoleKey.D1, OnPlayer name -> Some(deal (ValueCard Card.One) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D2, OnPlayer name -> Some(deal (ModifierCard Card.Plus2) name editor)
    | ConsoleModifiers.None, ConsoleKey.D2, OnPlayer name -> Some(deal (ValueCard Card.Two) name editor)
    | ConsoleModifiers.None, ConsoleKey.D3, OnPlayer name -> Some(deal (ValueCard Card.Three) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D4, OnPlayer name -> Some(deal (ModifierCard Card.Plus4) name editor)
    | ConsoleModifiers.None, ConsoleKey.D4, OnPlayer name -> Some(deal (ValueCard Card.Four) name editor)
    | ConsoleModifiers.None, ConsoleKey.D5, OnPlayer name -> Some(deal (ValueCard Card.Five) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D6, OnPlayer name -> Some(deal (ModifierCard Card.Plus6) name editor)
    | ConsoleModifiers.None, ConsoleKey.D6, OnPlayer name -> Some(deal (ValueCard Card.Six) name editor)
    | ConsoleModifiers.None, ConsoleKey.D7, OnPlayer name -> Some(deal (ValueCard Card.Seven) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D8, OnPlayer name -> Some(deal (ModifierCard Card.Plus8) name editor)
    | ConsoleModifiers.None, ConsoleKey.D8, OnPlayer name -> Some(deal (ValueCard Card.Eight) name editor)
    | ConsoleModifiers.None, ConsoleKey.D9, OnPlayer name -> Some(deal (ValueCard Card.Nine) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.X, OnPlayer name -> Some(deal (ModifierCard Card.Plus10) name editor)
    | ConsoleModifiers.None, ConsoleKey.X, OnPlayer name -> Some(deal (ValueCard Card.Ten) name editor)
    | ConsoleModifiers.None, ConsoleKey.E, OnPlayer name -> Some(deal (ValueCard Card.Eleven) name editor)
    | ConsoleModifiers.None, ConsoleKey.T, OnPlayer name -> Some(deal (ValueCard Card.Twelve) name editor)
    | ConsoleModifiers.None, ConsoleKey.S, OnPlayer name -> Some(deal (ActionCard Card.SecondChance) name editor)
    | ConsoleModifiers.None, ConsoleKey.D, OnPlayer name -> Some(deal (ActionCard Card.Deal3) name editor)
    | ConsoleModifiers.None, ConsoleKey.F, OnPlayer name -> Some(deal (ActionCard Card.Freeze) name editor)

    // Returning the most recent card of the highlighted hand
    | ConsoleModifiers.None, ConsoleKey.Backspace, OnPlayer name -> Some(unDeal name editor)

    // Moving copies of the highlighted card between discards and deck
    | ConsoleModifiers.None, ConsoleKey.Add, OnCard card when Map.find card editor.Discards > 0u ->
        Some {
            editor with
                Deck = Deck.Increment editor.Deck card
                Discards = Deck.Decrement editor.Discards card
        }
    | ConsoleModifiers.None, ConsoleKey.Subtract, OnCard card when Map.find card editor.Deck > 0u ->
        Some {
            editor with
                Deck = Deck.Decrement editor.Deck card
                Discards = Deck.Increment editor.Discards card
        }

    // Rotating the cursor through the distributions and the players
    | ConsoleModifiers.None, ConsoleKey.UpArrow, OnCard _ -> Some { editor with Cursor = OnPlayer(List.last names) }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, OnCard _ -> Some { editor with Cursor = OnPlayer(List.head names) }
    | ConsoleModifiers.None, ConsoleKey.UpArrow, OnPlayer name when name = List.head names ->
        Some { editor with Cursor = OnCard(ValueCard Card.Zero) }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, OnPlayer name when name = List.last names ->
        Some { editor with Cursor = OnCard(ValueCard Card.Zero) }
    | ConsoleModifiers.None, ConsoleKey.UpArrow, OnPlayer name ->
        let index = names |> List.findIndex ((=) name)
        Some { editor with Cursor = OnPlayer names[index - 1] }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, OnPlayer name ->
        let index = names |> List.findIndex ((=) name)
        Some { editor with Cursor = OnPlayer names[index + 1] }
    | ConsoleModifiers.None, ConsoleKey.LeftArrow, OnCard card ->
        let index = EveryCard |> List.findIndex ((=) card)
        let index' = (index - 1 + EveryCard.Length) % EveryCard.Length
        Some { editor with Cursor = OnCard EveryCard[index'] }
    | ConsoleModifiers.None, ConsoleKey.RightArrow, OnCard card ->
        let index = EveryCard |> List.findIndex ((=) card)
        let index' = (index + 1) % EveryCard.Length
        Some { editor with Cursor = OnCard EveryCard[index'] }
    | _ -> Some editor

let public RenderHelp () : unit =
    let rule = String.replicate width "─"

    let entry (keysText: string) (description: string) =
        sprintf "   %s%s" (keysText.PadRight 18) description

    let section (title: string) (annotation: string) =
        styled [ Ansi.Bright ] $" {title}" + styled [ Ansi.Dim ] $"  - {annotation}"

    Console.Clear()

    [
        rule
        "edit help" |> centered width
        rule
        styled [ Ansi.Bright ] " cursor"
        entry "↑/↓" "rotate between the distributions and each player"
        entry "←/→" "move along the distributions (wraps around)"
        ""
        section "deck" "with the cursor on the distributions"
        entry "+" "move a copy of the highlighted card from the discards to the deck"
        entry "-" "move a copy of the highlighted card from the deck to the discards"
        ""
        section "hands" "with the cursor on a player"
        entry "0-9" "value card 0-9, dealt from the deck"
        entry "x  e  t" "value card 10, 11, 12"
        entry "shift+2/4/6/8" "modifier card +2, +4, +6, +8"
        entry "shift+1  shift+x" "modifier card x2, +10"
        entry "s  d  f" "action card SecondChance, Deal3, Freeze"
        entry "backspace" "return the player's most recent card to the deck"
        ""
        styled [ Ansi.Bright ] " program"
        entry "enter or esc" "apply the edits and return to the game"
        "[any key] back to the editor"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]
    ]
    |> String.concat "\n"
    |> printf "%s"

let public Render (editor: Model) : unit =
    Console.Clear()

    let cursorCard =
        match editor.Cursor with
        | OnCard card -> Some card
        | OnPlayer _ -> None

    let pdf = editor.Deck |> Deck.pdf
    let cdf = editor.Deck |> Deck.cdf
    let gap = String.replicate 4 " "
    let pdf3 = pdf |> Map.toList |> normalizeDistribution |> sparkline 3 cursorCard
    let cdf3 = cdf |> Map.toList |> normalizeDistribution |> sparkline 3 cursorCard

    let ec = (sprintf "ec:     %s" (Deck.ec editor.Deck |> string)).PadRight 20
    let ev = (sprintf "ev:     %.2f" (Deck.ev editor.Deck)).PadRight 20
    let var = (sprintf "var:    %.2f" (Deck.var editor.Deck)).PadRight 20
    let std = (sprintf "std:    %.2f" (Deck.std editor.Deck)).PadRight 20

    let pdfTitle =
        match editor.Cursor with
        | OnCard card -> sprintf "p(%s)=%.2f%%" (string card) (Map.find card pdf * 100.0)
        | _ -> "pdf"
        |> centered (pdf |> Map.keys |> Seq.length)

    let cdfTitle =
        match editor.Cursor with
        | OnCard card -> sprintf "P(%s)=%.2f%%" (string card) (Map.find card cdf * 100.0)
        | _ -> "cdf"
        |> centered (cdf |> Map.keys |> Seq.length)

    printfn "%s" (String.replicate width "─")
    printfn "%s" (ec + pdf3[0] + gap + cdf3[0])
    printfn "%s" (ev + pdf3[1] + gap + cdf3[1])
    printfn "%s" (var + pdf3[2] + gap + cdf3[2])
    printfn "%s" (std + pdfTitle + gap + cdfTitle)
    printfn "%s" (String.replicate width "─")

    let renderPlayer (isFinished: bool) (player: Strategy.StrategyPlayer) =
        let probabilityToBust =
            Simulation.probabilityToBust editor.Deck editor.Discards player.Hand false
            * 100.0

        playerRow
            probabilityToBust
            (editor.Cursor = OnPlayer player.Name)
            (isFinished || Hand.IsBust player.Hand)
            (if isFinished then " (done)" else "")
            player
        |> printfn "%s"

    for player in editor.Active do
        renderPlayer false player

    for player in editor.Finished do
        renderPlayer true player

    match BustedActives editor with
    | busted when not (List.isEmpty busted) ->
        $"""{busted |> String.concat ", "} cannot stay busted while still in the round"""
        |> centered width
        |> styled [ Ansi.BrightRed ]
    | _ ->
        "[↕↔] cursor   [+/-] deck   [cards] deal   [backspace] undo   [?] help   [enter] resume"
        |> centered width
        |> styled [ Ansi.Dim; Ansi.Cyan ]
    |> printf "%s"
