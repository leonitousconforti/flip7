module public Editor

open System

open FSharp.Control

open Flip7

type public Model = {
    Cursor: Choice<Card, string>
    Help: bool
    Round: uint
    Turn: uint
    Seating: string list option
    Initial: Player list * Player list * Deck * Deck
    Active: Player list
    Finished: Player list
    Deck: Deck
    Discards: Deck
}

let public Make
    (round: uint)
    (turn: uint)
    (seating: string list option)
    (active: Player list)
    (finished: Player list)
    (deck: Deck)
    (discards: Deck)
    : Model = {
    Cursor = Choice1Of2(ValueCard Card.Zero)
    Help = false
    Round = round
    Turn = turn
    Seating = seating
    Initial = active, finished, deck, discards
    Active = active
    Finished = finished
    Deck = deck
    Discards = discards
}

type public Splice = {
    Round: uint
    Turn: uint
    Active: Player list
    Finished: Player list
    Deck: Deck
    Discards: Deck
}

let private spliceOf (edited: Model) : Splice = {
    Round = edited.Round
    Turn = edited.Turn
    Active = edited.Active
    Finished = edited.Finished
    Deck = edited.Deck
    Discards = edited.Discards
}

type public EditException(splice: Splice) =
    inherit Exception()
    member _.Splice = splice

type public Outcome =
    | Cancelled
    | Editing of Model
    | Committed of Splice

let private EveryCard: Card list = Deck.Empty |> Map.toList |> List.map fst

let private updateHand (name: string) (edit: Hand -> Hand) (editor: Model) : Model =
    let update =
        List.map (fun (player: Player) ->
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

let private seated (seating: string list option) (players: Player list) : Player list =
    match seating with
    | None -> players
    | Some seating ->
        seating
        |> List.choose (fun name -> players |> List.tryFind (fun player -> player.Name = name))

let public Key (key: ConsoleKeyInfo) (editor: Model) : Outcome =
    let names =
        editor.Active @ editor.Finished
        |> seated editor.Seating
        |> List.map (fun player -> player.Name)

    if editor.Help then
        Editing { editor with Help = false }
    else

    match key.Modifiers, key.Key, editor.Cursor with
    | ConsoleModifiers.None, (ConsoleKey.Enter | ConsoleKey.Escape), _ ->
        if editor.Active |> List.exists (fun player -> Hand.IsBust player.Hand) then
            Editing editor
        elif (editor.Active, editor.Finished, editor.Deck, editor.Discards) = editor.Initial then
            Cancelled
        else
            Committed(spliceOf editor)
    | _, _, _ when key.KeyChar = '?' || key.Key = ConsoleKey.H -> Editing { editor with Help = true }

    // Dealing cards from the deck into the highlighted hand
    | ConsoleModifiers.None, ConsoleKey.D0, Choice2Of2 name -> Editing(deal (ValueCard Card.Zero) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D1, Choice2Of2 name -> Editing(deal (ModifierCard Card.Double) name editor)
    | ConsoleModifiers.None, ConsoleKey.D1, Choice2Of2 name -> Editing(deal (ValueCard Card.One) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D2, Choice2Of2 name -> Editing(deal (ModifierCard Card.Plus2) name editor)
    | ConsoleModifiers.None, ConsoleKey.D2, Choice2Of2 name -> Editing(deal (ValueCard Card.Two) name editor)
    | ConsoleModifiers.None, ConsoleKey.D3, Choice2Of2 name -> Editing(deal (ValueCard Card.Three) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D4, Choice2Of2 name -> Editing(deal (ModifierCard Card.Plus4) name editor)
    | ConsoleModifiers.None, ConsoleKey.D4, Choice2Of2 name -> Editing(deal (ValueCard Card.Four) name editor)
    | ConsoleModifiers.None, ConsoleKey.D5, Choice2Of2 name -> Editing(deal (ValueCard Card.Five) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D6, Choice2Of2 name -> Editing(deal (ModifierCard Card.Plus6) name editor)
    | ConsoleModifiers.None, ConsoleKey.D6, Choice2Of2 name -> Editing(deal (ValueCard Card.Six) name editor)
    | ConsoleModifiers.None, ConsoleKey.D7, Choice2Of2 name -> Editing(deal (ValueCard Card.Seven) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.D8, Choice2Of2 name -> Editing(deal (ModifierCard Card.Plus8) name editor)
    | ConsoleModifiers.None, ConsoleKey.D8, Choice2Of2 name -> Editing(deal (ValueCard Card.Eight) name editor)
    | ConsoleModifiers.None, ConsoleKey.D9, Choice2Of2 name -> Editing(deal (ValueCard Card.Nine) name editor)
    | ConsoleModifiers.Shift, ConsoleKey.X, Choice2Of2 name -> Editing(deal (ModifierCard Card.Plus10) name editor)
    | ConsoleModifiers.None, ConsoleKey.X, Choice2Of2 name -> Editing(deal (ValueCard Card.Ten) name editor)
    | ConsoleModifiers.None, ConsoleKey.E, Choice2Of2 name -> Editing(deal (ValueCard Card.Eleven) name editor)
    | ConsoleModifiers.None, ConsoleKey.T, Choice2Of2 name -> Editing(deal (ValueCard Card.Twelve) name editor)
    | ConsoleModifiers.None, ConsoleKey.S, Choice2Of2 name -> Editing(deal (ActionCard Card.SecondChance) name editor)
    | ConsoleModifiers.None, ConsoleKey.D, Choice2Of2 name -> Editing(deal (ActionCard Card.Deal3) name editor)
    | ConsoleModifiers.None, ConsoleKey.F, Choice2Of2 name -> Editing(deal (ActionCard Card.Freeze) name editor)

    // Returning the most recent card of the highlighted hand
    | ConsoleModifiers.None, ConsoleKey.Backspace, Choice2Of2 name -> Editing(unDeal name editor)

    // Moving copies of the highlighted card between discards and deck
    | ConsoleModifiers.None, ConsoleKey.Add, Choice1Of2 card when Map.find card editor.Discards > 0u ->
        Editing {
            editor with
                Deck = Deck.Increment editor.Deck card
                Discards = Deck.Decrement editor.Discards card
        }
    | ConsoleModifiers.None, ConsoleKey.Subtract, Choice1Of2 card when Map.find card editor.Deck > 0u ->
        Editing {
            editor with
                Deck = Deck.Decrement editor.Deck card
                Discards = Deck.Increment editor.Discards card
        }

    // Rotating the cursor through the distributions and the players
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice1Of2 _ ->
        Editing { editor with Cursor = Choice2Of2(List.last names) }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice1Of2 _ ->
        Editing { editor with Cursor = Choice2Of2(List.head names) }
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of2 name when name = List.head names ->
        Editing { editor with Cursor = Choice1Of2(ValueCard Card.Zero) }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of2 name when name = List.last names ->
        Editing { editor with Cursor = Choice1Of2(ValueCard Card.Zero) }
    | ConsoleModifiers.None, ConsoleKey.UpArrow, Choice2Of2 name ->
        let index = names |> List.findIndex ((=) name)
        Editing { editor with Cursor = Choice2Of2 names[index - 1] }
    | ConsoleModifiers.None, ConsoleKey.DownArrow, Choice2Of2 name ->
        let index = names |> List.findIndex ((=) name)
        Editing { editor with Cursor = Choice2Of2 names[index + 1] }
    | ConsoleModifiers.None, ConsoleKey.LeftArrow, Choice1Of2 card ->
        let index = EveryCard |> List.findIndex ((=) card)
        let index' = (index - 1 + EveryCard.Length) % EveryCard.Length
        Editing { editor with Cursor = Choice1Of2 EveryCard[index'] }
    | ConsoleModifiers.None, ConsoleKey.RightArrow, Choice1Of2 card ->
        let index = EveryCard |> List.findIndex ((=) card)
        let index' = (index + 1) % EveryCard.Length
        Editing { editor with Cursor = Choice1Of2 EveryCard[index'] }
    | _ -> Editing editor

let public Fork (random: Random) (decide: Strategy.Decider) (splice: Splice) : Timeline =
    let turnsTaken =
        splice.Active
        |> List.map (fun player -> player.Name, splice.Turn - 1u)
        |> Map.ofList

    asyncSeq {
        yield {
            Event = Edited (List.head splice.Active).Name
            Players = splice.Active @ splice.Finished
            Deck = splice.Deck
            Discards = splice.Discards
        }

        yield!
            Timeline.ContinueWith
                random
                decide
                splice.Round
                turnsTaken
                splice.Active
                splice.Finished
                (splice.Deck, splice.Discards)
    }

let public RenderHelp () : unit =
    let rule = String.replicate 80 "─"

    let entry (keysText: string) (description: string) =
        sprintf "   %s%s" (keysText.PadRight 18) description

    let section (title: string) (annotation: string) =
        styled [ Ansi.Bright ] $" {title}" + styled [ Ansi.Dim ] $"  - {annotation}"

    Console.Clear()

    [
        rule
        "edit help" |> centered 80
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
        "[any key] back to the editor" |> centered 80 |> styled [ Ansi.Dim; Ansi.Cyan ]
    ]
    |> String.concat "\n"
    |> printf "%s"

let public Render (editor: Model) : unit =
    Console.Clear()

    let cursorCard =
        match editor.Cursor with
        | Choice1Of2 card -> Some card
        | Choice2Of2 _ -> None

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
        | Choice1Of2 card -> sprintf "p(%s)=%.2f%%" (string card) (Map.find card pdf * 100.0)
        | _ -> "pdf"
        |> centered (pdf |> Map.keys |> Seq.length)

    let cdfTitle =
        match editor.Cursor with
        | Choice1Of2 card -> sprintf "P(%s)=%.2f%%" (string card) (Map.find card cdf * 100.0)
        | _ -> "cdf"
        |> centered (cdf |> Map.keys |> Seq.length)

    let caption = std + pdfTitle + gap + cdfTitle
    let status =
        [
            ec + pdf3[0] + gap + cdf3[0]
            ev + pdf3[1] + gap + cdf3[1]
            var + pdf3[2] + gap + cdf3[2]
        ]
        |> String.concat "\n"

    let renderPlayer (isFinished: bool) (player: Player) =
        let probabilityToBust =
            Simulation.probabilityToBust editor.Deck editor.Discards player.Hand false
            * 100.0

        playerRow
            probabilityToBust
            (editor.Cursor = Choice2Of2 player.Name)
            (isFinished || Hand.IsBust player.Hand)
            (if isFinished then " (done)" else "")
            player

    let finishedNames = editor.Finished |> List.map (fun player -> player.Name)
    let content = [
        for player in editor.Active @ editor.Finished |> seated editor.Seating do
            yield renderPlayer (finishedNames |> List.contains player.Name) player
    ]

    let bottom = ""
    let footer =
        match
            editor.Active
            |> List.filter (fun player -> Hand.IsBust player.Hand)
            |> List.map (fun player -> player.Name)
        with
        | busted when not (List.isEmpty busted) ->
            $"""{busted |> String.concat ", "} cannot stay busted while still in the round"""
            |> centered 80
            |> styled [ Ansi.BrightRed ]
        | _ ->
            "[↕↔] cursor  [+/-] deck  [cards] deal  [bksp] undo  [?] help  [enter] resume"
            |> centered 80
            |> styled [ Ansi.Dim; Ansi.Cyan ]

    Render.Frame status caption content bottom footer
