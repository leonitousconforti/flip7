module DeckTests

open System
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// ---- composition ----

[<Fact>]
let ``the full deck holds n copies of each value card n, one Zero, one of each modifier and three of each action`` () =
    let copies (card: Card) =
        match card with
        | ValueCard _ -> max card.Value.ValuePoints 1u
        | ModifierCard _ -> 1u
        | ActionCard _ -> 3u

    Assert.Equal<Deck>(Deck.Full |> Map.map (fun card _ -> copies card), Deck.Full)
    // Every card is a key of both, so looking a card up is safe in either
    Assert.Equal<Set<Card>>(set Deck.Full.Keys, set Deck.Empty.Keys)
    // 79 value cards (the Zero and 1 + 2 + ... + 12 = 78 more), 6 modifiers and
    // 9 actions
    Assert.Equal(94I, Deck.Count Deck.Full)

// ---- IsEmpty ----

[<Fact>]
let ``a deck is empty only when no card is left`` () =
    Assert.True(Deck.IsEmpty Deck.Empty)
    Assert.False(Deck.IsEmpty Deck.Full)
    // One card among zeros tells "no card left" apart from "some card missing"
    Assert.False(Deck.IsEmpty(deckOf [ ValueCard Card.Five ]))

// ---- draws ----

[<Fact>]
let ``a draw takes its cards from the deck and leaves the discards alone`` () =
    // A few cards already played sit in the discards, so a draw that took from
    // them, or emptied them, would show
    let played = [ ValueCard Card.Twelve; ModifierCard Card.Plus4; ActionCard Card.Freeze ]
    let deck = List.fold Deck.Decrement Deck.Full played
    let discards = deckOf played

    // Draw1With hands back its card on its own rather than in a list
    let afterOne, card = Deck.Draw1With (Random 1) (deck, discards)

    let draws = [
        1, (afterOne, [ card ])
        3, Deck.Draw3With (Random 1) (deck, discards)
        5, Deck.DrawWith (Random 1) (deck, discards) 5u
    ]

    for count, ((deck', discards'), cards) in draws do
        Assert.Equal(count, cards.Length)
        // Putting the drawn cards back restores the deck exactly
        Assert.Equal<Deck>(deck, List.fold Deck.Increment deck' cards)
        Assert.Equal<Deck>(discards, discards')

[<Fact>]
let ``an empty deck takes the discards in as the new deck`` () =
    let five = ValueCard Card.Five

    let (deck', discards'), cards =
        Deck.DrawWith (Random 1) (deckOf [ five ], Deck.Decrement Deck.Full five) 3u

    // The deck's only card comes first, and the other two come from the 93
    // discards once they are shuffled in as the new deck
    Assert.Equal(3, cards.Length)
    Assert.Equal(five, List.head cards)
    Assert.True(Deck.IsEmpty discards')
    Assert.Equal(91I, Deck.Count deck')

[<Fact>]
let ``drawing with every card in a hand fails rather than hangs`` () =
    // Two empty piles used to be shuffled into each other forever, so the draw
    // runs on a task of its own and a regression times out instead of hanging
    // the suite
    let draw =
        Threading.Tasks.Task.Run(fun () -> Deck.Draw1With (Random 1) (Deck.Empty, Deck.Empty))

    let error =
        Assert.Throws<InvalidOperationException>(fun () ->
            draw.WaitAsync(TimeSpan.FromSeconds 10.0).GetAwaiter().GetResult() |> ignore
        )

    Assert.Contains("the deck and the discards are both empty", error.Message)

// The deck and the discards pooled into one count per card
let private pooled (deck: Deck, discards: Deck) : Deck =
    deck |> Map.map (fun card count -> count + Map.find card discards)

[<Fact>]
let ``a draw conserves every card and touches the discards only to reshuffle them in`` () =
    // A fifth of the decks are empty, so FsCheck's default hundred cases take
    // the reshuffle about twenty times
    let decks = Gen.frequency [ 1, Gen.constant Deck.Empty; 4, genDeck ]

    Prop.forAll
        (Arb.fromGen (Gen.zip genSeed decks))
        (fun (seed, deck) ->
            // The discards complete the deck to the full 94 cards, so the pair
            // can never run out mid-draw
            let discards = Deck.Full |> Map.map (fun card count -> count - Map.find card deck)

            let (deck', discards'), drawn = Deck.Draw1With (Random seed) (deck, discards)

            let conserved =
                Deck.Increment (pooled (deck', discards')) drawn = pooled (deck, discards)

            // A reshuffle takes every discard in, and a plain draw leaves them be
            let expectedDiscards = if Deck.IsEmpty deck then Deck.Empty else discards

            Prop.label "the drawn card is the only one to leave the deck and the discards" conserved
            .&. Prop.label
                    "a reshuffle empties the discards, and a plain draw leaves them as they were"
                    (discards' = expectedDiscards)
        )
    |> Check.QuickThrowOnFailure

// ---- saved format ----

[<Fact>]
let ``a deck round-trips through serialization`` () =
    // A round trip is cheap, and FsCheck's default hundred decks vary every
    // card's count
    Prop.forAll (Arb.fromGen genDeck) (fun deck -> Deck.Deserialize(Deck.Serialize deck) = deck)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``a deck is saved as one 'card: count' line per card`` () =
    // Timelines saved by earlier builds have to keep reading, and a change made
    // to both directions at once passes every round trip, so the lines are
    // pinned here: one per kind of card, in Card's order, actions first and
    // values last
    let lines = Deck.Serialize Deck.Full

    Assert.Equal(22, lines.Length)
    Assert.Equal<string array>([| "Deal3: 3"; "Freeze: 3"; "SecondChance: 3" |], Array.take 3 lines)
    Assert.Equal("12: 12", Array.last lines)

[<Fact>]
let ``a deck that is not exactly the lines Serialize writes is None rather than an exception`` () =
    // Each bad input is the full deck's lines with one thing wrong, most of
    // them in the first line, which is "Deal3: 3"
    let full = Deck.Serialize Deck.Full
    let withFirst line = Array.updateAt 0 line full

    let badInputs = [
        "no lines", [||]
        "a line short", Array.tail full
        "a blank line past the last card", Array.append full [| "" |]
        "a card repeated past the last card", Array.append full [| "Deal3: 3" |]
        "a card listed twice and another missing", withFirst "Freeze: 3"
        "a blank line in place of a card", withFirst ""
        "a null line in place of a card", withFirst null
        "no separator", withFirst "Deal3 3"
        "no space after the colon", withFirst "Deal3:3"
        "a second separator", withFirst "Deal3: 3: 3"
        "an unknown card", withFirst "Deal4: 3"
        "a card in the wrong case", withFirst "deal3: 3"
        "no count", withFirst "Deal3: "
        "a count in words", withFirst "Deal3: three"
        "a fractional count", withFirst "Deal3: 1.5"
        "a negative count", withFirst "Deal3: -1"
        "a count past uint's range", withFirst "Deal3: 99999999999"
        "a count with a sign", withFirst "Deal3: +3"
        "a count padded with a space", withFirst "Deal3:  3"
        "a count with a leading zero", withFirst "Deal3: 03"
        "more copies than a full deck holds", withFirst "Deal3: 4"
    ]

    // An exception is reported under its label like a deck that read, so one
    // bad input that raises does not hide the rest
    let wrong = [
        for label, lines in badInputs do
            try
                if Option.isSome (Deck.TryDeserialize lines) then
                    yield $"{label}: read as a deck"
            with error ->
                yield $"{label}: raised {error.GetType().Name}"
    ]

    Assert.True(wrong.IsEmpty, String.concat "\n" wrong)

// ---- pdf ----

[<Fact>]
let ``a card's probability is its share of the deck`` () =
    let one, three = ValueCard Card.One, ValueCard Card.Three
    let pdf = Deck.pdf (deckOf [ one; three; three; three ])

    Assert.Equal(0.25, pdf[one], 1e-9)
    Assert.Equal(0.75, pdf[three], 1e-9)
    // Every other card, the actions and modifiers included, has no share
    Assert.Empty(
        pdf
        |> Map.toList
        |> List.filter (fun (card, probability) -> card <> one && card <> three && probability <> 0.0)
    )

[<Fact>]
let ``an empty deck gives every card a probability of zero rather than NaN`` () =
    // Dividing by the empty deck's count of zero would give NaN for every card
    Assert.Equal<Map<Card, float>>(Map.map (fun _ _ -> 0.0) Deck.Empty, Deck.pdf Deck.Empty)

// ---- cdf ----

[<Fact>]
let ``a card's cumulative probability counts every card ordered before it, actions and modifiers first`` () =
    let cdf =
        Deck.cdf (
            deckOf [
                ActionCard Card.Freeze
                ValueCard Card.One
                ValueCard Card.Three
                ValueCard Card.Three
            ]
        )

    // The Freeze is a quarter of the deck and is ordered ahead of every
    // modifier and value card, so each of them counts it
    Assert.Equal(0.0, cdf[ActionCard Card.Deal3], 1e-9)
    Assert.Equal(0.25, cdf[ActionCard Card.Freeze], 1e-9)
    Assert.Equal(0.25, cdf[ModifierCard Card.Plus2], 1e-9)
    Assert.Equal(0.25, cdf[ValueCard Card.Zero], 1e-9)
    Assert.Equal(0.5, cdf[ValueCard Card.One], 1e-9)
    Assert.Equal(0.5, cdf[ValueCard Card.Two], 1e-9)
    Assert.Equal(1.0, cdf[ValueCard Card.Three], 1e-9)
    Assert.Equal(1.0, cdf[ValueCard Card.Twelve], 1e-9)

// ---- statistics ----

[<Fact>]
let ``a draw from the full deck is likeliest to be a Twelve and is worth 680/94 points on average`` () =
    // There are more Twelves than copies of any other card
    Assert.Equal(ValueCard Card.Twelve, Deck.ec Deck.Full)
    // The n copies of each value card n give 1 + 4 + ... + 144 = 650 points
    // and the +2 to +10 modifiers 30 more, while a Double and the actions count
    // no points
    Assert.Equal(680.0 / 94.0, Deck.ev Deck.Full, 1e-9)
    // The squared points sum to 6084 over the value cards (the cubes of 1 to
    // 12) and 220 over the modifiers, and the variance is their mean less the
    // square of the mean
    Assert.Equal(sqrt (6304.0 / 94.0 - (680.0 / 94.0) ** 2.0), Deck.std Deck.Full, 1e-9)
