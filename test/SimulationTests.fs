module SimulationTests

open Xunit
open Flip7
open Support

// ---- probabilityToBust ----

[<Fact>]
let ``a deal3 adds to the bust chance only for the last player standing, and a freeze never does`` () =
    let deck =
        deckOf [
            ActionCard Card.Freeze
            ActionCard Card.Deal3
            ValueCard Card.One
            ValueCard Card.Two
        ]
    let hand = [ ValueCard Card.One ]

    // With others still in, only drawing the other One busts
    Assert.Equal(0.25, Simulation.probabilityToBust deck Deck.Empty hand false, 1e-9)

    // Alone, a deal3 must be dealt to ourselves, and its three flips take every
    // card left, the One among them, so its quarter adds to the duplicate's. A
    // freeze would bank our points instead, which is not a bust
    Assert.Equal(0.25 + 0.25, Simulation.probabilityToBust deck Deck.Empty hand true, 1e-9)

[<Fact>]
let ``a held second chance leaves no bust chance, though every card left is a duplicate`` () =
    // A game holds twelve Twelves, so this table can really happen
    let deck = deckOf [ ValueCard Card.Twelve; ValueCard Card.Twelve ]
    let hand = [ ActionCard Card.SecondChance; ValueCard Card.Twelve ]

    // Asked with others still in on purpose: for a lone player the code also
    // lets one second chance cover all three flips of a deal3, an
    // approximation this test does not mean to pin
    Assert.Equal(0.0, Simulation.probabilityToBust deck Deck.Empty hand false, 1e-9)

[<Fact>]
let ``a held action card is not a bust target`` () =
    // The play editor can deal a Freeze into a hand, and drawing another is
    // resolved as an action, never a bust. A modifier cannot show this,
    // because a game holds one of each, so a held one is never left to draw
    let deck = deckOf [ ActionCard Card.Freeze; ValueCard Card.Two ]
    let hand = [ ActionCard Card.Freeze; ValueCard Card.Five ]
    Assert.Equal(0.0, Simulation.probabilityToBust deck Deck.Empty hand false, 1e-9)

[<Fact>]
let ``a deal3 that outlasts the deck flips the rest of its cards from the reshuffled discards, and never past certainty``
    ()
    =
    // One flip takes the Two and the other two take both reshuffled discards,
    // the One among them, so a drawn deal3 busts the lone player for certain
    let hand = [ ValueCard Card.One ]
    let deck = deckOf [ ActionCard Card.Deal3; ValueCard Card.Two ]
    let discards = deckOf [ ValueCard Card.One; ValueCard Card.Three ]
    Assert.Equal(0.5, Simulation.probabilityToBust deck discards hand true, 1e-9)
    Assert.Equal(0.0, Simulation.probabilityToBust deck discards hand false, 1e-9)

    // A duplicate is three draws in four, and each of a deal3's three flips
    // would bust on its own, which caps the deal3 term at certainty
    let crowded =
        deckOf [
            ActionCard Card.Deal3
            ValueCard Card.Twelve
            ValueCard Card.Twelve
            ValueCard Card.Eleven
        ]
    let held = [ ValueCard Card.Twelve; ValueCard Card.Eleven ]
    Assert.Equal(0.75 + 0.25, Simulation.probabilityToBust crowded Deck.Empty held true, 1e-9)

[<Fact>]
let ``the bust chance with an empty deck is read from the discards it will reshuffle`` () =
    // The last card has just been dealt, so the next comes from the reshuffled
    // discards, half the time the One the hand holds
    let discards = deckOf [ ValueCard Card.One; ValueCard Card.Two ]
    Assert.Equal(0.5, Simulation.probabilityToBust Deck.Empty discards [ ValueCard Card.One ] false, 1e-9)

// ---- expectedValueOfHit ----

[<Fact>]
let ``the expected value of a hit weighs busts against gains`` () =
    // Half the time bust and lose the 1, half the time draw the Two
    let deck = deckOf [ ValueCard Card.One; ValueCard Card.Two ]
    Assert.Equal(0.5 * -1.0 + 0.5 * 2.0, Simulation.expectedValueOfHit deck Deck.Empty [ ValueCard Card.One ], 1e-9)

[<Fact>]
let ``the expected value of a hit with an empty deck is read from the discards it will reshuffle`` () =
    // The next card is the reshuffled Two, which the hand gains in full
    let discards = deckOf [ ValueCard Card.Two ]
    Assert.Equal(2.0, Simulation.expectedValueOfHit Deck.Empty discards [ ValueCard Card.One ], 1e-9)

[<Fact>]
let ``the expected value of a hit counts the flip7 bonus`` () =
    // The guaranteed Seven completes the flip7: One to Seven score 28, the
    // bonus is 15, and the hand scores 21 now
    let hand = values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
    let deck = deckOf [ ValueCard Card.Seven ]
    Assert.Equal(28.0 + 15.0 - 21.0, Simulation.expectedValueOfHit deck Deck.Empty hand, 1e-9)

// ---- Issues ----

[<Fact>]
let ``Issues names a card that went missing, a card counted twice, and a hand holding two second chances`` () =
    Assert.Empty(Simulation.Issues Deck.Full Deck.Empty [])

    // The cards are ones whose names cannot be mistaken for the counts the
    // messages also give. A Freeze left the deck without reaching a hand or
    // the discards, and a +4 was dealt without leaving the deck
    let missing =
        Simulation.Issues (Deck.Decrement Deck.Full (ActionCard Card.Freeze)) Deck.Empty []
    Assert.Contains("Freeze", Assert.Single missing)

    let countedTwice =
        Simulation.Issues Deck.Full Deck.Empty [ [ ModifierCard Card.Plus4 ] ]
    Assert.Contains("+4", Assert.Single countedTwice)

    // The engine passes a second SecondChance on, so no hand may hold two.
    // The hand sits behind an empty one, so the issue has to name its own seat
    let dealt = [
        ActionCard Card.SecondChance
        ActionCard Card.SecondChance
        ValueCard Card.Five
    ]
    let doubled =
        Simulation.Issues (List.fold Deck.Decrement Deck.Full dealt) Deck.Empty [ []; dealt ]
    Assert.Contains("Player 1", Assert.Single doubled)
