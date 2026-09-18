module LookaheadTests

open Xunit
open Flip7

// The within-round search is exact, so it can be checked against arithmetic
// rather than against a tolerance
[<Fact>]
let ``One card of sight is exactly what expected value already sees`` () =
    let hands = [
        []
        [ ValueCard Card.Five ]
        [ ValueCard Card.Twelve; ValueCard Card.Eleven ]
        [ ValueCard Card.Twelve; ValueCard Card.Eleven; ValueCard Card.Ten ]
        [ ActionCard Card.SecondChance; ValueCard Card.Seven ]
    ]

    for hand in hands do
        let deck = hand |> List.fold Deck.Decrement Deck.Full

        Assert.Equal(Simulation.expectedValueOfHit deck Deck.Empty hand, Lookahead.GainFromHitting 1 deck hand, 10)

[<Fact>]
let ``Sight is worth something, and a hand already past seven is worth banking`` () =
    let hand = [ ValueCard Card.Five ]
    let deck = hand |> List.fold Deck.Decrement Deck.Full

    let byDepth =
        [ 1..4 ] |> List.map (fun depth -> Lookahead.GainFromHitting depth deck hand)

    // Seeing further can only find more worth in a hand that may be played on,
    // because the hand can always decline what it finds
    Assert.Equal<float list>(byDepth |> List.sort, byDepth)
    Assert.True(List.last byDepth > List.head byDepth, "sight should be worth something")

    // Seven distinct cards ends the round, so there is nothing left to play for
    let flipped =
        [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]
        |> List.map ValueCard

    Assert.Equal(0.0, Lookahead.GainFromHitting 3 (flipped |> List.fold Deck.Decrement Deck.Full) flipped)

[<Fact>]
let ``A hand that cannot survive a card is worth standing on`` () =
    // Every value card in the deck is one this hand already holds
    let hand = [ ValueCard Card.One; ValueCard Card.Two ]

    let deck =
        Deck.Empty
        |> fun d -> Deck.Increment d (ValueCard Card.One)
        |> fun d -> Deck.Increment d (ValueCard Card.Two)

    Assert.True(Lookahead.GainFromHitting 3 deck hand < 0.0, "certain bust is never worth it")
