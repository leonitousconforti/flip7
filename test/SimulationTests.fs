module SimulationTests

open Xunit
open Flip7

[<Fact>]
let ``Probability to bust simple when multiple player left`` () =
    let discards = Deck.Empty
    let hand = [ ValueCard Card.One ]
    let deck =
        Map.ofList [
            ActionCard Card.Freeze, 1u
            ActionCard Card.Deal3, 1u
            ValueCard Card.One, 1u
            ValueCard Card.Two, 1u
        ]

    let probabilityToBust = Simulation.probabilityToBust deck discards hand false
    Assert.Equal(0.25, probabilityToBust)

[<Fact>]
let ``Probability to bust simple when only player left`` () =
    let discards = Deck.Empty
    let hand = [ ValueCard Card.One ]
    let deck =
        Map.ofList [
            ActionCard Card.Freeze, 1u
            ActionCard Card.Deal3, 1u
            ValueCard Card.One, 1u
            ValueCard Card.Two, 1u
        ]

    // A duplicate One (1/4) or a deal3 (1/4) that forces a bust across its
    // flips; a drawn freeze would bank our points, so it is not a bust
    let probabilityToBust = Simulation.probabilityToBust deck discards hand true
    Assert.Equal(0.5, probabilityToBust)

[<Fact>]
let ``Probability to bust is zero while holding a second chance`` () =
    // Every remaining card duplicates the hand, but the held second chance
    // cancels the next duplicate
    let deck = Map.ofList [ ValueCard Card.One, 2u ]
    let hand = [ ActionCard Card.SecondChance; ValueCard Card.One ]
    Assert.Equal(0.0, Simulation.probabilityToBust deck Deck.Empty hand false)

[<Fact>]
let ``Expected value of hit weighs busts against gains`` () =
    // Half the time bust and lose the 1, half the time draw the Two
    let deck = Map.ofList [ ValueCard Card.One, 1u; ValueCard Card.Two, 1u ]
    Assert.Equal(0.5, Simulation.expectedValueOfHit deck Deck.Empty [ ValueCard Card.One ])

[<Fact>]
let ``Expected value of hit draws from the discards when the deck is empty`` () =
    let discards = Map.ofList [ ValueCard Card.Two, 1u ]
    Assert.Equal(2.0, Simulation.expectedValueOfHit Deck.Empty discards [ ValueCard Card.One ])

[<Fact>]
let ``Expected value of hit includes the flip7 bonus`` () =
    // The guaranteed Seven completes the flip7: 28 points + 15 bonus - 21 now
    let hand =
        [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
        |> List.map ValueCard
    let deck = Map.ofList [ ValueCard Card.Seven, 1u ]
    Assert.Equal(22.0, Simulation.expectedValueOfHit deck Deck.Empty hand)
