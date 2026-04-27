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

    let probabilityToBust = Simulation.probabilityToBust deck discards hand true
    Assert.Equal(0.75, probabilityToBust)
