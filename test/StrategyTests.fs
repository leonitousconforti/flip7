module StrategyTests

open Xunit
open Flip7

let private player: Strategy.StrategyPlayer = {
    Name = "Alice"
    FirmScore = 0u
    Hand = [ ValueCard Card.Seven; ValueCard Card.Eight ]
}

let private decks = Deck.Full, Deck.Empty

[<Fact>]
let ``ToString and Parse round-trip every strategy`` () =
    [
        AlwaysHits
        AlwaysStands
        RandomWithProbability 0.25
        HitUntilScore 45u
        HitUntilNumCards 4u
    ]
    |> List.iter (fun strategy -> Assert.Equal(strategy, Strategy.Parse(string strategy)))

[<Fact>]
let ``TryParse returns None for invalid strings`` () =
    Assert.Equal(None, Strategy.TryParse "Bogus")
    Assert.Equal(None, Strategy.TryParse "HitUntilScore")

[<Fact>]
let ``AlwaysHits always hits and AlwaysStands always stands`` () =
    Assert.Equal(Strategy.Hit, Strategy.Decide AlwaysHits 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide AlwaysStands 0u player [] decks)

[<Fact>]
let ``RandomWithProbability is deterministic at the extremes`` () =
    // NextDouble is in [0, 1) so probability 1.0 always hits and 0.0 never does
    Assert.Equal(Strategy.Hit, Strategy.Decide (RandomWithProbability 1.0) 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (RandomWithProbability 0.0) 0u player [] decks)

[<Fact>]
let ``RandomWithProbability is reproducible with a seeded random`` () =
    let decisions seed =
        List.init 100 (fun _ ->
            Strategy.DecideWith (System.Random seed) Strategy.Random 0u player [] decks
        )

    Assert.Equal<Strategy.HitOrStand list>(decisions 42, decisions 42)

[<Fact>]
let ``HitUntilScore hits below the threshold and stands at it`` () =
    // The player's hand scores 15
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilScore 16u) 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilScore 15u) 0u player [] decks)

[<Fact>]
let ``HitUntilNumCards hits below the threshold and stands at it`` () =
    // The player's hand has 2 cards
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilNumCards 3u) 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilNumCards 2u) 0u player [] decks)
