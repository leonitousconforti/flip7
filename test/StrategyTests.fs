module StrategyTests

open Xunit
open Flip7

let private player: Strategy.StrategyPlayer = {
    Name = "Alice"
    FirmScore = 0u
    Hand = [ ValueCard Card.Seven; ValueCard Card.Eight ]
}

let private other: Strategy.StrategyPlayer = {
    Name = "Bob"
    FirmScore = 0u
    Hand = [ ValueCard Card.One ]
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
        HitUntilBustProbability 0.4
        HitUntilNaiveBustProbability 0.35
        SoftHitUntilScore(20u, 2.5)
        HitUntilTotal 200u
        HitUntilUniqueValues 5u
        ChasesFlip7(18u, 6u)
        EmboldenedBySecondChance 21u
        HitWhileBehindLeader 10u
        StandsAfterTurn 12u
        MaximizesExpectedValue
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

[<Fact>]
let ``HitUntilBustProbability hits below the threshold and stands above it`` () =
    // With a full deck, the player's hand of a Seven and an Eight busts on 15
    // of the 94 remaining cards (~16%)
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilBustProbability 0.2) 0u player [ other ] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilBustProbability 0.1) 0u player [ other ] decks)

[<Fact>]
let ``HitUntilBustProbability counts freeze and deal3 as busts when alone`` () =
    // With other players still in the round the hand busts on 15 of 94 cards
    // (~16%), but alone the freeze and deal3 cards must be played on yourself,
    // pushing the probability past 19%
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilBustProbability 0.18) 0u player [ other ] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilBustProbability 0.18) 0u player [] decks)

let private allSevens: Deck =
    List.replicate 5 (ValueCard Card.Seven) |> List.fold Deck.Increment Deck.Empty

[<Fact>]
let ``HitUntilNaiveBustProbability ignores the actual deck composition`` () =
    // Every remaining card is a Seven the player already holds, but the naive
    // player imagines the full deck minus their own hand (13 of 92, ~14%)
    let poisoned = allSevens, Deck.Empty
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilNaiveBustProbability 0.2) 0u player [ other ] poisoned)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilBustProbability 0.2) 0u player [ other ] poisoned)

[<Fact>]
let ``SoftHitUntilScore is deterministic far from the threshold`` () =
    // 15 points is 50 temperature-units below 20 and above 10, so the hit
    // probability saturates at ~1 and ~0
    Assert.Equal(Strategy.Hit, Strategy.Decide (SoftHitUntilScore(20u, 0.1)) 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (SoftHitUntilScore(10u, 0.1)) 0u player [] decks)

[<Fact>]
let ``HitUntilTotal counts banked points as well as the hand`` () =
    // 100 banked plus 15 in hand
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilTotal 116u) 0u banked [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilTotal 115u) 0u banked [] decks)

[<Fact>]
let ``HitUntilUniqueValues ignores modifier cards`` () =
    // Three cards but only two unique values
    let modified = { player with Hand = ModifierCard Card.Plus4 :: player.Hand }
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitUntilUniqueValues 3u) 0u modified [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitUntilUniqueValues 2u) 0u modified [] decks)

[<Fact>]
let ``ChasesFlip7 keeps hitting near the bonus regardless of score`` () =
    let sixUniques = {
        player with
            Hand = [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ] |> List.map ValueCard
    }

    // 21 points would normally stand at a threshold of 20, but six unique
    // value cards are one flip from the bonus
    Assert.Equal(Strategy.Hit, Strategy.Decide (ChasesFlip7(20u, 6u)) 0u sixUniques [] decks)
    // With only two uniques the score threshold applies as usual
    Assert.Equal(Strategy.Stand, Strategy.Decide (ChasesFlip7(15u, 6u)) 0u player [] decks)
    Assert.Equal(Strategy.Hit, Strategy.Decide (ChasesFlip7(16u, 6u)) 0u player [] decks)

[<Fact>]
let ``EmboldenedBySecondChance hits fearlessly while holding one`` () =
    let insured = { player with Hand = ActionCard Card.SecondChance :: player.Hand }
    Assert.Equal(Strategy.Hit, Strategy.Decide (EmboldenedBySecondChance 15u) 0u insured [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (EmboldenedBySecondChance 15u) 0u player [] decks)

[<Fact>]
let ``HitWhileBehindLeader races the visible table totals`` () =
    // The rival shows 30 banked plus 1 in hand against our 15
    let rival = { other with FirmScore = 30u }
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitWhileBehindLeader 0u) 0u player [ rival ] decks)

    // Ahead 115 to 31 stands, unless the margin demands a bigger lead
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Stand, Strategy.Decide (HitWhileBehindLeader 0u) 0u banked [ rival ] decks)
    Assert.Equal(Strategy.Hit, Strategy.Decide (HitWhileBehindLeader 100u) 0u banked [ rival ] decks)

[<Fact>]
let ``StandsAfterTurn stands once the round is old enough`` () =
    Assert.Equal(Strategy.Hit, Strategy.Decide (StandsAfterTurn 5u) 4u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide (StandsAfterTurn 5u) 5u player [] decks)

[<Fact>]
let ``MaximizesExpectedValue hits on a fresh deck and stands when every card busts`` () =
    Assert.Equal(Strategy.Hit, Strategy.Decide MaximizesExpectedValue 0u player [] decks)
    Assert.Equal(Strategy.Stand, Strategy.Decide MaximizesExpectedValue 0u player [] (allSevens, Deck.Empty))
