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

// Tests are an edge of the program, so they inject the randomness and run
// the asynchronous decider synchronously
let private decide strategy round turn player others finished decks =
    Strategy.DecideWith (System.Random 1) strategy round turn player others finished decks
    |> Async.RunSynchronously

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
        Prompt
    ]
    |> List.iter (fun strategy -> Assert.Equal(strategy, Strategy.Parse(string strategy)))

[<Fact>]
let ``Externally decided strategies cannot be evaluated by DecideWith`` () =
    let player: Strategy.StrategyPlayer = {
        Name = "You"
        FirmScore = 0u
        Hand = [ ValueCard Card.Seven ]
    }

    Assert.Throws<System.InvalidOperationException>(fun () ->
        Strategy.DecideWith (System.Random 1) Prompt 1u 2u player [] [] decks |> ignore
    )
    |> ignore

[<Fact>]
let ``TryParse returns None for invalid strings`` () =
    Assert.Equal(None, Strategy.TryParse "Bogus")
    Assert.Equal(None, Strategy.TryParse "HitUntilScore")

[<Fact>]
let ``AlwaysHits always hits and AlwaysStands always stands`` () =
    Assert.Equal(Strategy.Hit, decide AlwaysHits 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide AlwaysStands 0u 0u player [] [] decks)

[<Fact>]
let ``RandomWithProbability is deterministic at the extremes`` () =
    // NextDouble is in [0, 1) so probability 1.0 always hits and 0.0 never does
    Assert.Equal(Strategy.Hit, decide (RandomWithProbability 1.0) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (RandomWithProbability 0.0) 0u 0u player [] [] decks)

[<Fact>]
let ``RandomWithProbability is reproducible with a seeded random`` () =
    let decisions seed =
        List.init
            100
            (fun _ ->
                Strategy.DecideWith (System.Random seed) Strategy.Random 0u 0u player [] [] decks
                |> Async.RunSynchronously
            )

    Assert.Equal<Strategy.HitOrStand list>(decisions 42, decisions 42)

[<Fact>]
let ``HitUntilScore hits below the threshold and stands at it`` () =
    // The player's hand scores 15
    Assert.Equal(Strategy.Hit, decide (HitUntilScore 16u) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilScore 15u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilNumCards hits below the threshold and stands at it`` () =
    // The player's hand has 2 cards
    Assert.Equal(Strategy.Hit, decide (HitUntilNumCards 3u) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilNumCards 2u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilBustProbability hits below the threshold and stands above it`` () =
    // With a full deck, the player's hand of a Seven and an Eight busts on 15
    // of the 94 remaining cards (~16%)
    Assert.Equal(Strategy.Hit, decide (HitUntilBustProbability 0.2) 0u 0u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilBustProbability 0.1) 0u 0u player [ other ] [] decks)

[<Fact>]
let ``HitUntilBustProbability counts deal3 but not freeze as a bust when alone`` () =
    // With other players still in the round the hand of a Seven and an Eight
    // busts on 15 of 94 cards (~16%). Alone, a drawn deal3 must be played on
    // yourself and can bust you across its three flips, pushing the probability
    // past 17%; a drawn freeze would just bank your points, so it does not count
    Assert.Equal(Strategy.Hit, decide (HitUntilBustProbability 0.17) 0u 0u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilBustProbability 0.17) 0u 0u player [] [] decks)

let private allSevens: Deck =
    List.replicate 5 (ValueCard Card.Seven) |> List.fold Deck.Increment Deck.Empty

[<Fact>]
let ``HitUntilNaiveBustProbability ignores the actual deck composition`` () =
    // Every remaining card is a Seven the player already holds, but the naive
    // player imagines the full deck minus their own hand (13 of 92, ~14%)
    let poisoned = allSevens, Deck.Empty
    Assert.Equal(Strategy.Hit, decide (HitUntilNaiveBustProbability 0.2) 0u 0u player [ other ] [] poisoned)
    Assert.Equal(Strategy.Stand, decide (HitUntilBustProbability 0.2) 0u 0u player [ other ] [] poisoned)

[<Fact>]
let ``SoftHitUntilScore is deterministic far from the threshold`` () =
    // 15 points is 50 temperature-units below 20 and above 10, so the hit
    // probability saturates at ~1 and ~0
    Assert.Equal(Strategy.Hit, decide (SoftHitUntilScore(20u, 0.1)) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (SoftHitUntilScore(10u, 0.1)) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilTotal counts banked points as well as the hand`` () =
    // 100 banked plus 15 in hand
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Hit, decide (HitUntilTotal 116u) 0u 0u banked [] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilTotal 115u) 0u 0u banked [] [] decks)

[<Fact>]
let ``HitUntilUniqueValues ignores modifier cards`` () =
    // Three cards but only two unique values
    let modified = {
        player with
            Hand = ModifierCard Card.Plus4 :: player.Hand
    }
    Assert.Equal(Strategy.Hit, decide (HitUntilUniqueValues 3u) 0u 0u modified [] [] decks)
    Assert.Equal(Strategy.Stand, decide (HitUntilUniqueValues 2u) 0u 0u modified [] [] decks)

[<Fact>]
let ``ChasesFlip7 keeps hitting near the bonus regardless of score`` () =
    let sixUniques = {
        player with
            Hand =
                [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
                |> List.map ValueCard
    }

    // 21 points would normally stand at a threshold of 20, but six unique
    // value cards are one flip from the bonus
    Assert.Equal(Strategy.Hit, decide (ChasesFlip7(20u, 6u)) 0u 0u sixUniques [] [] decks)
    // With only two uniques the score threshold applies as usual
    Assert.Equal(Strategy.Stand, decide (ChasesFlip7(15u, 6u)) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Hit, decide (ChasesFlip7(16u, 6u)) 0u 0u player [] [] decks)

[<Fact>]
let ``EmboldenedBySecondChance hits fearlessly while holding one`` () =
    let insured = {
        player with
            Hand = ActionCard Card.SecondChance :: player.Hand
    }
    Assert.Equal(Strategy.Hit, decide (EmboldenedBySecondChance 15u) 0u 0u insured [] [] decks)
    Assert.Equal(Strategy.Stand, decide (EmboldenedBySecondChance 15u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitWhileBehindLeader races the visible table totals`` () =
    // The rival shows 30 banked plus 1 in hand against our 15
    let rival = { other with FirmScore = 30u }
    Assert.Equal(Strategy.Hit, decide (HitWhileBehindLeader 0u) 0u 0u player [ rival ] [] decks)

    // Ahead 115 to 31 stands, unless the margin demands a bigger lead
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Stand, decide (HitWhileBehindLeader 0u) 0u 0u banked [ rival ] [] decks)
    Assert.Equal(Strategy.Hit, decide (HitWhileBehindLeader 100u) 0u 0u banked [ rival ] [] decks)

    // A leader who already stood still counts: their locked hand shows 116
    let stood = { rival with FirmScore = 115u }
    Assert.Equal(Strategy.Hit, decide (HitWhileBehindLeader 0u) 0u 0u banked [] [ stood ] decks)

    // A busted player's hand is worthless, so they are not the leader
    let busted = {
        stood with
            Hand = [ ValueCard Card.Seven; ValueCard Card.Seven ]
    }

    Assert.Equal(Strategy.Stand, decide (HitWhileBehindLeader 0u) 0u 0u banked [] [ busted ] decks)

[<Fact>]
let ``StandsAfterTurn hits up to and including the given turn`` () =
    // Turn 3 is at the threshold; turn 4 is past it
    Assert.Equal(Strategy.Hit, decide (StandsAfterTurn 3u) 0u 3u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (StandsAfterTurn 3u) 0u 4u player [] [] decks)

[<Fact>]
let ``MaximizesExpectedValue hits on a fresh deck and stands when every card busts`` () =
    Assert.Equal(Strategy.Hit, decide MaximizesExpectedValue 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide MaximizesExpectedValue 0u 0u player [] [] (allSevens, Deck.Empty))
