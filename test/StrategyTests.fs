module StrategyTests

open Xunit
open Flip7

// The evaluators take the strategy or targeting policy to evaluate as an
// argument, so the player's own declared policies are inert in these tests
let private player: Player = {
    Name = "Alice"
    Strategy = Strategy.Random
    Targeting = Targeting.ChoosesRandomly
    FirmScore = 0u
    Hand = [ ValueCard Card.Seven; ValueCard Card.Eight ]
}

let private other: Player = {
    Name = "Bob"
    Strategy = Strategy.Random
    Targeting = Targeting.ChoosesRandomly
    FirmScore = 0u
    Hand = [ ValueCard Card.One ]
}

let private decks = Deck.Full, Deck.Empty

// Tests are an edge of the program, so they inject the randomness and run
// the asynchronous decider synchronously
let private decide strategy round turn player others finished decks =
    Strategy.DecideHitOrStandWith (System.Random 1) strategy round turn player others finished decks
    |> Async.RunSynchronously

[<Fact>]
let ``ToString and Parse round-trip every strategy`` () =
    [
        Strategy.AlwaysHits
        Strategy.AlwaysStands
        Strategy.RandomWithProbability 0.25
        Strategy.HitUntilScore 45u
        Strategy.HitUntilNumCards 4u
        Strategy.HitUntilBustProbability 0.4
        Strategy.HitUntilNaiveBustProbability 0.35
        Strategy.SoftHitUntilScore(20u, 2.5)
        Strategy.HitUntilTotal 200u
        Strategy.HitUntilUniqueValues 5u
        Strategy.ChasesFlip7(18u, 6u)
        Strategy.EmboldenedBySecondChance 21u
        Strategy.HitWhileBehindLeader 10u
        Strategy.StandsAfterTurn 12u
        Strategy.MaximizesExpectedValue
        Strategy.Custom "TerminalPrompt"
    ]
    |> List.iter (fun strategy -> Assert.Equal(strategy, Strategy.Parse(string strategy)))

[<Fact>]
let ``Externally decided strategies cannot be evaluated by DecideHitOrStandWith`` () =
    let player: Player = {
        Name = "You"
        Strategy = Strategy.Custom "TerminalPrompt"
        Targeting = Targeting.ChoosesRandomly
        FirmScore = 0u
        Hand = [ ValueCard Card.Seven ]
    }

    Assert.Throws<System.InvalidOperationException>(fun () ->
        Strategy.DecideHitOrStandWith (System.Random 1) (Strategy.Custom "TerminalPrompt") 1u 2u player [] [] decks
        |> ignore
    )
    |> ignore

[<Fact>]
let ``TryParse returns None for invalid strings`` () =
    Assert.Equal(None, Strategy.TryParse "Bogus")
    Assert.Equal(None, Strategy.TryParse "HitUntilScore")

[<Fact>]
let ``AlwaysHits always hits and AlwaysStands always stands`` () =
    Assert.Equal(Strategy.Hit, decide Strategy.AlwaysHits 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide Strategy.AlwaysStands 0u 0u player [] [] decks)

[<Fact>]
let ``RandomWithProbability is deterministic at the extremes`` () =
    // NextDouble is in [0, 1) so probability 1.0 always hits and 0.0 never does
    Assert.Equal(Strategy.Hit, decide (Strategy.RandomWithProbability 1.0) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.RandomWithProbability 0.0) 0u 0u player [] [] decks)

[<Fact>]
let ``RandomWithProbability is reproducible with a seeded random`` () =
    let decisions seed =
        List.init
            100
            (fun _ ->
                Strategy.DecideHitOrStandWith (System.Random seed) Strategy.Random 0u 0u player [] [] decks
                |> Async.RunSynchronously
            )

    Assert.Equal<Strategy.HitOrStand list>(decisions 42, decisions 42)

[<Fact>]
let ``HitUntilScore hits below the threshold and stands at it`` () =
    // The player's hand scores 15
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilScore 16u) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilScore 15u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilNumCards hits below the threshold and stands at it`` () =
    // The player's hand has 2 cards
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilNumCards 3u) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilNumCards 2u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilBustProbability hits below the threshold and stands above it`` () =
    // With a full deck, the player's hand of a Seven and an Eight busts on 15
    // of the 94 remaining cards (~16%)
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilBustProbability 0.2) 0u 0u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilBustProbability 0.1) 0u 0u player [ other ] [] decks)

[<Fact>]
let ``HitUntilBustProbability counts deal3 but not freeze as a bust when alone`` () =
    // With other players still in the round the hand of a Seven and an Eight
    // busts on 15 of 94 cards (~16%). Alone, a drawn deal3 must be played on
    // yourself and can bust you across its three flips, pushing the probability
    // past 17%; a drawn freeze would just bank your points, so it does not count
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilBustProbability 0.17) 0u 0u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilBustProbability 0.17) 0u 0u player [] [] decks)

let private allSevens: Deck =
    List.replicate 5 (ValueCard Card.Seven) |> List.fold Deck.Increment Deck.Empty

[<Fact>]
let ``HitUntilNaiveBustProbability ignores the actual deck composition`` () =
    // Every remaining card is a Seven the player already holds, but the naive
    // player imagines the full deck minus their own hand (13 of 92, ~14%)
    let poisoned = allSevens, Deck.Empty
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilNaiveBustProbability 0.2) 0u 0u player [ other ] [] poisoned)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilBustProbability 0.2) 0u 0u player [ other ] [] poisoned)

[<Fact>]
let ``SoftHitUntilScore is deterministic far from the threshold`` () =
    // 15 points is 50 temperature-units below 20 and above 10, so the hit
    // probability saturates at ~1 and ~0
    Assert.Equal(Strategy.Hit, decide (Strategy.SoftHitUntilScore(20u, 0.1)) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.SoftHitUntilScore(10u, 0.1)) 0u 0u player [] [] decks)

[<Fact>]
let ``HitUntilTotal counts banked points as well as the hand`` () =
    // 100 banked plus 15 in hand
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilTotal 116u) 0u 0u banked [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilTotal 115u) 0u 0u banked [] [] decks)

[<Fact>]
let ``HitUntilUniqueValues ignores modifier cards`` () =
    // Three cards but only two unique values
    let modified = {
        player with
            Hand = ModifierCard Card.Plus4 :: player.Hand
    }
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilUniqueValues 3u) 0u 0u modified [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilUniqueValues 2u) 0u 0u modified [] [] decks)

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
    Assert.Equal(Strategy.Hit, decide (Strategy.ChasesFlip7(20u, 6u)) 0u 0u sixUniques [] [] decks)
    // With only two uniques the score threshold applies as usual
    Assert.Equal(Strategy.Stand, decide (Strategy.ChasesFlip7(15u, 6u)) 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Hit, decide (Strategy.ChasesFlip7(16u, 6u)) 0u 0u player [] [] decks)

[<Fact>]
let ``EmboldenedBySecondChance hits fearlessly while holding one`` () =
    let insured = {
        player with
            Hand = ActionCard Card.SecondChance :: player.Hand
    }
    Assert.Equal(Strategy.Hit, decide (Strategy.EmboldenedBySecondChance 15u) 0u 0u insured [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.EmboldenedBySecondChance 15u) 0u 0u player [] [] decks)

[<Fact>]
let ``HitWhileBehindLeader races the visible table totals`` () =
    // The rival shows 30 banked plus 1 in hand against our 15
    let rival = { other with FirmScore = 30u }
    Assert.Equal(Strategy.Hit, decide (Strategy.HitWhileBehindLeader 0u) 0u 0u player [ rival ] [] decks)

    // Ahead 115 to 31 stands, unless the margin demands a bigger lead
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Stand, decide (Strategy.HitWhileBehindLeader 0u) 0u 0u banked [ rival ] [] decks)
    Assert.Equal(Strategy.Hit, decide (Strategy.HitWhileBehindLeader 100u) 0u 0u banked [ rival ] [] decks)

    // A leader who already stood still counts: their locked hand shows 116
    let stood = { rival with FirmScore = 115u }
    Assert.Equal(Strategy.Hit, decide (Strategy.HitWhileBehindLeader 0u) 0u 0u banked [] [ stood ] decks)

    // A busted player's hand is worthless, so they are not the leader
    let busted = {
        stood with
            Hand = [ ValueCard Card.Seven; ValueCard Card.Seven ]
    }

    Assert.Equal(Strategy.Stand, decide (Strategy.HitWhileBehindLeader 0u) 0u 0u banked [] [ busted ] decks)

[<Fact>]
let ``StandsAfterTurn hits up to and including the given turn`` () =
    // Turn 3 is at the threshold; turn 4 is past it
    Assert.Equal(Strategy.Hit, decide (Strategy.StandsAfterTurn 3u) 0u 3u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.StandsAfterTurn 3u) 0u 4u player [] [] decks)

[<Fact>]
let ``MaximizesExpectedValue hits on a fresh deck and stands when every card busts`` () =
    Assert.Equal(Strategy.Hit, decide Strategy.MaximizesExpectedValue 0u 0u player [] [] decks)
    Assert.Equal(Strategy.Stand, decide Strategy.MaximizesExpectedValue 0u 0u player [] [] (allSevens, Deck.Empty))

// ---- targeting ----

// The three candidates the engine would offer, distinct on every axis a policy
// looks at: Bob is furthest ahead, Carol is closest to busting, Dave is
// furthest behind
let private leader: Player = {
    Name = "Bob"
    Strategy = Strategy.Random
    Targeting = Targeting.ChoosesRandomly
    FirmScore = 60u
    Hand = []
}

let private bustProne: Player = {
    Name = "Carol"
    Strategy = Strategy.Random
    Targeting = Targeting.ChoosesRandomly
    FirmScore = 20u
    Hand = [
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
    ]
}

let private laggard: Player = {
    Name = "Dave"
    Strategy = Strategy.Random
    Targeting = Targeting.ChoosesRandomly
    FirmScore = 5u
    Hand = []
}

let private opponents = [ leader; bustProne; laggard ]

// Out of a full deck: 53% of a card busting this hand, so a greedy player banks
let private risky = [
    ValueCard Card.Eight
    ValueCard Card.Nine
    ValueCard Card.Ten
    ValueCard Card.Eleven
    ValueCard Card.Twelve
]

// ...against 1%, so a greedy player takes three more
let private safe = [ ValueCard Card.One ]

// Banks once a hit is close to a coin flip, and takes three forced flips only
// while a single one is nearly safe
let private greedy = Targeting.PlaysGreedily(0.5, 0.1)

let private aim targeting ask chooser candidates =
    Strategy.DecideTargetWith (System.Random 1) targeting ask chooser candidates [] decks
    |> Async.RunSynchronously

[<Fact>]
let ``ToString and Parse round-trip every targeting policy`` () =
    [
        Targeting.ChoosesRandomly
        Targeting.PlaysSpitefully
        Targeting.PlaysGreedily(0.5, 0.1)
        Targeting.ChoosesExternally "TerminalPrompt"
    ]
    |> List.iter (fun targeting -> Assert.Equal(targeting, Targeting.Parse(string targeting)))

[<Fact>]
let ``TryParse returns None for an unparseable targeting policy`` () =
    Assert.Equal(None, Targeting.TryParse "Bogus")
    Assert.Equal(None, Targeting.TryParse "ChoosesExternally")

[<Fact>]
let ``Externally decided targeting cannot be evaluated by DecideTargetWith`` () =
    Assert.Throws<System.InvalidOperationException>(fun () ->
        aim (Targeting.ChoosesExternally "TerminalPrompt") Strategy.WhoToFreeze player opponents
        |> ignore
    )
    |> ignore

[<Fact>]
let ``a spiteful player freezes whoever is furthest ahead`` () =
    Assert.Equal("Bob", (aim Targeting.PlaysSpitefully Strategy.WhoToFreeze player opponents).Name)

[<Fact>]
let ``a spiteful player deals three to whoever is closest to busting`` () =
    Assert.Equal("Carol", (aim Targeting.PlaysSpitefully Strategy.WhoReceivesDeal3 player opponents).Name)

[<Fact>]
let ``a spiteful player passes a second chance to whoever is furthest behind`` () =
    Assert.Equal("Dave", (aim Targeting.PlaysSpitefully Strategy.WhoReceivesSecondChance player opponents).Name)

[<Fact>]
let ``a greedy player freezes themselves rather than flip on a likely bust`` () =
    let chooser = { player with Hand = risky }

    Assert.Equal("Alice", (aim greedy Strategy.WhoToFreeze chooser (chooser :: opponents)).Name)

[<Fact>]
let ``a greedy player deals three to themselves while their hand is safe`` () =
    let chooser = { player with Hand = safe }

    Assert.Equal("Alice", (aim greedy Strategy.WhoReceivesDeal3 chooser (chooser :: opponents)).Name)

[<Fact>]
let ``a greedy player deals three away once their own hand is risky`` () =
    let chooser = { player with Hand = risky }

    Assert.Equal("Carol", (aim greedy Strategy.WhoReceivesDeal3 chooser (chooser :: opponents)).Name)

[<Fact>]
let ``a greedy player's banking threshold decides whether it flips on`` () =
    let chooser = { player with Hand = risky }
    let candidates = chooser :: opponents

    // 53% of a card busting this hand, so it banks by freezing itself...
    Assert.Equal("Alice", (aim greedy Strategy.WhoToFreeze chooser candidates).Name)

    // ...unless it takes more than that to scare it, and then the freeze goes
    // to whoever is furthest ahead instead
    Assert.Equal("Bob", (aim (Targeting.PlaysGreedily(0.9, 0.1)) Strategy.WhoToFreeze chooser candidates).Name)

[<Fact>]
let ``a greedy player's deal3 threshold decides who takes the three flips`` () =
    let candidates = player :: opponents

    // 16% of a card busting this hand, too rich for the default threshold...
    Assert.Equal("Carol", (aim greedy Strategy.WhoReceivesDeal3 player candidates).Name)

    // ...but worth keeping for a player happy to run at a fifth
    Assert.Equal("Alice", (aim (Targeting.PlaysGreedily(0.5, 0.25)) Strategy.WhoReceivesDeal3 player candidates).Name)

// A player frozen earlier in a deal3 still hands out the cards they set aside,
// and can no longer keep one for themselves
[<Fact>]
let ``a chooser who is no longer a candidate still aims at someone legal`` () =
    let chooser = { player with Hand = risky }

    Assert.Equal("Bob", (aim greedy Strategy.WhoToFreeze chooser opponents).Name)
    Assert.Equal("Carol", (aim greedy Strategy.WhoReceivesDeal3 chooser opponents).Name)

[<Fact>]
let ``choosing randomly always answers with one of the candidates`` () =
    let names = opponents |> List.map (fun candidate -> candidate.Name)

    for _ in 1..50 do
        Assert.Contains((aim Targeting.ChoosesRandomly Strategy.WhoToFreeze player opponents).Name, names)
