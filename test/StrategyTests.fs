module StrategyTests

open System
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// The evaluators take the strategy or targeting policy to evaluate as an
// argument, so the players' own declared policies are inert in these tests.
// Alice's Seven and Eight are worth 15 and bust on 15 of the full deck's 94
// cards.
let private player =
    Player.Make("Alice", Strategy.Random, hand = [ ValueCard Card.Seven; ValueCard Card.Eight ])

let private other =
    Player.Make("Bob", Strategy.Random, hand = [ ValueCard Card.One ])

let private decks = Deck.Full, Deck.Empty

// A deck of nothing but Sevens, so every card left busts Alice's hand
let private allSevens: Deck = deckOf (List.replicate 5 (ValueCard Card.Seven))

// Tests are an edge of the program, so they inject the randomness and run the
// asynchronous decider synchronously. No strategy reads the round or the game
// so far, so every question is asked in round 1 with no history
let private decideAt strategy turn player others finished decks =
    Strategy.Decider.Default.HitOrStand (Random 1) strategy Seq.empty 1u turn player others finished decks
    |> Async.RunSynchronously

// Most questions find the player alone at their first turn, with the full deck
// to draw from
let private decide strategy player = decideAt strategy 1u player [] [] decks

// ---- hit-or-stand strategies ----

[<Fact>]
let ``RandomWithProbability is deterministic at the extremes`` () =
    // NextDouble is in [0, 1) so probability 1.0 always hits and 0.0 never does
    Assert.Equal(Strategy.Hit, decide (Strategy.RandomWithProbability 1.0) player)
    Assert.Equal(Strategy.Stand, decide (Strategy.RandomWithProbability 0.0) player)

[<Fact>]
let ``HitUntilScore hits below its threshold and stands at it`` () =
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilScore 16u) player)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilScore 15u) player)

// The Plus4 counts as a card for HitUntilNumCards but not as a value for
// HitUntilUniqueValues, so at a threshold of 3 the two strategies disagree
let private modified = {
    player with
        Hand = ModifierCard Card.Plus4 :: player.Hand
}

[<Fact>]
let ``HitUntilNumCards counts modifiers as cards, hitting below its threshold and standing at it`` () =
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilNumCards 4u) modified)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilNumCards 3u) modified)

[<Fact>]
let ``HitUntilUniqueValues ignores modifier cards`` () =
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilUniqueValues 3u) modified)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilUniqueValues 2u) modified)

[<Fact>]
let ``HitUntilBustProbability weighs the real deck, counting the deal3 a lone player must take`` () =
    // With Bob still in, the 15 busting cards of 94 give 15.96%. Alone, a drawn
    // deal3 must be taken and each of its three flips can bust too, which lifts
    // the chance to 17.61%. A threshold of 0.17 sits between the two
    Assert.Equal(0.1596, Simulation.probabilityToBust Deck.Full Deck.Empty player.Hand false, 4)
    Assert.Equal(0.1761, Simulation.probabilityToBust Deck.Full Deck.Empty player.Hand true, 4)

    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitUntilBustProbability 0.17) 1u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilBustProbability 0.17) player)
    Assert.Equal(Strategy.Stand, decideAt (Strategy.HitUntilBustProbability 0.15) 1u player [ other ] [] decks)

[<Fact>]
let ``HitUntilNaiveBustProbability ignores the actual deck composition`` () =
    // Every remaining card is a Seven the player already holds, but the naive
    // player imagines the full deck minus their own hand, where 13 of the 92
    // cards bust them
    let poisoned = allSevens, Deck.Empty
    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitUntilNaiveBustProbability 0.2) 1u player [ other ] [] poisoned)
    Assert.Equal(Strategy.Stand, decideAt (Strategy.HitUntilBustProbability 0.2) 1u player [ other ] [] poisoned)

    // 13 in 92 is under 0.15, but alone the deal3 the naive player imagines
    // drawing lifts the chance past it
    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitUntilNaiveBustProbability 0.15) 1u player [ other ] [] decks)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilNaiveBustProbability 0.15) player)

[<Fact>]
let ``SoftHitUntilScore is deterministic far from the threshold`` () =
    // 15 points is 50 temperature-units below 20 and above 10, so the hit
    // probability saturates at ~1 and ~0
    Assert.Equal(Strategy.Hit, decide (Strategy.SoftHitUntilScore(20u, 0.1)) player)
    Assert.Equal(Strategy.Stand, decide (Strategy.SoftHitUntilScore(10u, 0.1)) player)

[<Fact>]
let ``HitUntilTotal counts banked points as well as the hand`` () =
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Hit, decide (Strategy.HitUntilTotal(100u + 15u + 1u)) banked)
    Assert.Equal(Strategy.Stand, decide (Strategy.HitUntilTotal(100u + 15u)) banked)

[<Fact>]
let ``ChasesFlip7 keeps hitting near the bonus regardless of score`` () =
    let sixUniques = {
        player with
            Hand = values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
    }

    // 21 points would normally stand at a threshold of 20, but six unique
    // value cards are one flip from the bonus
    Assert.Equal(Strategy.Hit, decide (Strategy.ChasesFlip7(20u, 6u)) sixUniques)
    // With only two uniques the score threshold applies as usual
    Assert.Equal(Strategy.Stand, decide (Strategy.ChasesFlip7(15u, 6u)) player)
    Assert.Equal(Strategy.Hit, decide (Strategy.ChasesFlip7(16u, 6u)) player)

[<Fact>]
let ``EmboldenedBySecondChance hits fearlessly while holding one`` () =
    let insured = {
        player with
            Hand = ActionCard Card.SecondChance :: player.Hand
    }

    Assert.Equal(Strategy.Hit, decide (Strategy.EmboldenedBySecondChance 15u) insured)
    Assert.Equal(Strategy.Stand, decide (Strategy.EmboldenedBySecondChance 15u) player)

[<Fact>]
let ``HitWhileBehindLeader races the visible table totals`` () =
    // The rival shows 30 banked plus 1 in hand against our 15
    let rival = { other with FirmScore = 30u }
    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitWhileBehindLeader 0u) 1u player [ rival ] [] decks)

    // Ahead 115 to 31 stands, unless the margin demands a bigger lead
    let banked = { player with FirmScore = 100u }
    Assert.Equal(Strategy.Stand, decideAt (Strategy.HitWhileBehindLeader 0u) 1u banked [ rival ] [] decks)
    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitWhileBehindLeader 100u) 1u banked [ rival ] [] decks)

    // A leader who already stood still counts: their locked hand shows 116
    let stood = { rival with FirmScore = 115u }
    Assert.Equal(Strategy.Hit, decideAt (Strategy.HitWhileBehindLeader 0u) 1u banked [] [ stood ] decks)

    // A busted player's hand is worthless, so they show only their 115
    // banked, which ties at 115, and a tie stands
    let busted = {
        stood with
            Hand = [ ValueCard Card.Seven; ValueCard Card.Seven ]
    }

    Assert.Equal(Strategy.Stand, decideAt (Strategy.HitWhileBehindLeader 0u) 1u banked [] [ busted ] decks)

[<Fact>]
let ``StandsAfterTurn hits up to and including the given turn`` () =
    // Turn 3 is at the threshold; turn 4 is past it
    Assert.Equal(Strategy.Hit, decideAt (Strategy.StandsAfterTurn 3u) 3u player [] [] decks)
    Assert.Equal(Strategy.Stand, decideAt (Strategy.StandsAfterTurn 3u) 4u player [] [] decks)

[<Fact>]
let ``MaximizesExpectedValue hits on a fresh deck and stands when every card busts`` () =
    Assert.Equal(Strategy.Hit, decide Strategy.MaximizesExpectedValue player)
    Assert.Equal(Strategy.Stand, decideAt Strategy.MaximizesExpectedValue 1u player [] [] (allSevens, Deck.Empty))

// PlaysLikeAHuman is probabilistic, so these pin the shape of the curve it
// draws from rather than the draws themselves
let private hitChance (caution: uint) (hand: Hand) (others: Player list) (finished: Player list) : float =
    Strategy.HumanHitProbability caution { player with Hand = hand } others finished

[<Fact>]
let ``a human grows more reluctant as the hand grows into something worth losing`` () =
    let chances =
        [
            [ ValueCard Card.Two ]
            [ ValueCard Card.Nine ]
            [ ValueCard Card.Nine; ValueCard Card.Eight ]
            [ ValueCard Card.Twelve; ValueCard Card.Eleven; ValueCard Card.Ten ]
        ]
        |> List.map (fun hand -> hitChance 18u hand [] [])

    Assert.Equal<float list>(chances |> List.sortDescending, chances)
    Assert.True(List.head chances > 0.9, "a small hand is pushed on")
    Assert.True(List.last chances < 0.1, "a hand worth 33 is not")

[<Fact>]
let ``a human chases a flip7 once one is close`` () =
    // Five unique cards worth 38 would otherwise be well past baulking, so
    // the fixed chance of chasing the bonus is all that keeps them hitting
    let chasing = [
        ValueCard Card.Twelve
        ValueCard Card.Eleven
        ValueCard Card.Ten
        ValueCard Card.Three
        ValueCard Card.Two
    ]

    Assert.Equal(0.9, hitChance 18u chasing [] [], 1e-9)
    // One unique fewer is not close, and a hand worth 36 is baulked at
    Assert.True(hitChance 18u (List.take 4 chasing) [] [] < 0.1, "four uniques are not chased")

[<Fact>]
let ``a human takes more risk while somebody is running away with the game`` () =
    let hand = [ ValueCard Card.Nine; ValueCard Card.Eight ]
    let level = hitChance 18u hand [ other ] []
    let behind = hitChance 18u hand [ { other with FirmScore = 120u } ] []

    Assert.True(behind > level, "being well behind should embolden")

[<Fact>]
let ``a human's catch-up counts players who stood and stops growing forty behind`` () =
    let hand = [ ValueCard Card.Nine; ValueCard Card.Eight ]
    let bob banked = { other with FirmScore = banked }

    // Bob shows his 57 banked plus his One, 41 ahead of Alice's 17. Past forty
    // behind the catch-up stops at 10, so she baulks at 28 rather than at her
    // caution of 18, and hits with the logistic curve's 0.975 at 11 under
    let ahead = hitChance 18u hand [ bob 57u ] []
    Assert.Equal(0.975, ahead, 3)
    Assert.Equal(ahead, hitChance 18u hand [] [ bob 57u ], 1e-9)
    Assert.Equal(ahead, hitChance 18u hand [ bob 1000u ] [], 1e-9)

    // 20 behind earns half the catch-up, so she baulks at 23 and hits with
    // the curve's 0.881 at 6 under
    Assert.Equal(0.881, hitChance 18u hand [ bob 36u ] [], 3)

[<Fact>]
let ``a human feels safer holding a second chance`` () =
    let hand = [ ValueCard Card.Nine; ValueCard Card.Eight ]
    let bare = hitChance 18u hand [] []
    let held = hitChance 18u (ActionCard Card.SecondChance :: hand) [] []

    Assert.True(held > bare, "a second chance should embolden")

// ---- targeting ----

// The three candidates the engine would offer, distinct on every axis a policy
// looks at: Bob is furthest ahead, Carol is closest to busting (her six cards
// bust on 21 of the 94), and Dave is furthest behind
let private leader = Player.Make("Bob", Strategy.Random, 60u)

let private bustProne =
    Player.Make(
        "Carol",
        Strategy.Random,
        20u,
        values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
    )

let private laggard = Player.Make("Dave", Strategy.Random, 5u)

let private opponents = [ leader; bustProne; laggard ]

// A hand worth 50 that busts on 50 of the 94 cards, more than half and more
// than any opponent's
let private risky = [
    ValueCard Card.Eight
    ValueCard Card.Nine
    ValueCard Card.Ten
    ValueCard Card.Eleven
    ValueCard Card.Twelve
]

// Banks once a hit is close to a coin flip, and takes three forced flips only
// while a single one is nearly safe
let private greedy = Targeting.PlaysGreedily(0.5, 0.1)

// The name of the candidate a decider aims at. Only ChoosesRandomly reads the
// question's Random
let private aimedBy (decider: Strategy.Decider) targeting ask chooser candidates : string =
    (decider.Target (Random 1) targeting Seq.empty ask chooser candidates [] decks
     |> Async.RunSynchronously)
        .Name

let private aimedAt targeting ask chooser candidates =
    aimedBy Strategy.Decider.Default targeting ask chooser candidates

[<Fact>]
let ``a spiteful player freezes whoever is furthest ahead`` () =
    Assert.Equal("Bob", aimedAt Targeting.PlaysSpitefully Strategy.Ask.WhoToFreeze player opponents)

[<Fact>]
let ``a spiteful player deals three to whoever is closest to busting`` () =
    Assert.Equal("Carol", aimedAt Targeting.PlaysSpitefully Strategy.Ask.WhoReceivesDeal3 player opponents)

[<Fact>]
let ``spiteful and greedy players alike pass a second chance to whoever is furthest behind`` () =
    Assert.Equal("Dave", aimedAt Targeting.PlaysSpitefully Strategy.Ask.WhoReceivesSecondChance player opponents)
    Assert.Equal("Dave", aimedAt greedy Strategy.Ask.WhoReceivesSecondChance player opponents)

[<Fact>]
let ``spite never turns a freeze or a deal3 on the chooser while an opponent is left`` () =
    // Alice shows 150, her 100 banked plus the risky hand's 50, far ahead of
    // anyone, and she is likelier to bust than anyone
    let chooser = { player with FirmScore = 100u; Hand = risky }
    let candidates = chooser :: opponents

    Assert.Equal("Bob", aimedAt Targeting.PlaysSpitefully Strategy.Ask.WhoToFreeze chooser candidates)
    Assert.Equal("Carol", aimedAt Targeting.PlaysSpitefully Strategy.Ask.WhoReceivesDeal3 chooser candidates)
    // A greedy player who does not bank falls back on spite, with the same aim
    Assert.Equal("Bob", aimedAt (Targeting.PlaysGreedily(0.9, 0.1)) Strategy.Ask.WhoToFreeze chooser candidates)

[<Fact>]
let ``a greedy player freezes themselves once a hit is at least as likely to bust as it banks above, and otherwise freezes the leader``
    ()
    =
    let chooser = { player with Hand = risky }
    let candidates = chooser :: opponents

    // The risky hand busts more often than not, so it banks by freezing
    // itself...
    Assert.Equal("Alice", aimedAt greedy Strategy.Ask.WhoToFreeze chooser candidates)

    // ...unless it takes more than that to scare it, and then the freeze goes
    // to whoever is furthest ahead instead
    Assert.Equal("Bob", aimedAt (Targeting.PlaysGreedily(0.9, 0.1)) Strategy.Ask.WhoToFreeze chooser candidates)

[<Fact>]
let ``a greedy player keeps a deal3 while one card is no likelier to bust than it deals below, and otherwise deals it to the opponent closest to busting``
    ()
    =
    let candidates = player :: opponents

    // Alice's 15 busting cards of 94 are too rich for a player who keeps three
    // flips only below a tenth...
    Assert.Equal("Carol", aimedAt greedy Strategy.Ask.WhoReceivesDeal3 player candidates)

    // ...but worth keeping for a player happy to run at a quarter
    Assert.Equal("Alice", aimedAt (Targeting.PlaysGreedily(0.5, 0.25)) Strategy.Ask.WhoReceivesDeal3 player candidates)

    // Even when Alice is likelier to bust than anyone, the three flips go to
    // an opponent
    let chooser = { player with Hand = risky }
    Assert.Equal("Carol", aimedAt greedy Strategy.Ask.WhoReceivesDeal3 chooser (chooser :: opponents))

// A player frozen earlier in a deal3 still hands out the cards they set aside,
// and can no longer keep one for themselves
[<Fact>]
let ``a greedy chooser who can no longer keep the card aims it where spite would`` () =
    // The risky hand would have Alice freeze herself if she were a candidate
    let chooser = { player with Hand = risky }

    Assert.Equal("Bob", aimedAt greedy Strategy.Ask.WhoToFreeze chooser opponents)
    Assert.Equal("Carol", aimedAt greedy Strategy.Ask.WhoReceivesDeal3 chooser opponents)

// Each seed gives the question a Random of its own, so a policy that ignored
// its Random, or never reached one end of the candidates, would leave someone
// out of twenty picks
[<Fact>]
let ``choosing randomly reaches every candidate`` () =
    let picked =
        [ 1..20 ]
        |> List.map (fun seed ->
            (Strategy.Decider.Default.Target
                (Random seed)
                Targeting.ChoosesRandomly
                Seq.empty
                Strategy.Ask.WhoToFreeze
                player
                opponents
                []
                decks
             |> Async.RunSynchronously)
                .Name
        )
        |> set

    Assert.Equal<Set<string>>(set [ "Bob"; "Carol"; "Dave" ], picked)

// ---- teaching a decider ----

// A decider is taught Custom strategies and ChoosesExternally targeting by
// name, as a function or as a member, and passes everything else on to the
// decider it was taught from. The default refuses both by name.

// The Prompt strategy the lessons teach, which always stands
let private stands: Random -> Strategy.HitOrStandDecider =
    fun _ _ _ _ _ _ _ _ -> async.Return Strategy.Stand

// What a decider answers for Alice alone at her first turn
let private answeredBy (decider: Strategy.Decider) strategy =
    decider.HitOrStand (Random 1) strategy Seq.empty 1u 1u player [] [] decks
    |> Async.RunSynchronously

// Whether calling the decider raised the default's refusal of the given name
let private refuses (name: string) (ask: unit -> 'answer) : bool =
    let refusal = Record.Exception(fun () -> ask () |> ignore)
    refusal :? InvalidOperationException && refusal.Message.Contains name

[<Fact>]
let ``a decider answers the strategies it is taught, passes on the rest, and the default refuses an untaught name`` () =
    let piped =
        Strategy.Decider.Default |> Strategy.addHitOrStandStrategy "Prompt" stands

    let membered = Strategy.Decider.Default.addHitOrStandStrategy ("Prompt", stands)

    // Both ways of teaching must behave alike, so each assertion names its form
    for form, taught in [ "piped", piped; "member", membered ] do
        Assert.True(answeredBy taught (Strategy.Custom "Prompt") = Strategy.Stand, $"the {form} form answers Prompt")
        Assert.True(answeredBy taught Strategy.AlwaysHits = Strategy.Hit, $"the {form} form passes AlwaysHits on")

        Assert.True(
            refuses "Custom Other" (fun () -> answeredBy taught (Strategy.Custom "Other")),
            $"the {form} form's default refuses Custom Other"
        )

[<Fact>]
let ``a decider aims the targeting it is taught, passes on the rest, and the default refuses an untaught name`` () =
    let last: Random -> Strategy.TargetDecider =
        fun _ _ _ _ candidates _ _ -> async.Return(List.last candidates)

    let piped = Strategy.Decider.Default |> Strategy.addTargetingStrategy "Prompt" last
    let membered = Strategy.Decider.Default.addTargetingStrategy ("Prompt", last)

    let aimed taught targeting =
        aimedBy taught targeting Strategy.Ask.WhoToFreeze player opponents

    let spiteful = aimed Strategy.Decider.Default Targeting.PlaysSpitefully

    // Both ways of teaching must behave alike, so each assertion names its form
    for form, taught in [ "piped", piped; "member", membered ] do
        Assert.True(
            aimed taught (Targeting.ChoosesExternally "Prompt") = (List.last opponents).Name,
            $"the {form} form aims Prompt at the last candidate"
        )

        Assert.True(
            aimed taught Targeting.PlaysSpitefully = spiteful,
            $"the {form} form passes PlaysSpitefully on to the default"
        )

        Assert.True(
            refuses "ChoosesExternally Other" (fun () -> aimed taught (Targeting.ChoosesExternally "Other")),
            $"the {form} form's default refuses ChoosesExternally Other"
        )

// The terminal teaches its aiming and then its hit-or-stand prompt, and the
// sage benchmark teaches two targeting names, so each lesson has to keep the
// ones before it
[<Fact>]
let ``a decider taught several names answers every one of them`` () =
    let pick (choose: Player list -> Player) : Random -> Strategy.TargetDecider =
        fun _ _ _ _ candidates _ _ -> async.Return(choose candidates)

    let taught =
        Strategy.Decider.Default
        |> Strategy.addTargetingStrategy "First" (pick List.head)
        |> Strategy.addTargetingStrategy "Second" (pick List.last)
        |> Strategy.addHitOrStandStrategy "Prompt" stands

    let aimed targeting =
        aimedBy taught targeting Strategy.Ask.WhoToFreeze player opponents

    Assert.Equal("Bob", aimed (Targeting.ChoosesExternally "First"))
    Assert.Equal("Dave", aimed (Targeting.ChoosesExternally "Second"))
    Assert.Equal(Strategy.Stand, answeredBy taught (Strategy.Custom "Prompt"))

// ---- parsing ----

// The terminal reads every strategy and targeting policy it is given through
// TryParse, so a bad one has to come back as None

[<Fact>]
let ``a strategy string that does not parse is None rather than an exception`` () =
    Assert.Equal(None, Strategy.TryParse "Bogus")
    Assert.Equal(None, Strategy.TryParse "HitUntilScore")
    // A count outside uint's range overflows rather than failing to format
    Assert.Equal(None, Strategy.TryParse "HitUntilScore -1")
    Assert.Equal(None, Strategy.TryParse "HitUntilScore 99999999999")
    Assert.Equal(None, Strategy.TryParse "PlaysLikeAHuman -5")

[<Fact>]
let ``a strategy round-trips through its string form`` () =
    let property =
        Prop.forAll (Arb.fromGen genStrategy) (fun strategy -> Strategy.Parse(string strategy) = strategy)

    // A hundred cases miss a given strategy case in about one run in four
    // hundred
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

[<Fact>]
let ``a targeting string that does not parse is None rather than an exception`` () =
    Assert.Equal(None, Targeting.TryParse "Bogus")
    Assert.Equal(None, Targeting.TryParse "ChoosesExternally")
    Assert.Equal(None, Targeting.TryParse "PlaysGreedily 0.5")

[<Fact>]
let ``a targeting policy round-trips through its string form`` () =
    let property =
        Prop.forAll (Arb.fromGen genTargeting) (fun targeting -> Targeting.Parse(string targeting) = targeting)

    // Each of the four targeting cases is a quarter of the draws, so a hundred
    // cases all but never miss one
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)
