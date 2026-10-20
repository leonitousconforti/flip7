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
    // A count is read only as a uint in plain digits, so a sign is refused,
    // and so is a count outside uint's range rather than overflowing
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

    // Taking sides is strict: names quoted and escaped, the favoured before the
    // opposed, and always an otherwise that parses
    let malformed = [
        "TakesSides"
        "TakesSides for \"Carol\""
        "TakesSides for Carol otherwise PlaysSpitefully"
        "TakesSides for \"Carol otherwise PlaysSpitefully"
        "TakesSides against \"Bob\" for \"Carol\" otherwise PlaysSpitefully"
        "TakesSides for \"Carol\" and against \"Bob\" otherwise PlaysSpitefully"
        "TakesSides for \"Carol\"otherwise PlaysSpitefully"
        "TakesSides for \"Ca\\rol\" otherwise PlaysSpitefully"
        "TakesSides for \"Carol\\\" otherwise PlaysSpitefully"
        "TakesSides otherwise "
        "TakesSides otherwise Bogus"
        "TakesSidesotherwise PlaysSpitefully"
    ]

    for text in malformed do
        Assert.True(Targeting.TryParse text = None, $"{text} parsed")

[<Fact>]
let ``a targeting policy round-trips through its string form`` () =
    let property =
        Prop.forAll (Arb.fromGen genTargeting) (fun targeting -> Targeting.Parse(string targeting) = targeting)

    // Each of the five targeting cases is at least a seventh of the draws,
    // taking sides over awkward names a quarter, and taking them three deep a
    // twelfth, so a hundred cases all but never miss one
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

// Numbers are read strictly, and the same on .NET and in JavaScript. Each of
// these floats is refused, a fullwidth one and an infinity sign among them,
// though .NET's own parser reads "1,5" as 15 and "0.5\n" as a half, and
// Fable's reads "0x10" as 16 and "1_0" as 10
let private malformedFloats = [
    ""
    "-"
    "."
    "+0.5"
    "--1"
    "1,5"
    "0x10"
    "1_0"
    " 0.5"
    "0.5\n"
    "0.5\t"
    "１"
    "1e"
    "1e+"
    "e5"
    "1.2.3"
    "1e5.5"
    "nan"
    "inf"
    "Infinity0"
    "∞"
]

// A count is read only as a uint in plain digits, so signs, spaces, leading
// zeros, other notations, other digits and anything outside uint's range are
// all refused
let private malformedCounts = [
    "-1"
    "-0"
    "+1"
    "01"
    " 1"
    "1\n"
    "0x10"
    "1_0"
    "1,000"
    "1.0"
    "1e3"
    "１"
    "4294967296"
    "99999999999"
]

// TryParse refuses the string, and Parse raises its own FormatException naming
// it, rather than an OverflowException or the error of a platform's parser
let private refusedBy (tryParse: string -> 'T option) (parse: string -> 'T) (kind: string) (text: string) =
    Assert.True((tryParse text).IsNone, $"{text} parsed")
    let error = Assert.Throws<FormatException>(fun () -> parse text |> ignore)
    Assert.Equal($"Invalid {kind} string: {text}", error.Message)

[<Fact>]
let ``a malformed number is refused with the parser's own FormatException`` () =
    let strategy = refusedBy Strategy.TryParse Strategy.Parse "strategy"
    let targeting = refusedBy Targeting.TryParse Targeting.Parse "targeting"

    // The strings that a review of the JavaScript found throwing from TryParse
    targeting "PlaysGreedily a b"
    targeting "TakesSides otherwise PlaysGreedily a b"

    for number in malformedFloats do
        strategy $"RandomWithProbability {number}"
        strategy $"SoftHitUntilScore 20 {number}"
        targeting $"PlaysGreedily {number} 0.1"
        targeting $"TakesSides for \"Carol\" otherwise PlaysGreedily 0.5 {number}"

    for count in malformedCounts do
        strategy $"HitUntilScore {count}"
        strategy $"ChasesFlip7 3 {count}"
        strategy $"SoftHitUntilScore {count} 0.5"

// Whether two doubles are the same: by their bits, so that -0 is not 0, or by
// both being NaN, since a NaN is not equal even to itself and its bits need
// not survive being written
let private sameDouble (read: float) (value: float) =
    if Double.IsNaN value then
        Double.IsNaN read
    else
        BitConverter.DoubleToInt64Bits read = BitConverter.DoubleToInt64Bits value

// Every form either platform writes is read, and to the same double on both:
// .NET writes 1E-05 where JavaScript writes 1e-5 and 1e+21, and a point may
// have digits on only one side. An exponent past the largest double reads as
// Infinity, and a count reaches the top of uint's range
[<Fact>]
let ``a number in any form either platform writes parses to its value`` () =
    let accepted = [
        "1E-05", 1e-5
        "1e-5", 1e-5
        "5.", 5.0
        ".5", 0.5
        "-0", -0.0
        "1e+21", 1e21
        "5E-324", Double.Epsilon
        "1.7976931348623157E+308", Double.MaxValue
        "NaN", nan
        "Infinity", infinity
        "-Infinity", -infinity
        "1e999", infinity
    ]

    for text, value in accepted do
        match
            Strategy.TryParse $"SoftHitUntilScore 20 {text}",
            Targeting.TryParse $"TakesSides otherwise PlaysGreedily {text} 0.25"
        with
        | Some(Strategy.SoftHitUntilScore(20u, temperature)),
          Some(Targeting.TakesSides(None, None, Targeting.PlaysGreedily(banksAbove, deal3sBelow))) ->
            Assert.True(
                sameDouble temperature value
                && sameDouble banksAbove value
                && deal3sBelow = 0.25,
                $"{text} read as {temperature}, {banksAbove} and {deal3sBelow}"
            )
        | strategy, targeting -> Assert.Fail $"{text} parsed as {strategy} and {targeting}"

    Assert.Equal(Some(Strategy.ChasesFlip7(0u, UInt32.MaxValue)), Strategy.TryParse "ChasesFlip7 0 4294967295")

// Any double at all, from a random 64-bit pattern. The exponent is left as
// drawn, or cleared for the zeros and subnormals, or filled for the
// infinities and NaN, and the fraction is left as drawn or cleared, so the
// zeros and the infinities are each a twelfth of the draws and the subnormals
// and NaN each a quarter
let private genAnyDouble: Gen<float> = gen {
    let! quarters = Gen.listOfLength 4 (Gen.choose (0, 0xFFFF))
    let! exponent = Gen.elements [ None; Some 0L; Some 0x7FFL ]
    let! clearsFraction = Gen.frequency [ 3, Gen.constant false; 1, Gen.constant true ]
    let drawn =
        quarters |> List.fold (fun bits quarter -> (bits <<< 16) ||| int64 quarter) 0L

    let withExponent =
        match exponent with
        | Some exponent -> (drawn &&& ~~~(0x7FFL <<< 52)) ||| (exponent <<< 52)
        | None -> drawn

    let bits =
        if clearsFraction then
            withExponent &&& ~~~((1L <<< 52) - 1L)
        else
            withExponent

    return BitConverter.Int64BitsToDouble bits
}

[<Fact>]
let ``any double round-trips through a strategy and a targeting string`` () =
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.zip genAnyDouble genAnyDouble))
            (fun (first, second) ->
                match
                    Strategy.Parse(string (Strategy.RandomWithProbability first)),
                    Targeting.Parse(string (Targeting.PlaysGreedily(first, second)))
                with
                | Strategy.RandomWithProbability read, Targeting.PlaysGreedily(banksAbove, deal3sBelow) ->
                    sameDouble read first
                    && sameDouble banksAbove first
                    && sameDouble deal3sBelow second
                | _ -> false
            )

    // Two hundred cases miss the zeros or the infinities in about one run in
    // eighteen million
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 200, property)

// A chain of TakesSides holds at most three, counting the outermost. Three are
// written and read back as they were and play to the end of the chain: Bob is
// favoured and so left out of a freeze, Eve is not at the table, and the
// third opposes Dave, where spite among the rest would freeze Carol. A fourth
// is refused by Parse, though ToString still writes it, and by the engine's
// decider, which judges even a chain a million deep at once. Parse refuses the
// fourth the moment it begins: the one below is malformed too, with its name
// unquoted, so only a parser that stops there names the limit. ToString writes
// a chain far past the limit, a link at a time, where writing it by recursion
// would overflow the stack
[<Fact>]
let ``a targeting takes sides three deep, but not four`` () =
    let spite = Targeting.PlaysSpitefully

    let three =
        Targeting.TakesSides(
            Some "Bob",
            None,
            Targeting.TakesSides(Some "Eve", None, Targeting.TakesSides(None, Some "Dave", spite))
        )

    let written =
        "TakesSides for \"Bob\" otherwise TakesSides for \"Eve\" otherwise TakesSides against \"Dave\" otherwise PlaysSpitefully"

    Assert.Equal(written, string three)
    Assert.Equal(Some three, Targeting.TryParse written)
    Assert.Equal("Dave", aimedAt three Strategy.Ask.WhoToFreeze player opponents)
    Assert.Equal("Bob", aimedAt three Strategy.Ask.WhoReceivesSecondChance player opponents)

    let four = Targeting.TakesSides(None, None, three)
    Assert.Equal("TakesSides otherwise " + written, string four)
    Assert.Equal(None, Targeting.TryParse(string four))

    let early =
        Assert.Throws<FormatException>(fun () ->
            Targeting.Parse(String.replicate 3 "TakesSides otherwise " + "TakesSides for Carol")
            |> ignore
        )

    Assert.Contains("more than 3 deep", early.Message)

    let refused =
        Assert.Throws<InvalidOperationException>(fun () ->
            aimedAt four Strategy.Ask.WhoToFreeze player opponents |> ignore
        )

    Assert.Contains("at most 3 deep", refused.Message)

    let mutable deep = spite

    for _ in 1..1_000_000 do
        deep <- Targeting.TakesSides(None, None, deep)

    Assert.Throws<InvalidOperationException>(fun () -> aimedAt deep Strategy.Ask.WhoToFreeze player opponents |> ignore)
    |> ignore

    let mutable farPast = spite

    for _ in 1..10_000 do
        farPast <- Targeting.TakesSides(None, None, farPast)

    Assert.Equal(10_000 * "TakesSides otherwise ".Length + "PlaysSpitefully".Length, (string farPast).Length)

// Parse reads a string once, with a running index, and unescapes a name in one
// pass, so neither a chain far too deep nor a long name costs more than reading
// it. A chain of two thousand is refused at its fourth link, two long runs of
// escapes never close their quote, and a name of 240,000 characters, a third
// of them escaped, is read back. Each parse allocates a few megabytes at most,
// so the bound of 32 megabytes fails a parser that copied what is left at
// every link, which allocates 168 megabytes for this chain, or built a name a
// character at a time, which allocates gigabytes for these names. The sizes
// keep even such a parser's live memory under a quarter of a gigabyte, so a
// regression fails rather than fills the machine
[<Fact>]
let ``parsing a targeting costs no more than reading it`` () =
    let parsed (text: string) =
        let before = GC.GetAllocatedBytesForCurrentThread()
        let targeting = Targeting.TryParse text
        let allocated = GC.GetAllocatedBytesForCurrentThread() - before
        Assert.True(allocated < 32_000_000L, $"parsing {text.Length} characters allocated {allocated} bytes")
        targeting

    Assert.Equal(None, parsed (String.replicate 2_000 "TakesSides otherwise " + "PlaysSpitefully"))

    Assert.Equal(
        None,
        parsed (
            "TakesSides for \""
            + String.replicate 100_000 "\\\""
            + " otherwise PlaysSpitefully"
        )
    )

    Assert.Equal(None, parsed ("TakesSides against \"" + String.replicate 100_000 "\\\\"))

    let long =
        Targeting.TakesSides(Some(String.replicate 40_000 "ab\"c\\d"), Some "Bob", Targeting.PlaysSpitefully)

    Assert.Equal(Some long, parsed (string long))
