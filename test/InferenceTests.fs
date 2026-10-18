module InferenceTests

open Microsoft.FSharp.Reflection
open Xunit
open Flip7
open Support

// Every question a seeded game asks its seats, as its timeline shows them
let private observationsOfGame (seed: int) (lineup: Lineup) : Observation list =
    Timeline.SimulateWith (System.Random seed) lineup
    |> Observation.FromTimeline
    |> played

// Whether an observation is of a hit-or-stand decision rather than an aim
let private decides (observation: Observation) : bool = observation.Choice.IsChoice1Of2

// ---- likelihoods ----

// A table whose games run long: Ann draws to seven cards, so her turns run
// deep and her hands gather five or six distinct values
let private longTable: Lineup = [
    "Ann", Strategy.HitUntilNumCards 7u, Targeting.ChoosesRandomly
    "Bob", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
    "Cal", Strategy.AlwaysHits, Targeting.PlaysGreedily(0.3, 0.2)
    "Dee", Strategy.MaximizesExpectedValue, Targeting.ChoosesRandomly
]

// What the default decider answers for a strategy in the seat and at the table
// a decision was made at, rolling any dice on the given Random
let private answer (random: System.Random) (strategy: Strategy) (decision: Observation) : Strategy.HitOrStand =
    Strategy.Decider.Default.HitOrStand
        random
        strategy
        Seq.empty
        decision.Round
        decision.Turn
        decision.Player
        decision.OtherPlayers
        decision.FinishedPlayers
        (decision.Deck, decision.Discards)
    |> Async.RunSynchronously

[<Fact>]
let ``ProbabilityOfHit is 1 exactly where the default decider hits, for every deterministic strategy in the default grid``
    ()
    =
    let deterministic =
        Inference.DefaultCandidates
        |> List.where (fun strategy ->
            match strategy with
            | Strategy.RandomWithProbability _
            | Strategy.SoftHitUntilScore _
            | Strategy.PlaysLikeAHuman _ -> false
            | _ -> true
        )

    // The rarest road is a hand all but sure to bust, which makes the highest
    // bust-probability thresholds stand. Only the fifth game reaches one, with
    // a single card left in the deck, so all five are played
    let decisions =
        [ 1..5 ]
        |> List.collect (fun seed -> observationsOfGame seed longTable |> List.where decides)

    let strays = [
        for decision in decisions do
            for strategy in deterministic do
                // Deterministic strategies never draw, so any Random will do
                let expected =
                    match answer (System.Random 0) strategy decision with
                    | Strategy.Hit -> 1.0
                    | Strategy.Stand -> 0.0

                if Inference.ProbabilityOfHit strategy decision <> expected then
                    string strategy, decision.Name, decision.Round, decision.Turn
    ]

    Assert.Empty strays

    // A strategy that only ever answered one way was never tried at its
    // threshold. AlwaysHits and AlwaysStands answer one way by design.
    // StandsAfterTurn 1 always stands, since a player's first turn of a round
    // is a forced hit and their first question comes on their second. And
    // counted against the full deck, even the six highest values beside every
    // modifier bust a hand less than two times in three, so the naive
    // thresholds from 0.7 never stand. 0.6 can, but only for a lone player or
    // a hand holding three modifiers or more beside six high values, which
    // these games never reach
    let answersOneWay (strategy: Strategy) : bool =
        match strategy with
        | Strategy.AlwaysHits
        | Strategy.AlwaysStands -> true
        | Strategy.StandsAfterTurn turns -> turns = 1u
        | Strategy.HitUntilNaiveBustProbability threshold -> threshold > 0.5
        | _ -> false

    let decidedOneWay =
        deterministic
        |> List.where (fun strategy -> not (answersOneWay strategy))
        |> List.where (fun strategy ->
            let hits =
                decisions
                |> List.where (fun decision -> Inference.ProbabilityOfHit strategy decision = 1.0)

            hits.IsEmpty || hits.Length = decisions.Length
        )

    Assert.True(decidedOneWay.IsEmpty, $"%A{decidedOneWay} never decided both ways: pick new seeds")

// RandomWithProbability, SoftHitUntilScore and PlaysLikeAHuman each roll a
// single NextDouble per question and hit when it falls below their chance. So
// a decider rolling on Random seed answers exactly as that seed's first roll
// against Inference's chance says, and a hundred seeds roll on both sides of
// any chance away from the extremes
[<Fact>]
let ``ProbabilityOfHit is the chance the default decider hits, for every strategy that rolls dice`` () =
    let decisions = observationsOfGame 1 longTable |> List.where decides

    let dice = [
        Strategy.RandomWithProbability 0.3
        Strategy.SoftHitUntilScore(16u, 2.0)
        Strategy.PlaysLikeAHuman 18u
    ]

    // The curves are tried where they bend, and not only where they all but
    // certainly hit or stand
    for curve in List.tail dice do
        let bends =
            decisions
            |> List.exists (fun decision ->
                let chance = Inference.ProbabilityOfHit curve decision
                0.2 < chance && chance < 0.8
            )

        Assert.True(bends, $"{curve} is never between 0.2 and 0.8 to hit: pick a new seed")

    let strays = [
        for strategy in dice do
            for decision in decisions do
                let chance = Inference.ProbabilityOfHit strategy decision

                for seed in 1..100 do
                    let expected =
                        if (System.Random seed).NextDouble() < chance then
                            Strategy.Hit
                        else
                            Strategy.Stand

                    if answer (System.Random seed) strategy decision <> expected then
                        string strategy, decision.Name, decision.Round, decision.Turn, seed
    ]

    Assert.Empty strays

// ---- fitting ----

// Mom hits while one more card is less than 0.4 likely to bust her. She holds
// a One against a ten-card deck of Ones and Twos, so the number of Ones in it
// dials the chance to any tenth, and she decides once at each. Kid is still in
// the round, so that she is not alone, where a deal3 would weigh on her chance
let private momsDecisions: Observation list =
    let mom = Player.Make("Mom", Strategy.Random, hand = [ ValueCard Card.One ])
    let kid = Player.Make("Kid", Strategy.Random, hand = [ ValueCard Card.Two ])

    [ 0..10 ]
    |> List.map (fun ones -> {
        Name = "Mom"
        Choice =
            Choice1Of2(
                if float ones / 10.0 < 0.4 then
                    Strategy.Hit
                else
                    Strategy.Stand
            )
        Round = 1u
        Turn = 2u
        Player = mom
        OtherPlayers = [ kid ]
        FinishedPlayers = []
        Deck =
            deckOf (
                List.replicate ones (ValueCard Card.One)
                @ List.replicate (10 - ones) (ValueCard Card.Two)
            )
        Discards = Deck.Empty
    })

[<Fact>]
let ``Fit names a bust-probability threshold from clean decisions with little doubt`` () =
    let model = Inference.Fit momsDecisions |> List.exactlyOne

    Assert.Equal("Mom", model.Name)
    Assert.Equal(List.length momsDecisions, model.Observations)
    Assert.Equal(Strategy.HitUntilBustProbability 0.4, Inference.MostLikely model)

    // Named by the evidence rather than by the order of the grid. The
    // posterior is a distribution over the whole grid, without which a high
    // head would say nothing, since an unnormalised posterior's head is one
    let head = snd (List.head model.Posterior)
    Assert.True(head > 0.85, $"named at only {head}")
    Assert.Equal(Inference.DefaultCandidates.Length, model.Posterior.Length)
    Assert.Equal(1.0, model.Posterior |> List.sumBy snd, 1e-12)

// Standing with no Ones in the deck, when nothing can bust her, is as out of
// character as Mom gets. Flipping her decision at two Ones or at five instead
// would fit a threshold of 0.2 or 0.6 just as well as 0.4, and tie them
[<Fact>]
let ``one out-of-character decision does not rule out the strategy behind the rest`` () =
    let decisions =
        momsDecisions
        |> List.mapi (fun ones decision ->
            if ones = 0 then
                { decision with Choice = Choice1Of2 Strategy.Stand }
            else
                decision
        )

    let model = Inference.Fit decisions |> List.exactlyOne
    Assert.Equal(Strategy.HitUntilBustProbability 0.4, Inference.MostLikely model)

// The grids are lists rather than matches, so nothing warns when a strategy or
// a targeting the engine plays is left out of them, and a player who plays it
// would be fitted as something else
[<Fact>]
let ``the default grids hold every strategy and targeting the engine can evaluate`` () =
    let casesBut (decidedExternally: string) (union: System.Type) : Set<string> =
        FSharpType.GetUnionCases union
        |> Array.map (fun case -> case.Name)
        |> Set.ofArray
        |> Set.remove decidedExternally

    let caseOf (union: System.Type) (value: obj) : string =
        let case, _ = FSharpValue.GetUnionFields(value, union)
        case.Name

    Assert.Equal<Set<string>>(
        casesBut "Custom" typeof<Strategy>,
        Inference.DefaultCandidates
        |> List.map (box >> caseOf typeof<Strategy>)
        |> Set.ofList
    )

    Assert.Equal<Set<string>>(
        casesBut "ChoosesExternally" typeof<Targeting>,
        Inference.DefaultTargetings
        |> List.map (box >> caseOf typeof<Targeting>)
        |> Set.ofList
    )

// ---- evidence ----

// Three seats whose game mixes hit-or-stand decisions with aims at more than
// one candidate
let private threeSeats: Lineup = [
    "Ann", Strategy.AlwaysHits, Targeting.PlaysSpitefully
    "Bob", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
    "Carol", Strategy.AlwaysHits, Targeting.PlaysGreedily(0.3, 0.2)
]

// Sage folds each decision into a player's evidence as it is made, while a
// study scores a whole archive at once. They agree exactly, since both add the
// same terms in the same order
[<Fact>]
let ``evidence folded one decision at a time is the evidence scored at once`` () =
    let decisions =
        observationsOfGame 3 threeSeats
        |> List.where (fun observation -> observation.Name = "Bob" && decides observation)

    Assert.True(decisions.Length > 1, "Bob decided at most once, so nothing was folded: pick a new seed")

    Assert.Equal(
        Inference.Evidence "Bob" decisions,
        decisions
        |> List.map (fun decision -> Inference.Evidence "Bob" [ decision ])
        |> List.reduce Inference.Combine
    )

// FromTimeline hands Inference a game's aims among its hit-or-stand
// decisions. Each side reads only its own kind, so reading the whole stream is
// reading that kind alone, and a player seen only aiming gets no model of how
// they hit, nor one seen only deciding a model of how they aim
[<Fact>]
let ``Inference reads only its own kind of observation from a game's stream`` () =
    let observations = observationsOfGame 3 threeSeats
    let decisions, aims = observations |> List.partition decides

    Assert.True(not decisions.IsEmpty, "nobody decided: pick a new seed")

    Assert.True(
        aims |> List.exists (fun aim -> aim.OtherPlayers.Length > 1),
        "no aim had more than one candidate: pick a new seed"
    )

    // The fits and Accuracy filter out the other kind
    Assert.Equal<PlayerModel list>(Inference.Fit decisions, Inference.Fit observations)
    Assert.Equal<AimingModel list>(Inference.FitAims aims, Inference.FitAims observations)
    Assert.Equal(Inference.Accuracy Strategy.AlwaysHits decisions, Inference.Accuracy Strategy.AlwaysHits observations)
    Assert.Empty(Inference.Fit aims)
    Assert.Empty(Inference.FitAims decisions)

    // So does each player's evidence
    for name in [ "Ann"; "Bob"; "Carol" ] do
        let theirs = observations |> List.where (fun observation -> observation.Name = name)
        let decided, aimed = theirs |> List.partition decides

        Assert.Equal(Inference.Evidence name decided, Inference.Evidence name theirs)
        Assert.Equal(Inference.AimEvidence name aimed, Inference.AimEvidence name theirs)

    // And each kind's likelihood gives the other kind one
    for aim in aims do
        Assert.Equal(1.0, Inference.Likelihood 0.1 Strategy.AlwaysStands aim)

    for decision in decisions do
        Assert.Equal(1.0, Inference.AimLikelihood 0.1 (Aiming.Plays Targeting.PlaysSpitefully) decision)
