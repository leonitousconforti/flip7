module InferenceTests

open FSharp.Control
open Xunit
open Flip7

let private player (name: string) (hand: Hand) : Player =
    Player.Make(name, Strategy.Random, hand = hand)

let private valueByPoints: (uint * Card) list = [
    12u, ValueCard Card.Twelve
    11u, ValueCard Card.Eleven
    10u, ValueCard Card.Ten
    9u, ValueCard Card.Nine
    8u, ValueCard Card.Eight
    7u, ValueCard Card.Seven
    6u, ValueCard Card.Six
    5u, ValueCard Card.Five
    4u, ValueCard Card.Four
    3u, ValueCard Card.Three
    2u, ValueCard Card.Two
    1u, ValueCard Card.One
]

/// Builds a non-empty hand of distinct value cards worth exactly the given
/// score, so tests can dial in any hand score they need.
let private handWorth (score: uint) : Hand =
    let rec build (remaining: uint) (available: (uint * Card) list) (hand: Hand) : Hand =
        if remaining = 0u then
            hand
        else
            match available |> List.tryFind (fun (points, _) -> points <= remaining) with
            | None -> failwith $"cannot build a hand worth {score}"
            | Some(points, card) ->
                build (remaining - points) (available |> List.except [ points, card ]) (card :: hand)

    if score = 0u then
        [ ValueCard Card.Zero ]
    else
        build score valueByPoints []

[<Fact>]
let ``ProbabilityOfHit matches DecideHitOrStandWith for deterministic strategies`` () =
    let random = System.Random 11

    let observations =
        Timeline.SimulateWith random [
            "A", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
            "B", Strategy.HitUntilNumCards 4u, Targeting.PlaysSpitefully
        ]
        |> Observation.FromTimeline
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    Assert.NotEmpty observations

    let deterministic = [
        Strategy.AlwaysHits
        Strategy.AlwaysStands
        Strategy.HitUntilScore 15u
        Strategy.HitUntilNumCards 3u
        Strategy.HitUntilBustProbability 0.3
        Strategy.HitUntilNaiveBustProbability 0.3
        Strategy.HitUntilTotal 150u
        Strategy.HitUntilUniqueValues 4u
        Strategy.ChasesFlip7(18u, 5u)
        Strategy.EmboldenedBySecondChance 18u
        Strategy.HitWhileBehindLeader 10u
        Strategy.StandsAfterTurn 10u
        Strategy.MaximizesExpectedValue
    ]

    for observation in observations do
        for strategy in deterministic do
            let expected =
                match
                    Strategy.DecideHitOrStandWith
                        random
                        strategy
                        observation.Round
                        observation.Turn
                        observation.Player
                        observation.OtherPlayers
                        observation.FinishedPlayers
                        (observation.Deck, observation.Discards)
                    |> Async.RunSynchronously
                with
                | Strategy.Hit -> 1.0
                | Strategy.Stand -> 0.0

            Assert.Equal(expected, Inference.ProbabilityOfHit strategy observation)

[<Fact>]
let ``Fit recovers a hit-until-score threshold from clean decisions`` () =
    let observations =
        [ 0u .. 30u ]
        |> List.map (fun score ->
            let hand = handWorth score
            Assert.Equal(score, Hand.Score hand)

            {
                Name = "Dad"
                Choice = (if score < 20u then Strategy.Hit else Strategy.Stand)
                Round = 1u
                Turn = 2u
                Player = player "Dad" hand
                OtherPlayers = []
                FinishedPlayers = []
                Deck = Deck.Full
                Discards = Deck.Empty
            }
        )

    let model = Inference.Fit observations |> List.exactlyOne

    Assert.Equal("Dad", model.Name)
    Assert.Equal(31, model.Observations)
    Assert.Equal(Strategy.HitUntilScore 20u, Inference.MostLikely model)

[<Fact>]
let ``Fit recovers a bust-probability threshold from clean decisions`` () =
    let other = player "Kid" [ ValueCard Card.Two ]

    // A hand of a One against a 10-card deck with a varying number of Ones
    // dials the bust probability to any tenth
    let observations =
        [ 0u .. 10u ]
        |> List.map (fun ones ->
            let deck = Map.ofList [ ValueCard Card.One, ones; ValueCard Card.Two, 10u - ones ]

            {
                Name = "Mom"
                Choice =
                    (if float ones / 10.0 < 0.4 then
                         Strategy.Hit
                     else
                         Strategy.Stand)
                Round = 1u
                Turn = 2u
                Player = player "Mom" [ ValueCard Card.One ]
                OtherPlayers = [ other ]
                FinishedPlayers = []
                Deck = deck
                Discards = Deck.Empty
            }
        )

    let model = Inference.Fit observations |> List.exactlyOne
    Assert.Equal(Strategy.HitUntilBustProbability 0.4, Inference.MostLikely model)
