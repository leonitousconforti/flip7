module AnalysisTests

open Xunit
open Flip7
open Flip7.Analysis

let private player (name: string) (hand: Hand) : Player = Player.Make(name, Strategy.Random, hand = hand)

let private instant (event: Event) (players: Player list) : Instant = {
    Event = event
    Players = players
    Deck = Deck.Full
    Discards = Deck.Empty
}

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
            | Some(points, card) -> build (remaining - points) (available |> List.except [ points, card ]) (card :: hand)

    if score = 0u then
        [ ValueCard Card.Zero ]
    else
        build score valueByPoints []

[<Fact>]
let ``Dealt first cards are not decisions but later hits and stands are`` () =
    let timeline = [
        instant (Drew("A", ValueCard Card.Five)) [ player "B" []; player "A" [ ValueCard Card.Five ] ]
        instant (Drew("B", ValueCard Card.Seven)) [
            player "A" [ ValueCard Card.Five ]
            player "B" [ ValueCard Card.Seven ]
        ]
        instant (Drew("A", ValueCard Card.Three)) [
            player "B" [ ValueCard Card.Seven ]
            player "A" [ ValueCard Card.Three; ValueCard Card.Five ]
        ]
        instant (Stood "B") [
            player "A" [ ValueCard Card.Three; ValueCard Card.Five ]
            player "B" [ ValueCard Card.Seven ]
        ]
    ]

    let observations = timeline |> Seq.ofList |> Observation.FromTimeline

    Assert.Equal(2, List.length observations)

    Assert.Equal("A", observations[0].Name)
    Assert.Equal(Strategy.Hit, observations[0].Choice)
    Assert.Equal<Hand>([ ValueCard Card.Five ], observations[0].Player.Hand)

    Assert.Equal("B", observations[1].Name)
    Assert.Equal(Strategy.Stand, observations[1].Choice)
    Assert.Equal<Hand>([ ValueCard Card.Seven ], observations[1].Player.Hand)

[<Fact>]
let ``Drawing an action card counts as the drawer choosing to hit`` () =
    let timeline = [
        instant (Drew("A", ValueCard Card.Five)) [
            player "A" [ ValueCard Card.Five ]
            player "B" [ ValueCard Card.Seven ]
        ]
        instant (Froze("A", "B")) [
            player "A" [ ValueCard Card.Five ]
            player "B" [ ActionCard Card.Freeze; ValueCard Card.Seven ]
        ]
    ]

    let observations = timeline |> Seq.ofList |> Observation.FromTimeline
    let observation = Assert.Single observations

    Assert.Equal("A", observation.Name)
    Assert.Equal(Strategy.Hit, observation.Choice)
    Assert.Equal<Hand>([ ValueCard Card.Five ], observation.Player.Hand)

[<Fact>]
let ``ProbabilityOfHit matches DecideWith for deterministic strategies`` () =
    let random = System.Random 11

    let observations =
        Timeline.SimulateWith random [ "A", HitUntilScore 20u; "B", HitUntilNumCards 4u ] None None None None
        |> Observation.FromTimeline

    Assert.NotEmpty observations

    let deterministic = [
        AlwaysHits
        AlwaysStands
        HitUntilScore 15u
        HitUntilNumCards 3u
        HitUntilBustProbability 0.3
        HitUntilNaiveBustProbability 0.3
        HitUntilTotal 150u
        HitUntilUniqueValues 4u
        ChasesFlip7(18u, 5u)
        EmboldenedBySecondChance 18u
        HitWhileBehindLeader 10u
        StandsAfterTurn 10u
        MaximizesExpectedValue
    ]

    for observation in observations do
        for strategy in deterministic do
            let expected =
                match
                    Strategy.DecideWith
                        random
                        strategy
                        observation.Session
                        observation.Player
                        observation.OtherPlayers
                        observation.Decks
                with
                | Strategy.Hit -> 1.0
                | Strategy.Stand -> 0.0

            Assert.Equal(expected, Inference.ProbabilityOfHit strategy observation)

[<Fact>]
let ``Players who stood busted or were frozen are excluded from other players`` () =
    let a = player "A" [ ValueCard Card.Five ]
    let b = player "B" [ ValueCard Card.Seven ]
    let c = player "C" [ ValueCard Card.Two; ValueCard Card.Two ]
    let d = player "D" [ ValueCard Card.Nine ]
    let d' = player "D" [ ValueCard Card.Three; ValueCard Card.Nine ]
    let frozen = player "D" [ ActionCard Card.Freeze; ValueCard Card.Three; ValueCard Card.Nine ]

    let timeline = [
        instant (Busted("C", ValueCard Card.Two)) [ b; d; a; c ]
        instant (Stood "B") [ d; a; b; c ]
        instant (Drew("D", ValueCard Card.Three)) [ a; d'; b; c ]
        instant (Froze("A", "D")) [ a; frozen; b; c ]
        instant (Drew("A", ValueCard Card.Six)) [ player "A" [ ValueCard Card.Six; ValueCard Card.Five ]; frozen; b; c ]
    ]

    let observations = timeline |> Seq.ofList |> Observation.FromTimeline
    let othersOf (observation: Observation) = observation.OtherPlayers |> List.map (fun player -> player.Name)

    Assert.Equal(4, List.length observations)

    // All instants are in one round, so sessions count up from the second
    Assert.Equal<uint list>([ 1u; 2u; 3u; 4u ], observations |> List.map (fun observation -> observation.Session))

    // B stood while C had already busted, so C is excluded from B's others
    Assert.Equal("B", observations[0].Name)
    Assert.Equal<string list>([ "D"; "A" ], othersOf observations[0])

    // D hit after B stood, so only A remains in D's others
    Assert.Equal("D", observations[1].Name)
    Assert.Equal<string list>([ "A" ], othersOf observations[1])

    // A hit and drew the freeze; D was still active when A decided
    Assert.Equal("A", observations[2].Name)
    Assert.Equal<string list>([ "D" ], othersOf observations[2])

    // After freezing D, A is the last active player
    Assert.Equal("A", observations[3].Name)
    Assert.Equal<string list>([], othersOf observations[3])

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
                Session = 0u
                Player = { Name = "Dad"; FirmScore = 0u; Hand = hand }
                OtherPlayers = []
                Decks = Deck.Full, Deck.Empty
            }
        )

    let model = Inference.Fit observations |> List.exactlyOne

    Assert.Equal("Dad", model.Name)
    Assert.Equal(31, model.Observations)
    Assert.Equal(HitUntilScore 20u, Inference.MostLikely model)

[<Fact>]
let ``Fit recovers a bust-probability threshold from clean decisions`` () =
    let other: Strategy.StrategyPlayer = {
        Name = "Kid"
        FirmScore = 0u
        Hand = [ ValueCard Card.Two ]
    }

    // A hand of a One against a 10-card deck with a varying number of Ones
    // dials the bust probability to any tenth
    let observations =
        [ 0u .. 10u ]
        |> List.map (fun ones ->
            let deck = Map.ofList [ ValueCard Card.One, ones; ValueCard Card.Two, 10u - ones ]

            {
                Name = "Mom"
                Choice = (if float ones / 10.0 < 0.4 then Strategy.Hit else Strategy.Stand)
                Session = 0u
                Player = { Name = "Mom"; FirmScore = 0u; Hand = [ ValueCard Card.One ] }
                OtherPlayers = [ other ]
                Decks = deck, Deck.Empty
            }
        )

    let model = Inference.Fit observations |> List.exactlyOne
    Assert.Equal(HitUntilBustProbability 0.4, Inference.MostLikely model)
