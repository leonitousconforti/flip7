module ObservationTests

open FSharp.Control
open Xunit
open Flip7

let private player (name: string) (hand: Hand) : Player =
    Player.Make(name, Strategy.Random, hand = hand)

let private instant (event: Event) (players: Player list) : Instant = {
    Event = event
    Players = players
    Deck = Deck.Full
    Discards = Deck.Empty
}

let private observe (timeline: Instant list) : Observation list =
    timeline
    |> AsyncSeq.ofSeq
    |> Observation.FromTimeline
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously

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

    let observations = observe timeline

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

    let observations = observe timeline
    let observation = Assert.Single observations

    Assert.Equal("A", observation.Name)
    Assert.Equal(Strategy.Hit, observation.Choice)
    Assert.Equal<Hand>([ ValueCard Card.Five ], observation.Player.Hand)

[<Fact>]
let ``A forced first hit is not a decision even when earlier cards filled the hand`` () =
    let timeline = [
        // A's forced first hit draws a deal3 aimed at B, so B holds cards
        // before their own first turn while A holds none after theirs
        instant (Dealt3("A", "B", [ ValueCard Card.Two; ValueCard Card.Nine ])) [
            player "B" [ ValueCard Card.Nine; ValueCard Card.Two ]
            player "A" []
        ]
        // B's first turn is still a forced hit, not a decision
        instant (Drew("B", ValueCard Card.Five)) [
            player "A" []
            player "B" [ ValueCard Card.Five; ValueCard Card.Nine; ValueCard Card.Two ]
        ]
        // A's second turn is a real decision even though their hand is empty
        instant (Drew("A", ValueCard Card.Six)) [
            player "B" [ ValueCard Card.Five; ValueCard Card.Nine; ValueCard Card.Two ]
            player "A" [ ValueCard Card.Six ]
        ]
    ]

    let observations = observe timeline
    let observation = Assert.Single observations

    Assert.Equal("A", observation.Name)
    Assert.Equal(Strategy.Hit, observation.Choice)
    Assert.Equal(2u, observation.Turn)
    Assert.Equal<Hand>([], observation.Player.Hand)

[<Fact>]
let ``Players who stood busted or were frozen are excluded from other players`` () =
    let a = player "A" [ ValueCard Card.Five ]
    let b = player "B" [ ValueCard Card.Seven ]
    let c = player "C" [ ValueCard Card.Two; ValueCard Card.Two ]
    let d = player "D" [ ValueCard Card.Nine ]
    let d' = player "D" [ ValueCard Card.Three; ValueCard Card.Nine ]

    let frozen =
        player "D" [ ActionCard Card.Freeze; ValueCard Card.Three; ValueCard Card.Nine ]

    let timeline = [
        // The opening deal: every player's first turn is a forced hit
        instant (Drew("A", ValueCard Card.Five)) [ player "B" []; player "C" []; player "D" []; a ]
        instant (Drew("B", ValueCard Card.Seven)) [ player "C" []; player "D" []; a; b ]
        instant (Drew("C", ValueCard Card.Two)) [ player "D" []; a; b; player "C" [ ValueCard Card.Two ] ]
        instant (Drew("D", ValueCard Card.Nine)) [ a; b; player "C" [ ValueCard Card.Two ]; d ]
        // Second turns onward are decisions
        instant (Busted("C", ValueCard Card.Two)) [ b; d; a; c ]
        instant (Stood "B") [ d; a; b; c ]
        instant (Drew("D", ValueCard Card.Three)) [ a; d'; b; c ]
        instant (Froze("A", "D")) [ a; frozen; b; c ]
        instant (Drew("A", ValueCard Card.Six)) [ player "A" [ ValueCard Card.Six; ValueCard Card.Five ]; frozen; b; c ]
    ]

    let observations = observe timeline

    let othersOf (observation: Observation) =
        observation.OtherPlayers |> List.map (fun player -> player.Name)

    Assert.Equal(5, List.length observations)

    // Turns count per player within the round: everyone's second turn, then
    // A's third
    Assert.Equal<uint list>([ 2u; 2u; 2u; 2u; 3u ], observations |> List.map (fun observation -> observation.Turn))
    Assert.All(observations, (fun observation -> Assert.Equal(1u, observation.Round)))

    // C's bust was still C choosing to hit, with everyone else active
    Assert.Equal("C", observations[0].Name)
    Assert.Equal(Strategy.Hit, observations[0].Choice)
    Assert.Equal<string list>([ "A"; "B"; "D" ], othersOf observations[0])

    // B stood after C busted, so C is excluded from B's others
    Assert.Equal("B", observations[1].Name)
    Assert.Equal<string list>([ "D"; "A" ], othersOf observations[1])

    // D hit after B stood, so only A remains in D's others
    Assert.Equal("D", observations[2].Name)
    Assert.Equal<string list>([ "A" ], othersOf observations[2])

    // A hit and drew the freeze; D was still active when A decided
    Assert.Equal("A", observations[3].Name)
    Assert.Equal<string list>([ "D" ], othersOf observations[3])

    // After freezing D, A is the last active player, and everyone else has
    // finished one way or another
    Assert.Equal("A", observations[4].Name)
    Assert.Equal<string list>([], othersOf observations[4])

    Assert.Equal<string list>(
        [ "D"; "B"; "C" ],
        observations[4].FinishedPlayers |> List.map (fun player -> player.Name)
    )

[<Fact>]
let ``Set-aside cards resolving from a deal3 are not voluntary decisions`` () =
    let a = player "A" [ ValueCard Card.Three ]
    let b = player "B" [ ValueCard Card.Five ]
    let c = player "C" [ ValueCard Card.Seven ]

    let b' =
        player "B" [ ActionCard Card.Freeze; ValueCard Card.Two; ValueCard Card.Five ]

    let frozen = player "C" [ ValueCard Card.Seven ]

    let timeline = [
        // The opening deal: every player's first turn is a forced hit
        instant (Drew("A", ValueCard Card.Three)) [ player "B" []; player "C" []; a ]
        instant (Drew("B", ValueCard Card.Five)) [ player "C" []; a; b ]
        instant (Drew("C", ValueCard Card.Seven)) [ a; b; c ]
        // A hits, drawing a deal3 aimed at B; the flips set a freeze aside
        instant (Dealt3("A", "B", [ ValueCard Card.Two; ActionCard Card.Freeze ])) [ a; b'; c ]
        // ...which B then gives out: B never chose to hit here
        instant (Froze("B", "C")) [ a; player "B" [ ValueCard Card.Two; ValueCard Card.Five ]; frozen ]
        instant (Drew("A", ValueCard Card.Six)) [
            player "A" [ ValueCard Card.Six; ValueCard Card.Three ]
            player "B" [ ValueCard Card.Two; ValueCard Card.Five ]
            frozen
        ]
    ]

    let observations = observe timeline

    // Only A's deal3 draw and A's later hit are decisions
    Assert.Equal(2, List.length observations)
    Assert.All(observations, (fun observation -> Assert.Equal("A", observation.Name)))
    Assert.Equal<uint list>([ 2u; 3u ], observations |> List.map (fun observation -> observation.Turn))

    // The frozen player is finished for A's later decision, B is still active
    Assert.Equal<string list>([ "B" ], observations[1].OtherPlayers |> List.map (fun player -> player.Name))
    Assert.Equal<string list>([ "C" ], observations[1].FinishedPlayers |> List.map (fun player -> player.Name))
