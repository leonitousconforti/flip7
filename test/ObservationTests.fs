module ObservationTests

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
let ``Players who stood busted or were frozen are excluded from other players`` () =
    let a = player "A" [ ValueCard Card.Five ]
    let b = player "B" [ ValueCard Card.Seven ]
    let c = player "C" [ ValueCard Card.Two; ValueCard Card.Two ]
    let d = player "D" [ ValueCard Card.Nine ]
    let d' = player "D" [ ValueCard Card.Three; ValueCard Card.Nine ]

    let frozen =
        player "D" [ ActionCard Card.Freeze; ValueCard Card.Three; ValueCard Card.Nine ]

    let timeline = [
        instant (Busted("C", ValueCard Card.Two)) [ b; d; a; c ]
        instant (Stood "B") [ d; a; b; c ]
        instant (Drew("D", ValueCard Card.Three)) [ a; d'; b; c ]
        instant (Froze("A", "D")) [ a; frozen; b; c ]
        instant (Drew("A", ValueCard Card.Six)) [ player "A" [ ValueCard Card.Six; ValueCard Card.Five ]; frozen; b; c ]
    ]

    let observations = timeline |> Seq.ofList |> Observation.FromTimeline

    let othersOf (observation: Observation) =
        observation.OtherPlayers |> List.map (fun player -> player.Name)

    Assert.Equal(4, List.length observations)

    // Turns count per player within the round: B's first, D's first, then A's
    // first and second
    Assert.Equal<uint list>([ 1u; 1u; 1u; 2u ], observations |> List.map (fun observation -> observation.Turn))
    Assert.All(observations, (fun observation -> Assert.Equal(1u, observation.Round)))

    // B stood while C had already busted, so C is excluded from B's others
    Assert.Equal("B", observations[0].Name)
    Assert.Equal<string list>([ "D"; "A" ], othersOf observations[0])

    // D hit after B stood, so only A remains in D's others
    Assert.Equal("D", observations[1].Name)
    Assert.Equal<string list>([ "A" ], othersOf observations[1])

    // A hit and drew the freeze; D was still active when A decided
    Assert.Equal("A", observations[2].Name)
    Assert.Equal<string list>([ "D" ], othersOf observations[2])

    // After freezing D, A is the last active player, and everyone else has
    // finished one way or another
    Assert.Equal("A", observations[3].Name)
    Assert.Equal<string list>([], othersOf observations[3])

    Assert.Equal<string list>(
        [ "D"; "B"; "C" ],
        observations[3].FinishedPlayers |> List.map (fun player -> player.Name)
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
        instant (Drew("B", ValueCard Card.Five)) [ a; b; c ]
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

    let observations = timeline |> Seq.ofList |> Observation.FromTimeline

    // Only A's deal3 draw and A's later hit are decisions
    Assert.Equal(2, List.length observations)
    Assert.All(observations, (fun observation -> Assert.Equal("A", observation.Name)))
    Assert.Equal<uint list>([ 1u; 2u ], observations |> List.map (fun observation -> observation.Turn))

    // The frozen player is finished for A's later decision, B is still active
    Assert.Equal<string list>([ "B" ], observations[1].OtherPlayers |> List.map (fun player -> player.Name))
    Assert.Equal<string list>([ "C" ], observations[1].FinishedPlayers |> List.map (fun player -> player.Name))
