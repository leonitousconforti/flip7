module TimelineTests

open Xunit
open Flip7

/// A deck containing only copies of a single card has only one possible draw,
/// which makes the resulting timeline deterministic by construction.
let private deckOf (card: Card) (count: uint) : Deck = Deck.Empty |> Map.add card count

let private events (timeline: Instant seq) : Event list =
    timeline |> Seq.map (fun instant -> instant.Event) |> Seq.toList

[<Fact>]
let ``two standing players play out one exact round`` () =
    let timeline =
        Timeline.Simulate
            [ "Alice", AlwaysStands; "Bob", AlwaysStands ]
            None
            (Some(Map.ofList [ "Alice", 195u ]))
            (Some(deckOf (ValueCard Card.Five) 2u))
            None
        |> Seq.toList

    // Both players are dealt their forced first card, then both stand
    Assert.Equal<Event list>(
        [
            Drew("Alice", ValueCard Card.Five)
            Drew("Bob", ValueCard Card.Five)
            Stood "Alice"
            Stood "Bob"
            RoundEnded(Map.ofList [ "Alice", 5u; "Bob", 5u ])
        ],
        events timeline
    )

    // Alice reaches 200 so the game ends after this single round
    Assert.Equal<Map<string, uint>>(Map.ofList [ "Alice", 200u; "Bob", 5u ], Timeline.Scoreboard timeline)
    Assert.Equal(2u, (List.last timeline).Discards[ValueCard Card.Five])

[<Fact>]
let ``drawing a duplicate value card busts`` () =
    // The game never ends (the player busts forever, scoring 0 every round),
    // which is fine because the timeline is lazy
    let timeline =
        Timeline.Simulate [ "Alice", AlwaysHits ] None None (Some(deckOf (ValueCard Card.Five) 2u)) None
        |> Seq.truncate 3
        |> Seq.toList

    Assert.Equal<Event list>(
        [
            Drew("Alice", ValueCard Card.Five)
            Busted("Alice", ValueCard Card.Five)
            RoundEnded(Map.ofList [ "Alice", 0u ])
        ],
        events timeline
    )

    // The busted hand is discarded at the end of the round
    Assert.Equal(2u, (List.last timeline).Discards[ValueCard Card.Five])

[<Fact>]
let ``a second chance cancels a duplicate and both cards are discarded`` () =
    let timeline =
        Timeline.Simulate
            [ "Alice", AlwaysHits ]
            (Some(Map.ofList [ "Alice", [ ActionCard Card.SecondChance; ValueCard Card.Five ] ]))
            None
            (Some(deckOf (ValueCard Card.Five) 2u))
            None
        |> Seq.truncate 3
        |> Seq.toList

    Assert.Equal<Event list>(
        [
            Drew("Alice", ValueCard Card.Five)
            Busted("Alice", ValueCard Card.Five)
            RoundEnded(Map.ofList [ "Alice", 0u ])
        ],
        events timeline
    )

    // After the first draw the duplicate and the second chance that canceled
    // it are both in the discard pile, and the hand is reduced to one five
    Assert.Equal(1u, timeline[0].Discards[ActionCard Card.SecondChance])
    Assert.Equal(1u, timeline[0].Discards[ValueCard Card.Five])
    Assert.Equal<Hand>([ ValueCard Card.Five ], timeline[0].Players.Head.Hand)

[<Fact>]
let ``flipping a seventh unique value card ends the round with the bonus`` () =
    let seedHand = [
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
    ]

    let timeline =
        Timeline.Simulate
            [ "Alice", AlwaysHits ]
            (Some(Map.ofList [ "Alice", seedHand ]))
            (Some(Map.ofList [ "Alice", 195u ]))
            (Some(deckOf (ValueCard Card.Seven) 1u))
            None
        |> Seq.toList

    // 1+2+3+4+5+6+7 = 28 points plus the 15 point flip7 bonus
    Assert.Equal<Event list>(
        [
            Drew("Alice", ValueCard Card.Seven)
            Flip7Achieved "Alice"
            RoundEnded(Map.ofList [ "Alice", 43u ])
        ],
        events timeline
    )

    Assert.Equal<Map<string, uint>>(Map.ofList [ "Alice", 238u ], Timeline.Scoreboard timeline)

[<Fact>]
let ``a lone player drawing freeze freezes themselves`` () =
    let timeline =
        Timeline.Simulate [ "Alice", AlwaysHits ] None None (Some(deckOf (ActionCard Card.Freeze) 1u)) None
        |> Seq.truncate 2
        |> Seq.toList

    Assert.Equal<Event list>([ Froze("Alice", "Alice"); RoundEnded(Map.ofList [ "Alice", 0u ]) ], events timeline)

    // The freeze card ends up in the discard pile with the rest of the hand
    Assert.Equal(1u, (List.last timeline).Discards[ActionCard Card.Freeze])

[<Fact>]
let ``the same seed produces the exact same timeline`` () =
    let simulate seed =
        Timeline.SimulateWith
            (System.Random(seed: int))
            [
                "Alice", Strategy.Random
                "Bob", HitUntilScore 25u
                "Carol", AlwaysHits
                "Dave", HitUntilNumCards 4u
            ]
            None
            None
            None
            None
        |> Seq.toList

    Assert.Equal<Instant list>(simulate 42, simulate 42)
    Assert.NotEqual<Instant list>(simulate 42, simulate 43)

[<Theory>]
[<InlineData 1>]
[<InlineData 2>]
[<InlineData 3>]
[<InlineData 4>]
[<InlineData 5>]
let ``simulated games uphold the invariants`` (seed: int) =
    let timeline =
        Timeline.SimulateWith
            (System.Random seed)
            [ "Alice", Strategy.Random; "Bob", HitUntilScore 25u; "Carol", HitUntilNumCards 4u ]
            None
            None
            None
            None
        |> Seq.toList

    // Every card is accounted for at every instant
    for instant in timeline do
        let hands = instant.Players |> List.map (fun player -> player.Hand)
        Assert.Empty(Simulation.IsValid instant.Deck instant.Discards hands)

    // The game ends at the end of a round, once someone reaches 200 points
    Assert.True((List.last timeline).Event.IsRoundEnded)
    Assert.True(Timeline.Scoreboard timeline |> Map.exists (fun _ score -> score >= 200u))

    // Firm scores only ever grow
    timeline
    |> List.map (fun instant ->
        instant.Players
        |> List.map (fun player -> player.Name, player.FirmScore)
        |> Map.ofList
    )
    |> List.pairwise
    |> List.iter (fun (before, after) ->
        before |> Map.iter (fun name score -> Assert.True(score <= after[name]))
    )
