module TimelineTests

open FSharp.Control
open Xunit
open Flip7

[<Fact>]
let ``the same seed produces the exact same timeline`` () =
    let simulate seed =
        Timeline.SimulateWith (System.Random(seed: int)) [
            "Alice", Strategy.Random
            "Bob", HitUntilScore 25u
            "Carol", AlwaysHits
            "Dave", HitUntilNumCards 4u
        ]
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

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
        Timeline.SimulateWith (System.Random seed) [
            "Alice", Strategy.Random
            "Bob", HitUntilScore 25u
            "Carol", HitUntilNumCards 4u
        ]
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    // Every card is accounted for at every instant
    for instant in timeline do
        let hands = instant.Players |> List.map (fun player -> player.Hand)
        Assert.Empty(Simulation.Issues instant.Deck instant.Discards hands)

    // The game ends once someone reaches 200 points, at the final RoundEnded
    let finalInstant = List.last timeline
    Assert.True(finalInstant.Players |> List.exists (fun player -> player.FirmScore >= 200u))
    Assert.True(finalInstant.Event.IsRoundEnded)

    // Firm scores only ever grow
    timeline
    |> List.map (fun instant ->
        instant.Players
        |> List.map (fun player -> player.Name, player.FirmScore)
        |> Map.ofList
    )
    |> List.pairwise
    |> List.iter (fun (before, after) -> before |> Map.iter (fun name score -> Assert.True(score <= after[name])))

[<Fact>]
let ``SimulateWithDecider routes prompt players through the injected decider`` () =
    let mutable decisions = 0

    let decide: Strategy.Decider =
        fun strategy round turn player others finished decks ->
            match strategy with
            | Custom name ->
                decisions <- decisions + 1

                async.Return(
                    if Hand.Score player.Hand < 18u then
                        Strategy.Hit
                    else
                        Strategy.Stand
                )
            | strategy -> Strategy.DecideWith (System.Random 1) strategy round turn player others finished decks

    let timeline =
        Timeline.SimulateWithDecider (System.Random 5) decide [
            "You", Custom "TerminalPrompt"
            "Bot", HitUntilScore 25u
        ]
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    // The game runs to completion with the decider standing in for the human
    Assert.True(decisions > 0)
    Assert.True((List.last timeline).Event.IsRoundEnded)
    Assert.True(
        (List.last timeline).Players
        |> List.exists (fun player -> player.FirmScore >= 200u)
    )

[<Fact>]
let ``ContinueWith resumes a round mid-flight and banks finished hands`` () =
    let active = [
        Player.Make("A", Custom "TerminalPrompt", 10u, [ ValueCard Card.Five ])
        Player.Make("B", HitUntilScore 25u, 20u, [ ValueCard Card.Seven ])
    ]

    let finished = [
        Player.Make("C", AlwaysStands, 30u, [ ValueCard Card.Nine; ValueCard Card.Two ])
    ]

    let deck =
        active @ finished
        |> List.collect (fun player -> player.Hand)
        |> List.fold Deck.Decrement Deck.Full

    // Everyone stands as soon as they are asked, so the seeded round closes
    // immediately; both actives already took their forced first hit
    let decide: Strategy.Decider = fun _ _ _ _ _ _ _ -> async.Return Strategy.Stand

    let turnsTaken = Map.ofList [ "A", 1u; "B", 1u ]

    let timeline =
        Timeline.ContinueWith (System.Random 3) decide 4u turnsTaken active finished (deck, Deck.Empty)
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    // The seeded round plays out from exactly where it stood
    Assert.Equal(Stood "A", timeline[0].Event)
    Assert.Equal(Stood "B", timeline[1].Event)
    Assert.Equal(RoundEnded(Map.ofList [ "A", 5u; "B", 7u; "C", 11u ]), timeline[2].Event)

    // Every card is accounted for at every instant across the splice
    for instant in timeline do
        let hands = instant.Players |> List.map (fun player -> player.Hand)
        Assert.Empty(Simulation.Issues instant.Deck instant.Discards hands)

    // The game then continues to completion as usual
    Assert.True((List.last timeline).Event.IsRoundEnded)
    Assert.True(
        (List.last timeline).Players
        |> List.exists (fun player -> player.FirmScore >= 200u)
    )
