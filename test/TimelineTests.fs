module TimelineTests

open Xunit
open Flip7

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
            [
                "Alice", Strategy.Random
                "Bob", HitUntilScore 25u
                "Carol", HitUntilNumCards 4u
            ]
            None
            None
            None
            None
        |> Seq.toList

    // Every card is accounted for at every instant
    for instant in timeline do
        let hands = instant.Players |> List.map (fun player -> player.Hand)
        Assert.Empty(Simulation.Issues instant.Deck instant.Discards hands)

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
    |> List.iter (fun (before, after) -> before |> Map.iter (fun name score -> Assert.True(score <= after[name])))
