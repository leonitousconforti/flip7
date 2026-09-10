module TimelineTests

open FSharp.Control
open Xunit
open Flip7

[<Fact>]
let ``the same seed produces the exact same timeline`` () =
    let simulate seed =
        Timeline.SimulateWith (System.Random(seed: int)) [
            "Alice", Strategy.Random, ChoosesRandomly
            "Bob", HitUntilScore 25u, ChoosesRandomly
            "Carol", AlwaysHits, ChoosesRandomly
            "Dave", HitUntilNumCards 4u, ChoosesRandomly
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
            "Alice", Strategy.Random, ChoosesRandomly
            "Bob", HitUntilScore 25u, ChoosesRandomly
            "Carol", HitUntilNumCards 4u, ChoosesRandomly
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

// A characterization test, not a specification: it pins the exact event stream
// a seed produces so a refactor meant to preserve behavior can prove it did.
// `the same seed produces the exact same timeline` cannot do that, because it
// compares two runs of the same build. Regenerate these deliberately whenever
// engine behavior changes on purpose - a draw consumes randomness proportional
// to the cards left in the deck, so one extra roll anywhere reshuffles the
// whole rest of the game and every digest below moves.
[<Theory>]
[<InlineData(42, 117, "D004A47D9B319D3E")>]
[<InlineData(7, 146, "DA6EF3B239A1AAE1")>]
[<InlineData(1234, 145, "32481F4D5223AA74")>]
let ``a seeded game produces exactly the events it always has`` (seed: int) (count: int) (digest: string) =
    let events =
        Timeline.SimulateWith (System.Random seed) [
            "Alice", Strategy.Random, ChoosesRandomly
            "Bob", HitUntilScore 25u, ChoosesRandomly
            "Carol", AlwaysHits, ChoosesRandomly
            "Dave", HitUntilNumCards 4u, ChoosesRandomly
        ]
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously
        |> List.map (fun instant -> string instant.Event)

    Assert.Equal(count, List.length events)

    let actual =
        events
        |> String.concat "\n"
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Security.Cryptography.SHA256.HashData
        |> System.Convert.ToHexString

    Assert.Equal(digest, actual.Substring(0, 16))

// Seeds 657 and 1657 each reach the case the rules single out: a deal3 flips
// two set-aside cards, the first is given back to the flipper and busts them,
// and the second must then be discarded unresolved rather than handed out by a
// player whose round is over
[<Theory>]
[<InlineData 657>]
[<InlineData 1657>]
let ``a busted flipper never gives out a set-aside card`` (seed: int) =
    let timeline =
        Timeline.SimulateWith (System.Random seed) [
            "A", AlwaysHits, ChoosesRandomly
            "B", AlwaysHits, ChoosesRandomly
        ]
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    for before, instant in List.pairwise timeline do
        let giver =
            match instant.Event with
            | Froze(source, _) -> Some source
            | Dealt3(source, _, _) -> Some source
            | _ -> None

        match giver with
        | None -> ()
        | Some name ->
            // Busting on the event being examined is legal: you may deal
            // yourself a deal3 and bust on the flips
            Assert.DoesNotContain(before.Players, (fun player -> player.Name = name && Hand.IsBust player.Hand))

// Names are how the engine tells seats apart, so a duplicate has to be caught
// rather than quietly scoring two players as one
[<Fact>]
let ``seating the same name twice is refused`` () =
    // Eagerly, before anything is enumerated: the error belongs where the
    // lineup was written, not wherever the timeline was first pulled
    Assert.Throws<System.ArgumentException>(fun () ->
        Timeline.SimulateWith (System.Random 1) [
            "Alice", AlwaysHits, ChoosesRandomly
            "Alice", AlwaysStands, ChoosesRandomly
        ]
        |> ignore
    )
    |> ignore

[<Fact>]
let ``continuing from a state with a repeated name is refused`` () =
    let active = [ Player.Make("A", AlwaysHits) ]
    let finished = [ Player.Make("A", AlwaysStands) ]

    Assert.Throws<System.ArgumentException>(fun () ->
        Timeline.ContinueWith
            (System.Random 1)
            (Strategy.DecideWith(System.Random 1))
            1u
            Map.empty
            active
            finished
            (Deck.Full, Deck.Empty)
        |> ignore
    )
    |> ignore

[<Fact>]
let ``SimulateWithDecider routes prompt players through the injected decider`` () =
    let mutable decisions = 0

    let decide: Strategy.Decider = {
        HitOrStand =
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
                | strategy ->
                    Strategy.DecideHitOrStandWith (System.Random 1) strategy round turn player others finished decks
        Target = Strategy.DecideTargetWith(System.Random 1)
    }

    let timeline =
        Timeline.SimulateWithDecider (System.Random 5) decide [
            "You", Custom "TerminalPrompt", ChoosesRandomly
            "Bot", HitUntilScore 25u, ChoosesRandomly
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
    // Standing is only the hit-or-stand half: play continues past the seeded
    // round, and a forced first hit there can still flip an action card that
    // has to be aimed at someone
    let decide: Strategy.Decider = {
        HitOrStand = fun _ _ _ _ _ _ _ -> async.Return Strategy.Stand
        Target = Strategy.DecideTargetWith(System.Random 3)
    }

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
