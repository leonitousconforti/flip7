module TimelineTests

open FSharp.Control
open Xunit
open Flip7
open Support

// Continues a game on the default decider from the given round, with the
// players still in it, the next to act at the head, and those finished, all
// seated as named. Everyone holding cards has taken their forced first turn,
// and the deck holds every card that is not in a hand. No test reads the cards
// a continuation deals, so any seed does. The timeline is not played, so a
// refusal shows that it comes before anything is enumerated.
let private continueAt (round: uint) (seating: string list) (active: Player list) (finished: Player list) : Timeline =
    let deck =
        active @ finished
        |> List.collect (fun player -> player.Hand)
        |> List.fold Deck.Decrement Deck.Full

    let turnsTaken =
        active
        |> List.where (fun player -> not (List.isEmpty player.Hand))
        |> List.map (fun player -> player.Name, 1u)
        |> Map.ofList

    Timeline.ContinueWith
        (System.Random 3)
        Strategy.Decider.Default
        Seq.empty
        seating
        round
        turnsTaken
        active
        finished
        (deck, Deck.Empty)

// ---- whole games ----

// A characterization test, not a specification: it pins the exact event stream
// a seed produces, so a refactor meant to preserve behavior can prove it did.
// Comparing two runs of the same build, as the properties do, cannot. The
// events are hashed in their serialized form, so rewording a caption moves
// nothing. Regenerate these deliberately whenever engine behavior changes on
// purpose. A draw consumes randomness proportional to the cards left in the
// deck, so one extra roll anywhere reshuffles the whole rest of the game and
// every digest below moves. A failure shows the new count and digest to paste.
[<Theory>]
[<InlineData(42, 162, "7694E7B105844432")>]
[<InlineData(7, 220, "04761262B6646DEE")>]
[<InlineData(1234, 182, "E2141B3CACE919E4")>]
let ``a seeded game produces exactly the events it always has`` (seed: int) (count: int) (digest: string) =
    let events =
        Timeline.SimulateWith (System.Random seed) fourSeats
        |> played
        |> List.map (fun instant -> instant.Event |> Event.Serialize |> String.concat "\n")

    let actual =
        events
        |> String.concat "\n"
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Security.Cryptography.SHA256.HashData
        |> System.Convert.ToHexString

    Assert.Equal((count, digest), (List.length events, actual.Substring(0, 16)))

let private isSetAside (card: Card) : bool =
    card = ActionCard Card.Freeze || card = ActionCard Card.Deal3

// The bust road: the target of a deal3 set aside a deal3 and then another card,
// dealt that deal3 to themselves, and busted on its flips with the other card
// still to give out
let private flipperBustsWithASetAsideLeft (timeline: Instant list) : bool =
    timeline
    |> List.pairwise
    |> List.exists (fun (outer, nested) ->
        match outer.Event, nested.Event with
        | Dealt3(_, flipper, cards), Dealt3(source, target, _) when source = flipper && target = flipper ->
            let setAsides = cards |> List.where isSetAside

            List.length setAsides >= 2
            && List.head setAsides = ActionCard Card.Deal3
            && nested.Players
               |> List.exists (fun player -> player.Name = flipper && Hand.IsBust player.Hand)
        | _ -> false
    )

// The flip7 road: the target of a deal3 set a card aside and then flipped their
// seventh different number
let private targetFlipsSevenWithASetAsideLeft (timeline: Instant list) : bool =
    timeline
    |> List.exists (fun instant ->
        match instant.Event with
        | Dealt3(_, target, cards) ->
            List.exists isSetAside cards
            && instant.Players
               |> List.exists (fun player -> player.Name = target && Hand.HasFlip7Bonus player.Hand)
        | _ -> false
    )

// A deal3 sets its freeze and deal3 cards aside until its flips are done, and
// then its target gives them out. A bust or a flip7 on the flips ends the
// target's round first, and the cards set aside are then discarded with the
// hand. Each seed must take its road, and two seats that always hit rarely do:
// about 6 games in 1000 take the bust road and about 15 the flip7 road.
[<Theory>]
[<InlineData(115, "bust")>]
[<InlineData(391, "bust")>]
[<InlineData(1, "flip7")>]
let ``once a deal3 ends its target's round, the cards it set aside are discarded, never given out``
    (seed: int)
    (road: string)
    =
    let timeline =
        Timeline.SimulateWith (System.Random seed) [
            "A", Strategy.AlwaysHits, Targeting.ChoosesRandomly
            "B", Strategy.AlwaysHits, Targeting.ChoosesRandomly
        ]
        |> played

    let reachesTheRoad =
        match road with
        | "bust" -> flipperBustsWithASetAsideLeft timeline
        | "flip7" -> targetFlipsSevenWithASetAsideLeft timeline
        | _ -> failwith $"there is no {road} road"

    Assert.True(reachesTheRoad, $"seed {seed} no longer takes the {road} road: pick a new seed")

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

        // A flip7 ends the round at once, so nothing is given out after it
        match before.Players |> List.tryFind (fun player -> Hand.HasFlip7Bonus player.Hand) with
        | Some player when not before.Event.IsFlip7Achieved -> Assert.Equal(Flip7Achieved player.Name, instant.Event)
        | _ -> ()

// ---- refusals ----

// Names are how the engine tells seats apart, so a duplicate has to be caught
// rather than quietly scoring two players as one
[<Fact>]
let ``seating the same name twice is refused`` () =
    // Eagerly, before anything is enumerated: the error belongs where the
    // lineup was written, not wherever the timeline was first pulled
    let exn =
        Assert.Throws<System.ArgumentException>(fun () ->
            Timeline.SimulateWith (System.Random 1) [
                "Alice", Strategy.AlwaysHits, Targeting.ChoosesRandomly
                "Alice", Strategy.AlwaysStands, Targeting.ChoosesRandomly
            ]
            |> ignore
        )

    Assert.Contains("Alice", exn.Message)

[<Fact>]
let ``continuing from a state with a repeated name is refused`` () =
    let active = [ Player.Make("A", Strategy.AlwaysHits) ]
    let finished = [ Player.Make("A", Strategy.AlwaysStands) ]

    let exn =
        Assert.Throws<System.ArgumentException>(fun () -> continueAt 1u [ "A"; "A" ] active finished |> ignore)

    Assert.Contains("more than once: A", exn.Message)

// A seating has to name exactly the players at the table: one it leaves out
// would never be dealt into a later round, and one it repeats would be dealt
// in twice. Each seating is written with commas, and the problem is what the
// refusal must say is wrong with it.
[<Theory>]
[<InlineData("A,C", "left out: B; not at the table: C")>]
[<InlineData("A", "left out: B")>]
[<InlineData("A,A,B", "seated more than once: A")>]
[<InlineData("A,B,C", "not at the table: C")>]
[<InlineData("", "left out: A, B")>]
let ``continuing with a seating that is not the table is refused`` (seating: string) (problem: string) =
    let active = [
        Player.Make("A", Strategy.AlwaysStands)
        Player.Make("B", Strategy.AlwaysStands)
    ]

    let names =
        seating.Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

    let exn =
        Assert.Throws<System.ArgumentException>(fun () -> continueAt 1u names active [] |> ignore)

    Assert.Contains("The seating must name every player once", exn.Message)
    Assert.Contains(problem, exn.Message)

// A targeting takes sides at most three deep, counting the outermost, so a
// player seated on a deeper one is refused where the lineup was written, in a
// whole game or a continuation, active or finished, rather than at the first
// aim that comes to them. Three deep plays a whole game
[<Fact>]
let ``seating a player who takes sides four deep is refused, and three deep plays`` () =
    let rec deep (depth: int) : Targeting =
        if depth = 0 then
            Targeting.PlaysSpitefully
        else
            Targeting.TakesSides(Some "Bob", Some "Carol", deep (depth - 1))

    let game (targeting: Targeting) =
        Timeline.SimulateWith (System.Random 1) [
            "Alice", Strategy.HitUntilScore 20u, targeting
            "Bob", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
            "Carol", Strategy.HitUntilScore 20u, Targeting.PlaysSpitefully
        ]

    let whole =
        Assert.Throws<System.ArgumentException>(fun () -> game (deep 4) |> ignore)

    Assert.Contains("at most 3 deep", whole.Message)
    Assert.Contains("take them deeper: Alice", whole.Message)

    let active = [ Player.Make("A", Strategy.AlwaysStands, targeting = deep 4) ]
    let finished = [ Player.Make("B", Strategy.AlwaysStands, targeting = deep 5) ]

    let continued =
        Assert.Throws<System.ArgumentException>(fun () -> continueAt 1u [ "A"; "B" ] active finished |> ignore)

    Assert.Contains("at most 3 deep", continued.Message)
    Assert.Contains("take them deeper: A, B", continued.Message)

    let last = game (deep 3) |> played |> List.last
    Assert.True(last.Event.IsRoundEnded, "the game did not end with a round")
    Assert.Contains(last.Players, (fun player -> player.FirmScore >= 200u))

// ---- continuations ----

[<Fact>]
let ``a continued round resumes from the turns already taken and banks the hands already finished`` () =
    // A and B have taken their forced first turns, so each is asked straight
    // away and stands. C stood earlier and D busted
    let active = [
        Player.Make("A", Strategy.AlwaysStands, 10u, [ ValueCard Card.Five ])
        Player.Make("B", Strategy.AlwaysStands, 20u, [ ValueCard Card.Seven ])
    ]

    let finished = [
        Player.Make("C", Strategy.AlwaysStands, 30u, [ ValueCard Card.Nine; ValueCard Card.Two ])
        Player.Make("D", Strategy.AlwaysStands, 40u, [ ValueCard Card.Seven; ValueCard Card.Seven ])
    ]

    let resumed =
        continueAt 4u [ "A"; "B"; "C"; "D" ] active finished
        |> AsyncSeq.take 3
        |> played

    Assert.Equal<Event list>(
        [
            Stood "A"
            Stood "B"
            RoundEnded(Map.ofList [ "A", 5u; "B", 7u; "C", 9u + 2u; "D", 0u ])
        ],
        resumed |> List.map (fun instant -> instant.Event)
    )

    // Read by name, because round 4 starts three seats along, at D, and lists
    // the players from there
    let banked =
        (List.last resumed).Players
        |> List.map (fun player -> player.Name, player.FirmScore)
        |> Map.ofList

    Assert.Equal<Map<string, uint>>(Map.ofList [ "A", 10u + 5u; "B", 20u + 7u; "C", 30u + 9u + 2u; "D", 40u ], banked)

[<Fact>]
let ``a continued round is scored and followed in seat order`` () =
    let active = [ Player.Make("A", Strategy.AlwaysStands, 0u, [ ValueCard Card.Five ]) ]

    let finished = [
        Player.Make("B", Strategy.AlwaysStands, 0u, [ ValueCard Card.Seven ])
        Player.Make("C", Strategy.AlwaysStands, 0u, [ ValueCard Card.Nine ])
    ]

    let rounds =
        continueAt 2u [ "A"; "B"; "C" ] active finished
        |> AsyncSeq.filter (fun instant -> instant.Event.IsRoundEnded)
        |> AsyncSeq.take 2
        |> played
        |> List.map (fun instant -> instant.Players |> List.map (fun player -> player.Name))

    // The second round started one seat along, at B, and the third starts at C
    Assert.Equal<string list list>([ [ "B"; "C"; "A" ]; [ "C"; "A"; "B" ] ], rounds)

[<Fact>]
let ``a round that brings a player to exactly 200 ends the game`` () =
    let active = [
        Player.Make("A", Strategy.AlwaysStands, 195u, [ ValueCard Card.Five ])
        Player.Make("B", Strategy.AlwaysStands, 0u, [ ValueCard Card.Seven ])
    ]

    // A banks 5, which lands on 200 exactly and ends the game as surely as
    // passing it would
    let timeline = continueAt 1u [ "A"; "B" ] active [] |> played

    Assert.Equal<Event list>(
        [ Stood "A"; Stood "B"; RoundEnded(Map.ofList [ "A", 5u; "B", 7u ]) ],
        timeline |> List.map (fun instant -> instant.Event)
    )

    let a = (List.last timeline).Players |> List.find (fun player -> player.Name = "A")

    Assert.Equal(195u + 5u, a.FirmScore)

// Where a game stood at a hit-or-stand question, which is everything a
// continuation needs to pick it up there
type private Fork = {
    // How many instants the game had yielded before the question
    Yielded: int
    History: Instant list
    // How many calls the game had made on its Random before the question's
    // own draw
    Calls: int
    Round: uint
    TurnsTaken: Map<string, uint>
    Active: Player list
    Finished: Player list
    Decks: Deck * Deck
}

// The instants through the second RoundEnded: the rest of the round under way
// and the whole of the next, which the seating deals
let private throughNextRound (instants: Instant list) : Instant list =
    let roundEnds =
        instants
        |> List.indexed
        |> List.where (fun (_, instant) -> instant.Event.IsRoundEnded)
        |> List.map fst

    match roundEnds with
    | _ :: second :: _ -> List.take (second + 1) instants
    | _ -> instants

// A continuation is a fork: given the game so far, the seating, the turns
// everyone has taken, and the Random where it stood, a game continued from any
// hit-or-stand question plays out exactly as the game it was forked from,
// through the end of the next round, which the seating deals. Every turn count
// a fork needs is one a decider can see: everyone still in the round has taken
// their forced first turn before anyone is asked. The Random is not: a decider
// is handed one of its own, seeded by a draw from the game's. By the time a
// question is asked the game has already drawn its seed, so the fork winds a
// fresh Random to one call short and makes that draw again as it asks the
// question afresh.
[<Theory>]
[<InlineData 1>]
[<InlineData 2>]
[<InlineData 3>]
let ``a game forked at any question with its seating plays out as it did through the next round`` (seed: int) =
    let lineup = [
        "Alice", Strategy.PlaysLikeAHuman 20u, Targeting.ChoosesRandomly
        "Bob", Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully
        "Carol", Strategy.Random, Targeting.ChoosesRandomly
        "Dave", Strategy.HitUntilScore 22u, Targeting.PlaysGreedily(0.8, 0.2)
    ]

    let seating = lineup |> List.map (fun (name, _, _) -> name)
    let recording = Recording seed
    let yielded = ResizeArray<Instant>()
    let forks = ResizeArray<Fork>()

    // The round of the latest question, and the turn each player was last
    // asked about in that round
    let lastAskedTurns = ref (0u, Map.empty<string, uint>)

    let decide =
        Strategy.Decider.Default
        |> beforeEachQuestion (fun _ history question ->
            match question with
            | HitOrStandQuestion(_, round, turn, player, others, finished, decks) ->
                let lastRound, lastTurns = lastAskedTurns.Value
                let lastTurns = if round = lastRound then lastTurns else Map.empty

                // The asker has taken one turn fewer than the one they are
                // asked about, anyone asked earlier in the round the turn they
                // were asked about, and anyone else just their forced first
                let taken =
                    player :: others
                    |> List.map (fun active ->
                        if active.Name = player.Name then
                            active.Name, turn - 1u
                        else
                            active.Name, lastTurns |> Map.tryFind active.Name |> Option.defaultValue 1u
                    )
                    |> Map.ofList

                forks.Add {
                    Yielded = yielded.Count
                    History = List.ofSeq history
                    Calls = recording.Calls - 1
                    Round = round
                    TurnsTaken = taken
                    Active = player :: others
                    Finished = finished
                    Decks = decks
                }

                lastAskedTurns.Value <- round, Map.add player.Name turn lastTurns
            | TargetQuestion _ -> ()
        )

    Timeline.SimulateWithDecider recording decide lineup
    |> AsyncSeq.iter yielded.Add
    |> Async.RunSynchronously

    Assert.NotEmpty(forks)
    let game = List.ofSeq yielded

    for fork in forks do
        let expected = game |> List.skip fork.Yielded |> throughNextRound
        let forked = System.Random seed

        for _ in 1 .. fork.Calls do
            forked.NextDouble() |> ignore

        let continued =
            Timeline.ContinueWith
                forked
                Strategy.Decider.Default
                fork.History
                seating
                fork.Round
                fork.TurnsTaken
                fork.Active
                fork.Finished
                fork.Decks
            |> AsyncSeq.take expected.Length
            |> played

        Assert.Equal<Instant list>(expected, continued)

// ---- deciders ----

// The engine plays only an aim at one of the candidates it offered. No decider
// in this repository answers otherwise, so this guards the deciders others
// write against the published library.
[<Fact>]
let ``an aim at a player who is not a candidate is refused`` () =
    let aimsAtAStranger = {
        Strategy.Decider.Default with
            Target = fun _ _ _ _ _ _ _ _ -> async.Return(Player.Make("Zed", Strategy.AlwaysHits))
    }

    let exn =
        Assert.Throws<System.InvalidOperationException>(fun () ->
            Timeline.SimulateWithDecider (System.Random 1) aimsAtAStranger [
                "A", Strategy.AlwaysHits, Targeting.ChoosesRandomly
                "B", Strategy.AlwaysHits, Targeting.ChoosesRandomly
            ]
            |> played
            |> ignore
        )

    Assert.Contains("at Zed, which is not among", exn.Message)
