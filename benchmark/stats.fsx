#r "nuget: FSharp.Control.AsyncSeq, 4.15.0"

#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
#load "../src/Lookahead.fs"
#load "../src/Player.fs"
#load "../src/Strategy.fs"
#load "../src/Timeline.fs"
#load "../src/Persistence.fs"
#load "../src/Observation.fs"
#load "../src/Inference.fs"

open Flip7
open FSharp.Control

let threads =
    fsi.CommandLineArgs
    |> Array.tryItem 1
    |> Option.map int
    |> Option.defaultValue 4

let depth =
    fsi.CommandLineArgs
    |> Array.tryItem 2
    |> Option.map int
    |> Option.defaultValue 5

let seed =
    fsi.CommandLineArgs
    |> Array.tryItem 3
    |> Option.map int
    |> Option.defaultValue 0

// Every legal hand of 0..8 cards: a dup and the second chance covering it
// cancel the moment they meet, so a lasting hand holds each value at most once,
// each modifier at most once, and up to the deck's three of each action
let hands = seq {
    let limits =
        Deck.Full
        |> Map.toList
        |> List.map (fun (card, count) -> card, if card.IsValueCard then 1 else int count)

    let rec choose (size: int) (from: (Card * int) list) : Hand seq = seq {
        match size, from with
        | 0, _ -> yield []
        | _, [] -> ()
        | size, (card, limit) :: rest ->
            for copies in min size limit .. -1 .. 0 do
                for tail in choose (size - copies) rest do
                    yield List.replicate copies card @ tail
    }

    for size in 0..8 do
        yield! choose size limits
}

let all = hands |> Seq.toArray

let canceller = new System.Threading.CancellationTokenSource()
System.Console.CancelKeyPress.Add(fun press ->
    press.Cancel <- true
    canceller.Cancel()
)

let searcher = new Lookahead(depth, all, cancellation = canceller.Token)
let LookAheadDecider (random: System.Random) : Strategy.HitOrStandDecider =
    let x = fun () -> random.NextDouble()
    let onUnpricedHand = fun () -> if x () < 0.5 then Strategy.Hit else Strategy.Stand
    fun _strategy _round _turn player _otherPlayers _finishedPlayers _decks -> async {
        match! searcher.GainFromHitting player.Hand with
        | Error(UnpricedHand _hand) -> return onUnpricedHand ()
        | Error lookAheadError -> return raise (System.InvalidOperationException $"Lookahead failed: {lookAheadError}")
        | Ok netScore -> return if netScore > 0.0 then Strategy.Hit else Strategy.Stand
    }

let makeDecider (random: System.Random) : Strategy.Decider = {
    Target = Strategy.DecideTargetWith random
    HitOrStand =
        fun strategy ->
            match strategy with
            | Strategy.Custom "LookAhead" -> LookAheadDecider random strategy
            | _ -> Strategy.DecideHitOrStandWith random strategy
}

let players = [
    ("Alice", Strategy.Random, Targeting.ChoosesRandomly)
    ("Bob", Strategy.Random, Targeting.ChoosesRandomly)
    ("Charlie", Strategy.Random, Targeting.ChoosesRandomly)
    ("Dave", Strategy.Random, Targeting.ChoosesRandomly)
    ("Eve", Strategy.Random, Targeting.ChoosesRandomly)
    ("Frank", Strategy.Random, Targeting.ChoosesRandomly)
    ("Grace", Strategy.Random, Targeting.ChoosesRandomly)
    ("Heidi", Strategy.Random, Targeting.ChoosesRandomly)
    ("Ivan", Strategy.Random, Targeting.ChoosesRandomly)
    ("Judy", Strategy.Random, Targeting.ChoosesRandomly)
    ("Kevin", Strategy.Random, Targeting.ChoosesRandomly)
    ("Sage", Strategy.Custom "LookAhead", Targeting.PlaysSpitefully)
]

module TimelineStats =
    type public Stats = {
        /// How many rounds were played
        Rounds: float

        /// How many turns each player made by round
        Decisions: Map<uint, Map<string, float>>
        /// How many targeting decisions each player made by round
        Targeting: Map<uint, Map<string, float>>

        /// How many points each player scored by round
        Points: Map<uint, Map<string, float>>
        /// How many flip7s each player achieved by round
        Flip7s: Map<uint, Map<string, float>>
    }

    let public Empty = {
        Rounds = 0.0
        Decisions = Map.empty
        Targeting = Map.empty
        Points = Map.empty
        Flip7s = Map.empty
    }

    let private currentRound (stats: Stats) : uint = uint stats.Rounds + 1u
    let private tally (byRound: Map<uint, Map<string, float>>) (round: uint) (player: string) =
        let players = byRound |> Map.tryFind round |> Option.defaultValue Map.empty
        let count = players |> Map.tryFind player |> Option.defaultValue 0.0
        byRound |> Map.add round (players |> Map.add player (count + 1.0))

    let private updateRoundEnded (stats: Stats) (scoreboard: Map<string, uint>) : Stats =
        let round = currentRound stats

        {
            stats with
                Points =
                    stats.Points
                    |> Map.add round (scoreboard |> Map.map (fun _ score -> float score))
                Rounds = float round
        }

    let public Update (stats: Stats) (instant: Instant) : Stats =
        let round = currentRound stats

        match instant.Event with
        | Event.RoundEnded scoreboard -> updateRoundEnded stats scoreboard
        | Event.Flip7Achieved player -> { stats with Flip7s = tally stats.Flip7s round player }
        | Event.Drew(player, _card) -> {
            stats with
                Decisions = tally stats.Decisions round player
          }
        | Event.Stood player -> {
            stats with
                Decisions = tally stats.Decisions round player
          }
        | Event.Froze(player, _target) -> {
            stats with
                Targeting = tally stats.Targeting round player
          }
        | Event.SecondChancePassed(player, _target) -> {
            stats with
                Targeting = tally stats.Targeting round player
          }
        | Event.Dealt3(player, _target, _cards) -> {
            stats with
                Targeting = tally stats.Targeting round player
          }
        | _ -> stats

    // Stats paired with the games they cover merge as a games-weighted mean.
    // The mean runs over every key on either side with an absent key counting
    // as zero: a player who made no targeting decision in a game, or a round
    // that only a long game reached, still averages in as nothing for that game
    let public Aggregate ((games1, stats1): uint * Stats) ((games2, stats2): uint * Stats) : uint * Stats =
        if games1 + games2 = 0u then
            0u, Empty
        else

        let weight1, weight2 = float games1, float games2
        let mean (a: float) (b: float) : float =
            (a * weight1 + b * weight2) / (weight1 + weight2)

        let merge (zero: 'v) (combine: 'v -> 'v -> 'v) (a: Map<'k, 'v>) (b: Map<'k, 'v>) : Map<'k, 'v> =
            Seq.append (Map.keys a) (Map.keys b)
            |> Seq.distinct
            |> Seq.map (fun key ->
                let valueA = a |> Map.tryFind key |> Option.defaultValue zero
                let valueB = b |> Map.tryFind key |> Option.defaultValue zero
                key, combine valueA valueB
            )
            |> Map.ofSeq

        let meanByRound = merge Map.empty (merge 0.0 mean)

        games1 + games2,
        {
            Rounds = mean stats1.Rounds stats2.Rounds
            Decisions = meanByRound stats1.Decisions stats2.Decisions
            Targeting = meanByRound stats1.Targeting stats2.Targeting
            Points = meanByRound stats1.Points stats2.Points
            Flip7s = meanByRound stats1.Flip7s stats2.Flip7s
        }

    let public Summary ((games, stats): uint * Stats) : string =
        let perPlayer (byRound: Map<uint, Map<string, float>>) : string =
            byRound
            |> Map.toList
            |> List.collect (fun (_round, players) -> Map.toList players)
            |> List.groupBy fst
            |> List.map (fun (player, rounds) -> player, rounds |> List.sumBy snd)
            |> List.sortBy fst
            |> List.map (fun (player, total) -> $"{player}: %.2f{total}")
            |> String.concat ", "

        $"Games: {games}, rounds per game: %.2f{stats.Rounds}\n"
        + $"Decisions per player per game: {perPlayer stats.Decisions}\n"
        + $"Targeting decisions per player per game: {perPlayer stats.Targeting}\n"
        + $"Points per player per game: {perPlayer stats.Points}\n"
        + $"Flip7s per player per game: {perPlayer stats.Flip7s}"

let makeThread (index: int) : Async<uint * TimelineStats.Stats> =
    let random = System.Random(seed + index)
    let decider = makeDecider random

    let rec thread ((games, totals): uint * TimelineStats.Stats) = async {
        let! game =
            (random, decider, players)
            |||> Timeline.SimulateWithDecider
            |> AsyncSeq.fold TimelineStats.Update TimelineStats.Empty

        let aggregate = TimelineStats.Aggregate (games, totals) (1u, game)
        eprintfn $"\rThread {index + 1}: {TimelineStats.Summary aggregate}      "
        if canceller.IsCancellationRequested then
            return aggregate
        else
            return! thread aggregate
    }

    in
    thread (0u, TimelineStats.Empty)

// Run 4 parallel simulations until the user cancels, printing final results to
// stdout. Keep track of number of wins for each player, average number of
// hit or stand decisions a player gets to make, average number of targeting
// decisions a player gets to make, average number of rounds per game, average
// number of turns per round, average number of flip7s achieved per game.
searcher.GainFromHitting [] |> Async.RunSynchronously |> ignore
Seq.init threads makeThread |> Async.Parallel |> Async.RunSynchronously
