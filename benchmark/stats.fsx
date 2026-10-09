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

#load "../ui/shared/Stats.fs"

open Flip7
open FSharp.Control
open System.Threading.Tasks

let threads =
    fsi.CommandLineArgs
    |> Array.tryItem 1
    |> Option.map int
    |> Option.defaultValue 4

let limit =
    fsi.CommandLineArgs
    |> Array.tryItem 2
    |> Option.map int
    |> Option.defaultValue -1

let depth =
    fsi.CommandLineArgs
    |> Array.tryItem 3
    |> Option.map int
    |> Option.defaultValue 5

let seed =
    fsi.CommandLineArgs
    |> Array.tryItem 4
    |> Option.map int
    |> Option.defaultValue 0

// Every legal hand of 0..8 cards: a dup and the second chance covering it
// cancel the moment they meet, so a lasting hand holds each value at most once,
// each modifier at most once, and up to the deck's three of each action
let roots = seq {
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

let canceller = new System.Threading.CancellationTokenSource()
System.Console.CancelKeyPress.Add(fun press ->
    press.Cancel <- true
    canceller.Cancel()
)

// Progress on stderr, so redirected stdout stays clean
let LookaheadBuilderProgress = fun line -> eprintfn $"  {line}"
let searcher =
    new Lookahead(depth, roots, progress = LookaheadBuilderProgress, cancellation = canceller.Token)

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

let makeThread (cancellation: System.Threading.CancellationToken) (index: int) : Async<uint * TimelineStats.Stats> =
    let cancelled: Task = Task.Delay(System.Threading.Timeout.Infinite, cancellation)
    let random = System.Random(seed + index)
    let decider = makeDecider random

    let quota =
        if limit < 0 then
            System.UInt32.MaxValue
        else
            let remainder = if index < limit % threads then 1 else 0
            uint (limit / threads + remainder)

    let rec thread ((games, totals): uint * TimelineStats.Stats) = async {
        if games >= quota then
            return games, totals
        else

        let game =
            (random, decider, players)
            |||> Timeline.SimulateWithDecider
            |> AsyncSeq.fold TimelineStats.Update TimelineStats.Empty
            |> fun play -> Async.StartAsTask(play, cancellationToken = cancellation)

        do! Task.WhenAny(game, cancelled) |> Async.AwaitTask |> Async.Ignore
        if cancellation.IsCancellationRequested then
            return games, totals
        else

        let! played = Async.AwaitTask game
        let aggregate = TimelineStats.Aggregate (games, totals) (1u, played)
        eprintfn $"\rThread {index + 1}: {TimelineStats.Summary aggregate}      "
        return! thread aggregate
    }

    in
    thread (0u, TimelineStats.Empty)

let final =
    Seq.init threads (makeThread canceller.Token)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.fold TimelineStats.Aggregate (0u, TimelineStats.Empty)

System.Console.Clear()
printfn $"{TimelineStats.Summary final}"
