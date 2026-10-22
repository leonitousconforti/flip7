// How soon a game's timeline gives away the seed that dealt it.
//
//   dotnet fsi --optimize+ benchmark/oracle.fsx [games]
//
// Plays seeded games of a few lineups as Timeline.SimulateWith plays them, on a
// Random fresh from its seed, and feeds each timeline to an Oracle an instant
// at a time, the way a decider handed the game so far would. For each lineup it
// reports how many instants and cards it took to pin the seed, the time and
// memory of the one step that lists the seeds still agreeing, what every other
// step costs, and whether every seed pinned is the true one. The oracle needs
// no strategy, so the lineups only change how fast cards come out.
//
// A game started further along a Random, as stats.fsx's are after a thread's
// first game, or as the terminal app's were while it shuffled the lineup on the
// Random it dealt from, leaves the oracle no seed: it takes the Random to be
// fresh at the TableSeated instant. Seeds agree with such a game only by
// chance, so for each lineup the script also plays as many games on Randoms
// that drew once before the game and reports how many ever showed a seed as
// pinned.

#r "nuget: FSharp.Control.AsyncSeq, 4.15.0"

// Keep in compile order with src/Flip7.fsproj
#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
#load "../src/Player.fs"
#load "../src/Instant.fs"
#load "../src/Strategy.fs"
#load "../src/Timeline.fs"
#load "../src/Observation.fs"
#load "../src/Oracle.fs"

open Flip7
open FSharp.Control

let games =
    fsi.CommandLineArgs
    |> Array.tryItem 1
    |> Option.map int
    |> Option.defaultValue 30

let lineups = [
    "three players",
    [
        "Alice", Strategy.PlaysLikeAHuman 20u, Targeting.PlaysSpitefully
        "Bob", Strategy.MaximizesExpectedValue, Targeting.PlaysGreedily(0.8, 0.2)
        "Chloe", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
    ]
    "two cautious players",
    [
        "Alice", Strategy.HitUntilScore 15u, Targeting.ChoosesRandomly
        "Bob", Strategy.HitUntilNumCards 3u, Targeting.PlaysSpitefully
    ]
    "twelve players",
    [
        for seat in 1..12 ->
            $"P{seat}",
            (if seat % 2 = 0 then
                 Strategy.AlwaysHits
             else
                 Strategy.PlaysLikeAHuman(uint (10 + seat))),
            Targeting.ChoosesRandomly
    ]
]

type Game = {
    Instants: int
    Cards: int
    Reshuffled: bool
    Listing: float * float
    Steps: float array
    True: bool
}

// Cards that have left the deck by an instant, which is every card dealt so far
// until the deck is first reshuffled
let cardsSeen (instant: Instant) : int = 94 - int (Deck.Count instant.Deck)

// Plays a game and observes it instant by instant, timing every step and
// counting what the step that lists the seeds allocates. None when no seed is
// ever pinned
let solve (seed: int) (lineup: (string * Strategy * Targeting) list) : Game option =
    let timeline =
        Timeline.SimulateWith (System.Random seed) lineup
        |> AsyncSeq.toArrayAsync
        |> Async.RunSynchronously

    let steps = ResizeArray<float>()
    let mutable oracle = Oracle.Start
    let mutable listing = None
    let mutable pinned = None

    for index, instant in Array.indexed timeline do
        let unlisted = (Oracle.SeedsOf oracle).IsUnlisted
        let allocated = System.GC.GetTotalAllocatedBytes true
        let watch = System.Diagnostics.Stopwatch.StartNew()
        oracle <- Oracle.Observe oracle instant
        let seconds = watch.Elapsed.TotalSeconds
        let seeds = Oracle.SeedsOf oracle

        if unlisted && not seeds.IsUnlisted then
            listing <- Some(seconds, float (System.GC.GetTotalAllocatedBytes true - allocated) / 1e6)
        else
            steps.Add seconds

        match seeds, pinned with
        | Seeds.Pinned found, None -> pinned <- Some(index, found)
        | _ -> ()

    match pinned, listing with
    | Some(index, found), Some listing ->
        Some {
            Instants = index + 1
            Cards = cardsSeen timeline[index]
            Reshuffled =
                timeline[..index]
                |> Array.pairwise
                |> Array.exists (fun (before, after) -> Deck.Count after.Deck > Deck.Count before.Deck)
            Listing = listing
            Steps = steps.ToArray()
            True = found = abs seed
        }
    | _ -> None

// Whether a game on a Random that drew once before the game ever shows a seed
// as pinned, which only a seed agreeing by chance can be
let pinnedByChance (seed: int) (lineup: (string * Strategy * Targeting) list) : bool =
    let random = System.Random seed
    random.Next() |> ignore

    Timeline.SimulateWith random lineup
    |> AsyncSeq.toArrayAsync
    |> Async.RunSynchronously
    |> Array.scan Oracle.Observe Oracle.Start
    |> Array.exists (fun oracle -> (Oracle.SeedsOf oracle).IsPinned)

let percentile (fraction: float) (values: float array) =
    let sorted = Array.sort values
    sorted[min (sorted.Length - 1) (int (fraction * float sorted.Length))]

let summary (scale: float) (values: float array) =
    let values = values |> Array.map ((*) scale)
    $"%8.2f{percentile 0.5 values} %8.2f{percentile 0.9 values} %8.2f{Array.max values}"

// The first game pays for compiling the oracle, so it is played and dropped
solve 1 (snd lineups.Head) |> ignore

let picker = System.Random 2026

printfn $"{games} games per lineup on {System.Environment.ProcessorCount} cores"
printfn "                                 median      p90      max"

for name, lineup in lineups do
    let solved = [| for _ in 1..games -> solve (picker.Next()) lineup |]
    let found = solved |> Array.choose id
    let steps = found |> Array.collect (fun game -> game.Steps)

    printfn $"{name}:"
    printfn $"  instants to pin the seed  {summary 1.0 (found |> Array.map (fun game -> float game.Instants))}"
    printfn $"  cards seen by then        {summary 1.0 (found |> Array.map (fun game -> float game.Cards))}"
    printfn $"  listing step, ms          {summary 1e3 (found |> Array.map (fun game -> fst game.Listing))}"
    printfn $"  listing step, MB          {summary 1.0 (found |> Array.map (fun game -> snd game.Listing))}"
    printfn $"  every other step, us      {summary 1e6 steps}"
    printfn $"  all other steps, ms/game  {summary 1e3 (found |> Array.map (fun game -> Array.sum game.Steps))}"

    printfn
        $"  pinned {found.Length}/{games}, the true seed {found |> Array.where (fun game -> game.True) |> Array.length}/{found.Length}, after a reshuffle {found |> Array.where (fun game -> game.Reshuffled) |> Array.length}"

    let byChance =
        [| for _ in 1..games -> pinnedByChance (picker.Next()) lineup |]
        |> Array.where id
        |> Array.length

    printfn $"  on a Random used before the game, pinned {byChance}/{games}"
