// Sage's win rate at a table of people.
//
// Five seats: Sage and four PlaysLikeAHuman opponents, everyone aiming their
// action cards spitefully, which is what anyone does once they can see who is
// winning. Par is a fifth of the games, so that is what a seat wins by
// turning up; anything above it is the seat playing better than the table.
//
// The same seeds are played twice, once with Sage in the seat and once with
// MaximizesExpectedValue, and the two are compared game by game rather than
// rate against rate. Both arms meet the same deal on a given seed, so what is
// left in the difference is the seat's play, and that pairing is what makes an
// edge of a few points resolvable at all. The seeds are ones no tuning has
// ever been measured against.
//
// Run from anywhere, no build needed - fsi compiles the library sources:
//
//   dotnet fsi benchmark/sage.fsx [games] [rolloutCap]
//
// 400 games takes a few minutes and resolves a difference of about five
// points. A quick check that it still runs: dotnet fsi benchmark/sage.fsx 4 50

#r "nuget: FSharp.Control.AsyncSeq, 4.15.0"

// Keep in compile order with src/Flip7.fsproj
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
#load "../src/Sage.fs"

open System
open System.IO
open FSharp.Control
open Flip7

let private argument (index: int) (fallback: int) : int =
    fsi.CommandLineArgs
    |> Array.tryItem (index + 1)
    |> Option.map int
    |> Option.defaultValue fallback

let games = argument 0 400
let cap = argument 1 400
let seat = "Sage"

// Caution is the hand score each starts to baulk at, spread the way a real
// table is: someone always banks early and someone always pushes on
let humans =
    [ "Alice", 16u; "Bob", 19u; "Chloe", 22u; "Dave", 25u ]
    |> List.map (fun (name, caution) -> name, Strategy.PlaysLikeAHuman caution, Targeting.PlaysSpitefully)

let par = 100.0 / float (List.length humans + 1)

let deciderWith (random: Random) (sage: Sage ref option) : Strategy.Decider =
    let canonical = Strategy.DecideWith random

    {
        Strategy.HitOrStand =
            fun strategy round turn player others finished decks ->
                match strategy, sage with
                | Strategy.Custom "Adaptive", Some sage ->
                    sage.Value.Decide random strategy round turn player others finished decks
                | strategy, _ -> canonical.HitOrStand strategy round turn player others finished decks
        Strategy.Target =
            fun targeting ask chooser candidates finished decks ->
                match targeting, sage with
                | Targeting.ChoosesExternally "Adaptive", Some sage ->
                    sage.Value.Aim random targeting ask chooser candidates finished decks
                | targeting, _ -> canonical.Target targeting ask chooser candidates finished decks
    }

// 1.0 for an outright win by the seat, 0.5 for a shared top score
let score (final: Instant option) : float =
    match final with
    | None -> 0.0
    | Some instant ->
        let mine =
            instant.Players
            |> List.pick (fun player -> if player.Name = seat then Some player.FirmScore else None)

        let best =
            instant.Players
            |> List.choose (fun player -> if player.Name <> seat then Some player.FirmScore else None)
            |> List.max

        if mine > best then 1.0
        elif mine = best then 0.5
        else 0.0

// Sage folds every instant as it arrives, the way interactive play does
let playSage (history: Instant list list) (seed: int) : float =
    let random = Random seed
    let sage = ref (Sage(history, rollouts = cap))

    Timeline.SimulateWithDecider
        random
        (deciderWith random (Some sage))
        ((seat, Strategy.Custom "Adaptive", Targeting.ChoosesExternally "Adaptive")
         :: humans)
    |> AsyncSeq.map (fun instant ->
        sage.Value <- Sage(instant, sage.Value)
        instant
    )
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

let playExpectedValue (seed: int) : float =
    let random = Random seed

    Timeline.SimulateWithDecider
        random
        (deciderWith random None)
        ((seat, Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully) :: humans)
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

let root = Path.Join(Path.GetTempPath(), $"flip7-sage-{Guid.NewGuid():N}")
let watch = Diagnostics.Stopwatch.StartNew()
Directory.CreateDirectory root |> ignore

try
    // Sage studies games between the humans first, as it would from timelines/
    let history =
        [ 1..8 ]
        |> List.map (fun index ->
            let directory = Path.Join(root, string index)
            let random = Random index

            Timeline.SimulateWithDecider random (deciderWith random None) humans
            |> Persistence.WriteTimelineEager directory
            |> AsyncSeq.iter ignore
            |> Async.RunSynchronously

            Persistence.ReadTimeline directory
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously
        )

    printfn $"{games} games against {List.length humans} people, rollout cap {cap}"

    let outcomes =
        [| 1..games |]
        |> Array.map (fun index ->
            let seed = 500000 + index
            let sage = playSage history seed
            let expected = playExpectedValue seed

            if index % 100 = 0 then
                printfn $"  {index} games in, {watch.Elapsed.TotalMinutes:F1} min"

            sage, expected
        )

    let rate (values: float array) = Array.average values * 100.0
    let sageRate = rate (Array.map fst outcomes)
    let expectedRate = rate (Array.map snd outcomes)
    let spread (values: float array) =
        2.0
        * sqrt (Array.average values * (1.0 - Array.average values) / float values.Length)
        * 100.0

    // Game by game, because both arms met the same deal
    let differences = outcomes |> Array.map (fun (s, e) -> s - e)
    let mean = Array.average differences

    let error =
        differences
        |> Array.sumBy (fun d -> (d - mean) * (d - mean))
        |> fun squares -> sqrt (squares / float (differences.Length * (differences.Length - 1)))

    printfn ""
    printfn
        $"  Sage                  : %.1f{sageRate}%% +/- %.0f{spread (Array.map fst outcomes)}   (%.2f{sageRate / par}x par)"
    printfn
        $"  MaximizesExpectedValue: %.1f{expectedRate}%% +/- %.0f{spread (Array.map snd outcomes)}   (%.2f{expectedRate / par}x par)"
    printfn $"  par                   : %.1f{par}%%"
    printfn ""
    printfn
        $"  Sage over expected value: %+.1f{mean * 100.0} points, standard error %.1f{error * 100.0} (%.2f{abs mean / error} SE)"
    printfn $"done in {watch.Elapsed}"
finally
    Directory.Delete(root, true)
