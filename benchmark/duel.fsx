// Settles one question: is Sage worth more than plain expected-value play
// against people?
//
// The same seat, the same field of three average humans, the same seeds, once
// with Sage in it and once with MaximizesExpectedValue. Both arms start from
// an identical deal on each seed, so the comparison is taken seed by seed
// rather than rate against rate, which is what makes an edge of a few points
// resolvable at all. The seeds are deliberately ones no tuning has ever seen.
//
//   dotnet fsi benchmark/duel.fsx [games] [rolloutCap]

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
open FSharp.Control
open Flip7

let private argument (index: int) (fallback: int) : int =
    fsi.CommandLineArgs
    |> Array.tryItem (index + 1)
    |> Option.map int
    |> Option.defaultValue fallback

let games = argument 0 800
let cap = argument 1 400
let seat = "Sage"

let humans =
    [ "Alice", 16u; "Bob", 19u; "Chloe", 22u ]
    |> List.map (fun (name, caution) -> name, Strategy.PlaysLikeAHuman caution, Targeting.PlaysSpitefully)

let deciderWith (random: Random) (sage: Sage ref option) : Strategy.Decider =
    let canonical = Strategy.DecideWith random

    {
        Strategy.HitOrStand =
            fun strategy round turn player others finished decks ->
                match strategy, sage with
                | Strategy.Custom "Adaptive", Some sage ->
                    sage.Value.Decide random strategy round turn player others finished decks
                | _ -> canonical.HitOrStand strategy round turn player others finished decks
        Strategy.Target =
            fun targeting ask chooser candidates finished decks ->
                match targeting, sage with
                | Targeting.ChoosesExternally "Adaptive", Some sage ->
                    sage.Value.Aim random targeting ask chooser candidates finished decks
                | targeting, _ -> canonical.Target targeting ask chooser candidates finished decks
    }

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

let root = IO.Path.Join(IO.Path.GetTempPath(), $"flip7-duel-{Guid.NewGuid():N}")

let watch = Diagnostics.Stopwatch.StartNew()
IO.Directory.CreateDirectory root |> ignore

try
    // Sage studies games between the humans, as it would from timelines/
    let history =
        [ 1..8 ]
        |> List.map (fun index ->
            let random = Random index
            Timeline.SimulateWithDecider random (deciderWith random None) humans
            |> Persistence.WriteTimelineEager(IO.Path.Join(root, string index))
            |> AsyncSeq.iter ignore
            |> Async.RunSynchronously

            Persistence.ReadTimeline(IO.Path.Join(root, string index))
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously
        )

    printfn $"{games} games each, rollout cap {cap}, seeds no tuning has seen"

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

    // Taken seed by seed: both arms met the same deal, so what is left is the
    // difference the seat's play made
    let differences = outcomes |> Array.map (fun (s, e) -> s - e)
    let mean = Array.average differences

    let error =
        differences
        |> Array.sumBy (fun d -> (d - mean) * (d - mean))
        |> fun squares -> sqrt (squares / float (differences.Length * (differences.Length - 1)))

    printfn ""
    printfn $"  Sage                  : %.1f{sageRate}%%"
    printfn $"  MaximizesExpectedValue: %.1f{expectedRate}%%"
    printfn ""
    printfn $"  difference            : %+.1f{mean * 100.0} points, standard error %.1f{error * 100.0}"
    printfn $"  that is %.2f{abs mean / error} standard errors"

    printfn
        $"""  verdict               : {if abs mean > 2.0 * error then
                                           (if mean > 0.0 then "Sage is better" else "Sage is worse")
                                       else
                                           "no difference established"}"""

    printfn ""
    printfn $"done in {watch.Elapsed}"
finally
    IO.Directory.Delete(root, true)
