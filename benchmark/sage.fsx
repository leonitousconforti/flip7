// Measures how much persisted history Sage needs before it reliably wins.
//
// Three regulars play fixed strategies hidden behind Custom labels, so Sage
// has to infer them from observations exactly as it does for humans. Training
// games between the regulars are simulated and persisted to a temp directory
// through the real persistence layer, Sage is started from ever larger slices
// of that history, and its win rate is measured over a fixed set of
// evaluation games. The evaluation seeds are the same for every slice, so the
// comparison across training sizes is paired.
//
// Run from anywhere, no build needed - fsi compiles the library sources
// itself:
//
//   dotnet fsi benchmark/sage.fsx [evalGames] [rollouts] [maxTraining]
//
// Defaults are 10 evaluation games, 50 rollouts per decision, and training
// slices of 0/1/2/4/8/16 games; expect a few minutes. A quick smoke run:
// dotnet fsi benchmark/sage.fsx 2 10 1

#r "nuget: FSharp.Control.AsyncSeq, 4.15.0"

// Keep in compile order with src/Flip7.fsproj
#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
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

let evaluationGames = argument 0 10
let rolloutsPerDecision = argument 1 50
let maxTraining = argument 2 16
let trainingSizes =
    [ 0; 1; 2; 4; 8; 16 ] |> List.filter (fun size -> size <= maxTraining)

// The table Sage sits at: regulars whose true strategies hide behind Custom
// labels, so Sage must model them from data rather than read their labels
let hidden =
    Map [
        "Alice", Strategy.HitUntilScore 22u
        "Bob", Strategy.ChasesFlip7(24u, 6u)
        "Chloe", Strategy.EmboldenedBySecondChance 26u
    ]

let opponents =
    hidden
    |> Map.toList
    |> List.map (fun (name, _) -> name, Strategy.Custom name, Targeting.ChoosesRandomly)

// Routes each regular's Custom label to their hidden strategy, and Adaptive
// to Sage when one is seated
let deciderWith (random: Random) (sage: Sage ref option) : Strategy.Decider =
    let canonical = Strategy.DecideWith random

    {
        canonical with
            Strategy.HitOrStand =
                fun strategy round turn player others finished decks ->
                    match strategy, sage with
                    | Strategy.Custom "Adaptive", Some sage ->
                        sage.Value.Decide random strategy round turn player others finished decks
                    | Strategy.Custom name, _ when Map.containsKey name hidden ->
                        canonical.HitOrStand (Map.find name hidden) round turn player others finished decks
                    | strategy, _ -> canonical.HitOrStand strategy round turn player others finished decks
    }

// 1.0 for an outright win by Sage's seat, 0.5 for a shared top score
let score (final: Instant option) : float =
    match final with
    | None -> 0.0
    | Some instant ->
        let mine =
            instant.Players
            |> List.pick (fun player -> if player.Name = "Sage" then Some player.FirmScore else None)

        let best =
            instant.Players
            |> List.choose (fun player ->
                if player.Name <> "Sage" then
                    Some player.FirmScore
                else
                    None
            )
            |> List.max

        if mine > best then 1.0
        elif mine = best then 0.5
        else 0.0

// One evaluation game: Sage, trained on the given history, seated against the
// regulars, folding every instant as it streams by like interactive play does
let evaluate (history: Instant list list) (seed: int) : float =
    let random = Random seed
    let sage = ref (Sage(history, rollouts = rolloutsPerDecision))
    let lineup =
        ("Sage", Strategy.Custom "Adaptive", Targeting.ChoosesRandomly) :: opponents

    Timeline.SimulateWithDecider random (deciderWith random (Some sage)) lineup
    |> AsyncSeq.map (fun instant ->
        sage.Value <- Sage(instant, sage.Value)
        instant
    )
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

// The same seat playing plain expected value, as the no-modeling reference
let baseline (seed: int) : float =
    let random = Random seed
    let lineup =
        ("Sage", Strategy.MaximizesExpectedValue, Targeting.ChoosesRandomly)
        :: opponents

    Timeline.SimulateWithDecider random (deciderWith random None) lineup
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

let root = Path.Join(Path.GetTempPath(), $"flip7-sage-bench-{Guid.NewGuid():N}")

let train (seed: int) (directory: string) : Instant list =
    let random = Random seed

    Timeline.SimulateWithDecider random (deciderWith random None) opponents
    |> Persistence.WriteTimelineEager directory
    |> AsyncSeq.iter ignore
    |> Async.RunSynchronously

    Persistence.ReadTimeline directory
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously

let report (label: string) (outcomes: float list) : unit =
    let wins = outcomes |> List.filter ((=) 1.0) |> List.length
    let ties = outcomes |> List.filter ((=) 0.5) |> List.length
    let losses = outcomes.Length - wins - ties
    let rate = List.sum outcomes / float outcomes.Length * 100.0
    printfn $"  %25s{label}: %5.1f{rate}%%  ({wins} wins, {ties} ties, {losses} losses)"

let watch = Diagnostics.Stopwatch.StartNew()

Directory.CreateDirectory root |> ignore

try
    printfn $"persisting {maxTraining} training games under {root}"

    let history =
        [ 1..maxTraining ]
        |> List.map (fun index -> train index (Path.Join(root, string index)))

    let seeds = [ 1001 .. 1000 + evaluationGames ]

    printfn $"evaluating over {evaluationGames} games, {rolloutsPerDecision} rollouts per decision"
    seeds |> List.map baseline |> report "MaximizesExpectedValue"

    for size in trainingSizes do
        seeds
        |> List.map (evaluate (List.take size history))
        |> report $"Sage after {size} games"

    printfn $"done in {watch.Elapsed}"
finally
    Directory.Delete(root, true)
