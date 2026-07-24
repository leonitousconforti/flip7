module Flip7.Analysis.Program

open System
open System.IO
open Flip7
open Flip7.Analysis

/// The hidden strategies the demo tries to recover. Carol's is deliberately
/// off the candidate grid to show degradation to the nearest candidate.
let private GroundTruth: (string * Strategy) list = [
    "Alice", HitUntilScore 24u
    "Bob", HitUntilNumCards 5u
    "Carol", RandomWithProbability 0.7
    "Dave", HitUntilBustProbability 0.4
]

let private bar (probability: float) : string =
    String.replicate (int (Math.Round(probability * 24.0))) "█"

let private describe (observations: Observation list) (model: PlayerModel) : unit =
    let mostLikely = Inference.MostLikely model
    let mine = observations |> List.filter (fun observation -> observation.Name = model.Name)
    let accuracy = Inference.Accuracy mostLikely mine

    printfn ""
    printfn $"%s{model.Name} — %d{model.Observations} decisions, %.0f{model.HitRate * 100.0}%% hits"
    printfn $"  best fit: %s{string mostLikely} (predicts %.0f{accuracy * 100.0}%% of their decisions)"

    for candidate, probability in model.Posterior |> List.truncate 5 do
        printfn $"  %5.1f{probability * 100.0}%%  %s{(string candidate).PadRight 26} %s{bar probability}"

let private report (timelines: Instant list list) : PlayerModel list =
    let observations = timelines |> List.collect (Seq.ofList >> Observation.FromTimeline)
    let models = Inference.Fit observations

    printfn
        $"%d{List.length timelines} timelines, %d{timelines |> List.sumBy List.length} instants, %d{List.length observations} voluntary decisions"

    for model in models do
        describe observations model

    models

let private analyze (directories: string list) : int =
    let timelines = directories |> List.map (Persistence.ReadTimeline >> Seq.toList)
    report timelines |> ignore
    0

let private demo (games: int) : int =
    let random = Random 7
    let root = Path.Combine(Path.GetTempPath(), "flip7-analysis-demo")

    if Directory.Exists root then
        Directory.Delete(root, true)

    let directories =
        List.init games (fun game ->
            let directory = Path.Combine(root, $"game-{game}")

            Timeline.SimulateWith random GroundTruth None None None None
            |> Seq.iteri (fun index instant ->
                Persistence.WriteInstant (Path.Combine(directory, string index)) instant |> ignore
            )

            directory
        )

    printfn $"simulated %d{games} games with hidden strategies and persisted them to %s{root}"
    printfn "reading the games back from disk and fitting player models..."
    printfn ""

    let timelines = directories |> List.map (Persistence.ReadTimeline >> Seq.toList)
    let models = report timelines

    printfn ""
    printfn "ground truth vs recovered:"

    for name, actual in GroundTruth do
        let recovered =
            models
            |> List.tryFind (fun model -> model.Name = name)
            |> Option.map (Inference.MostLikely >> string)
            |> Option.defaultValue "(no decisions observed)"

        let mark = if recovered = string actual then "✓" else "≈"
        printfn $"  %s{name}: played %s{string actual}, recovered %s{recovered} %s{mark}"

    0

[<EntryPoint>]
let main args =
    match Array.toList args with
    | [ "--demo" ] -> demo 5
    | [ "--demo"; games ] -> demo (int games)
    | "--analyze" :: directories when not (List.isEmpty directories) -> analyze directories
    | _ ->
        eprintfn "usage: dotnet run --project analysis -- --demo [games]"
        eprintfn "       dotnet run --project analysis -- --analyze <timeline-directory>..."
        1
