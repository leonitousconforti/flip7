// Measures how much room is left above Sage.
//
// Winning nearly every game is not on offer in Flip 7: a round can end the
// moment any opponent flips seven, busting is the price of scoring at all,
// and three opponents each get their share of good decks. This script puts
// numbers on that. On shared seeds it reports how well the best non-adaptive
// strategy does from Sage's seat, whether Sage is still gaining from a larger
// rollout budget or has reached the plateau of its own architecture, and how
// far the field itself moves the ceiling.
//
//   dotnet fsi benchmark/ceiling.fsx [fixedGames] [sageGames]

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

let fixedGames = argument 0 100
let sageGames = argument 1 50
let seat = "Sage"

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

// A deliberately poor field, to show how much of a win rate is the opponents
// rather than the player
let weaklings =
    [ "Alice"; "Bob"; "Chloe" ]
    |> List.map (fun name -> name, Strategy.AlwaysHits, Targeting.ChoosesRandomly)

// The field that actually matters: people. Caution varies a little the way it
// does around a real table, and they aim spitefully, because everyone can see
// who is winning
let humans =
    [ "Alice", 16u; "Bob", 19u; "Chloe", 22u ]
    |> List.map (fun (name, caution) -> name, Strategy.PlaysLikeAHuman caution, Targeting.PlaysSpitefully)

// Routes the regulars' hidden strategies and Sage's asks; every other seat
// is decided the ordinary way
let deciderWith (random: Random) (sage: Sage ref option) : Strategy.Decider =
    let canonical = Strategy.DecideWith random

    {
        Strategy.HitOrStand =
            fun strategy round turn player others finished decks ->
                match strategy, sage with
                | Strategy.Custom "Adaptive", Some sage ->
                    sage.Value.Decide random strategy round turn player others finished decks
                | Strategy.Custom name, _ when Map.containsKey name hidden ->
                    canonical.HitOrStand (Map.find name hidden) round turn player others finished decks
                | strategy, _ -> canonical.HitOrStand strategy round turn player others finished decks
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

let playFixedAgainst
    (field: list<string * Strategy * Targeting>)
    (strategy: Strategy)
    (targeting: Targeting)
    (seed: int)
    : float =
    let random = Random seed

    Timeline.SimulateWithDecider random (deciderWith random None) ((seat, strategy, targeting) :: field)
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

let playSageAgainst
    (field: list<string * Strategy * Targeting>)
    (history: Instant list list)
    (rollouts: int)
    (seed: int)
    : float =
    let random = Random seed
    let sage = ref (Sage(history, rollouts = rollouts))

    Timeline.SimulateWithDecider
        random
        (deciderWith random (Some sage))
        ((seat, Strategy.Custom "Adaptive", Targeting.ChoosesExternally "Adaptive")
         :: field)
    |> AsyncSeq.map (fun instant ->
        sage.Value <- Sage(instant, sage.Value)
        instant
    )
    |> AsyncSeq.tryLast
    |> Async.RunSynchronously
    |> score

let playSage (history: Instant list list) (rollouts: int) (seed: int) : float =
    playSageAgainst opponents history rollouts seed

let playFixed (strategy: Strategy) (targeting: Targeting) (seed: int) : float =
    playFixedAgainst opponents strategy targeting seed

let report (label: string) (outcomes: float list) : unit =
    let rate = List.sum outcomes / float outcomes.Length * 100.0
    // Two standard errors on a win rate, so a row's noise is visible
    let error =
        2.0 * sqrt (rate / 100.0 * (1.0 - rate / 100.0) / float outcomes.Length) * 100.0
    printfn $"  %34s{label}: %5.1f{rate}%% +/- %.0f{error}"

let root = Path.Join(Path.GetTempPath(), $"flip7-ceiling-{Guid.NewGuid():N}")

let trainAgainst (field: list<string * Strategy * Targeting>) (seed: int) (directory: string) : Instant list =
    let random = Random seed
    Timeline.SimulateWithDecider random (deciderWith random None) field
    |> Persistence.WriteTimelineEager directory
    |> AsyncSeq.iter ignore
    |> Async.RunSynchronously

    Persistence.ReadTimeline directory
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously

let watch = Diagnostics.Stopwatch.StartNew()
Directory.CreateDirectory root |> ignore

try
    let fixedSeeds = [ 1001 .. 1000 + fixedGames ]
    let sageSeeds = [ 1001 .. 1000 + sageGames ]

    printfn $"the best a fixed strategy manages from Sage's seat ({fixedGames} games)"

    let candidates =
        [ Strategy.MaximizesExpectedValue; Strategy.AlwaysHits ]
        @ ([ 14u .. 2u .. 30u ] |> List.map Strategy.HitUntilScore)
        @ ([ 3u .. 5u ] |> List.map Strategy.HitUntilNumCards)
        @ ([ 0.2; 0.3; 0.4; 0.5 ] |> List.map Strategy.HitUntilBustProbability)
        @ [ Strategy.ChasesFlip7(20u, 5u); Strategy.HitUntilTotal 200u ]

    let ranked =
        candidates
        |> List.map (fun candidate ->
            candidate,
            fixedSeeds
            |> List.map (playFixed candidate Targeting.PlaysSpitefully)
            |> List.average
        )
        |> List.sortByDescending snd

    for candidate, rate in ranked |> List.truncate 6 do
        printfn $"  %34s{string candidate}: %5.1f{rate * 100.0}%%"

    printfn ""
    printfn $"Sage as its rollout budget grows, trained on 8 games ({sageGames} games)"

    let history =
        [ 1..8 ]
        |> List.map (fun index -> trainAgainst opponents index (Path.Join(root, string index)))

    // Sage is only modelling the people if it watched the people: the human
    // row gets games between the humans to study, not games between the
    // regulars, whose strategies key differently
    let humanHistory =
        [ 1..8 ]
        |> List.map (fun index -> trainAgainst humans index (Path.Join(root, $"human{index}")))

    for rollouts in [ 50; 150; 400 ] do
        sageSeeds
        |> List.map (playSage history rollouts)
        |> report $"Sage at {rollouts} rollouts"

    printfn ""
    printfn $"how much of a win rate is the field rather than the player ({sageGames} games)"

    sageSeeds
    |> List.map (playSageAgainst weaklings history 150)
    |> report "Sage vs three AlwaysHits"

    sageSeeds
    |> List.map (playSageAgainst humans humanHistory 150)
    |> report "Sage vs three average humans"

    sageSeeds
    |> List.map (playFixedAgainst humans Strategy.MaximizesExpectedValue Targeting.PlaysSpitefully)
    |> report "expected value vs those humans"

    printfn ""
    printfn $"done in {watch.Elapsed}"
finally
    Directory.Delete(root, true)
