// Sage's win rate at a table of people.
//
// Five seats: Sage and four PlaysLikeAHuman opponents, everyone aiming their
// action cards spitefully, which is what anyone does once they can see who is
// winning. Par is a fifth of the games, so that is what a seat wins by turning
// up; anything above it is the seat playing better than the table.
//
// The same seeds are played twice, once with Sage in the seat and once with
// MaximizesExpectedValue, and the two are compared game by game rather than
// rate against rate. Both arms meet the same deal on a given seed, so what is
// left in the difference is the seat's play, and that pairing is what makes an
// edge of a few points resolvable at all. The seeds are ones no tuning has ever
// been measured against.
//
// Sage plays twice over: once having studied games these same people played
// before, and once sitting down knowing nothing, which is what a first evening
// looks like. The gap between those two is what the history is worth. Sharing
// the lead counts as winning it.
//
// Run from anywhere, no build needed - fsi compiles the library sources:
//
//   dotnet fsi benchmark/sage.fsx [games] [rolloutCap] [trainingGames]
//
// 400 games takes a few minutes and resolves a difference of about five points.
// A quick check that it still runs: dotnet fsi benchmark/sage.fsx 4 50

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
open System.Threading
open FSharp.Control
open Flip7

let private argument (index: int) (fallback: int) : int =
    fsi.CommandLineArgs
    |> Array.tryItem (index + 1)
    |> Option.map int
    |> Option.defaultValue fallback

let games = argument 0 400
let cap = argument 1 400
let trainingGames = argument 2 8

let humans =
    [ "Alice", 12u; "Bob", 19u; "Chloe", 22u; "Dave", 27u ]
    |> List.map (fun (name, caution) -> name, Strategy.PlaysLikeAHuman caution, Targeting.PlaysSpitefully)

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

let playSage (history: Instant list list) (seed: int) : Async<bool> = async {
    let random = Random seed
    let sage = ref (Sage(history, rollouts = cap))
    let decide = deciderWith random (Some sage)
    let players =
        ("Sage", Strategy.Custom "Adaptive", Targeting.ChoosesExternally "Adaptive")
        :: humans

    let! instant =
        (random, decide, players)
        |||> Timeline.SimulateWithDecider
        |> AsyncSeq.map (fun instant ->
            sage.Value <- Sage(instant, sage.Value)
            instant
        )
        |> AsyncSeq.last

    let sage, otherPlayers =
        instant.Players |> List.partition (fun player -> player.Name = "Sage")

    let bestPlayer = otherPlayers |> List.maxBy (fun player -> player.FirmScore)
    return sage.Head.FirmScore >= bestPlayer.FirmScore
}

let playExpectedValue (seed: int) : Async<bool> = async {
    let random = Random seed
    let decide = deciderWith random None
    let players =
        ("Sage", Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully) :: humans

    let! instant =
        (random, decide, players) |||> Timeline.SimulateWithDecider |> AsyncSeq.last

    let sage, otherPlayers =
        instant.Players |> List.partition (fun player -> player.Name = "Sage")

    let bestPlayer = otherPlayers |> List.maxBy (fun player -> player.FirmScore)
    return sage.Head.FirmScore >= bestPlayer.FirmScore
}

let history =
    [ 1..trainingGames ]
    |> List.map (fun index ->
        let random = Random(900000 + index)
        let decide = deciderWith random None

        Timeline.SimulateWithDecider random decide humans
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously
    )

let watch = Diagnostics.Stopwatch.StartNew()
let played = ref 0

let outcomes =
    [| 1..games |]
    |> Array.map (fun index -> async {
        let seed = 500000 + index
        let! studied = playSage history seed |> Async.StartChild
        let! fresh = playSage List.empty seed |> Async.StartChild
        let! expected = playExpectedValue seed |> Async.StartChild

        let! studiedResult = studied
        let! freshResult = fresh
        let! expectedResult = expected

        let finished = Interlocked.Increment(&played.contents)
        let step = max 1 (games / 10)

        if finished % step = 0 || finished = games then
            printfn $"  %4d{finished}/{games} games, %.1f{watch.Elapsed.TotalMinutes} min"

        return studiedResult, freshResult, expectedResult
    })
    |> fun played -> Async.Parallel(played, maxDegreeOfParallelism = 4)
    |> Async.RunSynchronously

let studiedWon = outcomes |> Array.map (fun (studied, _, _) -> studied)
let freshWon = outcomes |> Array.map (fun (_, fresh, _) -> fresh)
let expectedWon = outcomes |> Array.map (fun (_, _, expected) -> expected)
let par = 100.0 / float (List.length humans + 1)

let rateOf (won: bool array) : float =
    won |> Array.averageBy (fun win -> if win then 1.0 else 0.0)

let spread (won: bool array) : float =
    let rate = rateOf won
    2.0 * sqrt (rate * (1.0 - rate) / float won.Length) * 100.0

let report (label: string) (won: bool array) : unit =
    let rate = rateOf won * 100.0
    printfn $"  %-23s{label}: %5.1f{rate}%% +/- %.0f{spread won}   (%.2f{rate / par}x par)"

// Game by game, because every seat met the same deal on a given seed. The
// difference is far quieter than either rate, which is the whole reason a few
// points can be told from nothing at this many games
let against (label: string) (better: bool array) (worse: bool array) : unit =
    let differences =
        Array.map2 (fun a b -> (if a then 1.0 else 0.0) - (if b then 1.0 else 0.0)) better worse

    let mean = Array.average differences

    let error =
        differences
        |> Array.sumBy (fun difference -> (difference - mean) * (difference - mean))
        |> fun squares -> sqrt (squares / float (differences.Length * (differences.Length - 1)))

    printfn
        $"  %-28s{label}: %+.1f{mean * 100.0} points, standard error %.1f{error * 100.0} (%.2f{abs mean / error} SE)"

printfn ""
report "Sage, having studied" studiedWon
report "Sage, knowing nothing" freshWon
report "MaximizesExpectedValue" expectedWon
printfn "  %-23s: %5.1f%%" "par" par
printfn ""
against "Sage over expected value" studiedWon expectedWon
against "what the history is worth" studiedWon freshWon
printfn $"done in {watch.Elapsed}"
