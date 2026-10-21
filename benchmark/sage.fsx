// Sage playing for Alice at a table that is against her.
//
//   dotnet fsi --optimize+ benchmark/sage.fsx [seed] [games] [studied]
//
// Sage answers for Alice and for Bob, who supports her. Charlie is the
// antagonist, and Dave and Eve support him: all three favour Charlie and oppose
// Alice when they aim an action card, and otherwise aim spitefully. They choose
// externally, so Sage learns their sides only from the aims it sees them make.
// Fiona and Gavin are neutral and aim greedily, as they declared.
//
// Before playing, Sage studies as many earlier games of the table as asked,
// none by default, with Alice and Bob in them played by stand-ins who play as
// Sage's seats do unaided. Each game Sage plays is the next of that sitting, so
// it starts knowing what the study showed of how everyone hits or stands and
// aims, and adds what the game shows it. One game of the table shows few aims
// at a choice of players, so it takes a study for Sage to know whose side the
// antagonists are on from the start.
//
// One game, the default, is told as Sage played it: every search, with where it
// was made, how many games it replayed, how it foresaw the game ending and how
// many of its answers Sage played before the game left the plan, each of those
// where the plan departed from what Alice or Bob would answer unaided, then how
// the game ended and what Sage made of everyone by its last question. More
// games play that many seeds from the given one, a line each, and Alice's
// record.

#r "nuget: FSharp.Control.AsyncSeq, 4.15.0"

// Keep in compile order with src/Flip7.fsproj
#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
#load "../src/Lookahead.fs"
#load "../src/Player.fs"
#load "../src/Instant.fs"
#load "../src/Strategy.fs"
#load "../src/Timeline.fs"
#load "../src/Persistence.fs"
#load "../src/Observation.fs"
#load "../src/Inference.fs"
#load "../src/Oracle.fs"
#load "../src/Sage.fs"

open Flip7
open FSharp.Control

let argument (index: int) (fallback: int) : int =
    fsi.CommandLineArgs
    |> Array.tryItem index
    |> Option.map int
    |> Option.defaultValue fallback

let seed = argument 1 1
let games = argument 2 1
let studied = argument 3 0

let table =
    [
        "Alice", (Strategy.Custom "Alice", Targeting.ChoosesExternally "Alice")
        "Bob", (Strategy.Custom "Bob", Targeting.ChoosesExternally "Bob")
        "Charlie", (Strategy.MaximizesExpectedValue, Targeting.ChoosesExternally "Antagonists")
        "Dave", (Strategy.HitUntilScore 26u, Targeting.ChoosesExternally "Antagonists")
        "Eve", (Strategy.HitUntilNumCards 4u, Targeting.ChoosesExternally "Antagonists")
        "Fiona", (Strategy.PlaysLikeAHuman 20u, Targeting.PlaysGreedily(0.2, 0.2))
        "Gavin", (Strategy.ChasesFlip7(20u, 5u), Targeting.PlaysGreedily(0.3, 0.2))
    ]
    |> Map.ofList

let antagonism =
    Targeting.TakesSides(Some "Charlie", Some "Alice", Targeting.PlaysSpitefully)

let deciderWithAntagonists =
    Strategy.Decider.Default
    |> Strategy.addTargetingStrategy "Antagonists" (fun random -> Strategy.Decider.Default.Target random antagonism)

let study =
    let standIns =
        table
        |> Map.remove "Alice"
        |> Map.remove "Bob"
        |> Map.add "Alice" (Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully)
        |> Map.add "Bob" (Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully)
        |> Map.toList
        |> List.map (fun (name, (strategy, targeting)) -> name, strategy, targeting)

    [
        for game in 1..studied ->
            Timeline.SimulateWithDecider (System.Random(1_000_000 + game)) deciderWithAntagonists standIns
    ]
    |> Study.OfTimelines
    |> Async.RunSynchronously

let play (seed: int) : Async<Sage * Player list> = async {
    let sage = Sage("Alice", supporters = [ "Bob" ], study = study)

    let table' =
        table
        |> Map.toList
        |> List.map (fun (name, (strategy, targeting)) -> name, strategy, targeting)

    let deciderWithSage =
        sage.Strategies
        |> Map.fold
            (fun decider name (hitOrStand, target) ->
                decider
                |> Strategy.addHitOrStandStrategy name hitOrStand
                |> Strategy.addTargetingStrategy name target
            )
            deciderWithAntagonists

    let! game =
        Timeline.SimulateWithDecider (System.Random seed) deciderWithSage table'
        |> AsyncSeq.toListAsync

    return sage, (List.last game).Players
}

let marginOf (players: Player list) : int =
    let alice, others = players |> List.partition (fun player -> player.Name = "Alice")

    int (List.exactlyOne alice).FirmScore
    - (others |> List.map (fun player -> int player.FirmScore) |> List.max)

let outcome (margin: int) : string =
    if margin > 0 then $"wins by {margin}"
    elif margin = 0 then "ties for the lead"
    else $"falls {-margin} short"

let having =
    match studied with
    | 0 -> "having studied no earlier games"
    | 1 -> "having studied 1 earlier game"
    | studied -> $"having studied {studied} earlier games"

if games = 1 then
    let sage, players = play seed |> Async.RunSynchronously

    printfn $"seed {seed}, Sage {having}"
    printfn ""

    for number, plan in List.indexed sage.Plans do
        let where =
            match plan.Answers with
            | Answer.HitOrStand(seat, round, turn, _, _) :: _ -> $"{seat} on turn {turn} of round {round}"
            | _ -> "an aim"

        printfn
            $"search {number + 1}, for {where}: {plan.Replays} replays, Alice {outcome plan.Margin}; {plan.Played} of {plan.Answers.Length} answers played"

        for index in plan.Departures do
            if index < plan.Played then
                printfn $"    departs: {plan.Answers[index]}"

    let margin = marginOf players

    printfn ""
    printfn $"Alice {outcome margin}"

    for player in players |> List.sortByDescending (fun player -> player.FirmScore) do
        printfn $"    {player.Name, -8}{player.FirmScore, 4}"

    match List.tryLast sage.Plans with
    | None -> printfn "Sage never pinned the seed, so it never searched"
    | Some plan when plan.Margin = margin -> printfn "as Sage's last plan foresaw"
    | Some plan -> printfn $"where Sage's last plan foresaw Alice {outcome plan.Margin}"

    printfn ""
    printfn "What Sage made of everyone by its last question:"

    for name, _ in table |> Map.toList do
        if not (sage.Strategies.ContainsKey name) then
            let strategy, aiming =
                match sage.ModelOf name with
                | None -> "never seen deciding", "never seen aiming at one of several players"
                | Some model ->
                    let strategy, sure = List.head model.StrategyPosterior
                    let targeting, aimSure = List.head model.TargetingPosterior

                    (if model.Observations > 0 then
                         $"{strategy} (%.0f{sure * 100.0}%% sure, {model.Observations} decisions)"
                     else
                         "never seen deciding"),
                    (if model.Aims > 0 then
                         $"{targeting} (%.0f{aimSure * 100.0}%% sure, {model.Aims} aims)"
                     else
                         "never seen aiming at one of several players")

            printfn $"    {name, -8}{strategy}"
            printfn $"            {aiming}"
else
    let results =
        [ seed .. seed + games - 1 ]
        |> List.map (fun seed -> async {
            let! sage, players = play seed
            return seed, sage.Plans, marginOf players
        })
        |> fun games -> Async.Parallel(games, System.Environment.ProcessorCount)
        |> Async.RunSynchronously

    for seed, plans, margin in results do
        let departures =
            plans
            |> List.sumBy (fun plan -> plan.Departures |> List.filter (fun index -> index < plan.Played) |> List.length)

        printfn $"seed {seed}: Alice {outcome margin} after {plans.Length} searches and {departures} departures"

    let won = results |> Array.filter (fun (_, _, margin) -> margin > 0) |> Array.length
    printfn $"Alice won {won} of {games}, Sage {having}"
