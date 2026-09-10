module Program

open System

open Argu

[<RequireQualifiedAccess>]
type public ReplayArgs =
    | [<MainCommand; ExactlyOnce>] Directory of directory: string
    | [<Unique>] Pace of milliseconds: int
    | [<Unique>] CacheCapacity of capacity: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ReplayArgs.Directory _ -> "the directory of the recorded game"
            | ReplayArgs.Pace _ -> "milliseconds between ingested instants; lower fast-forwards the replay"
            | ReplayArgs.CacheCapacity _ ->
                "the number of instants to cache in memory; higher uses more memory but allows faster scrubbing"

[<RequireQualifiedAccess>]
type public SimulateArgs =
    | [<Mandatory>] Player of player: string
    | [<Unique>] Seed of seed: int
    | [<Unique>] Pace of milliseconds: int
    | [<Unique>] CacheCapacity of capacity: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SimulateArgs.Player _ ->
                "a player: a name, optionally with a strategy and then a targeting policy after commas: \"Alice,HitUntilScore 25,PlaysSpitefully\"; repeat for each of up to five players"
            | SimulateArgs.Seed _ -> "seed for the game's randomness, so the run is reproducible"
            | SimulateArgs.Pace _ -> "milliseconds between ingested instants; lower fast-forwards the game"
            | SimulateArgs.CacheCapacity _ ->
                "the number of instants to cache in memory; higher uses more memory but allows faster scrubbing"

[<RequireQualifiedAccess>]
type public PlayArgs =
    | [<MainCommand; Mandatory>] Names of names: string list
    | [<Unique>] Seed of seed: int
    | [<Unique>] Pace of milliseconds: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | PlayArgs.Names _ -> "one to five human names; AIs fill the remaining seats"
            | PlayArgs.Seed _ -> "seed for the game's randomness, so the run is reproducible"
            | PlayArgs.Pace _ -> "milliseconds between dealt instants"

type public Arguments =
    | [<CliPrefix(CliPrefix.None)>] Replay of ParseResults<ReplayArgs>
    | [<CliPrefix(CliPrefix.None)>] Simulate of ParseResults<SimulateArgs>
    | [<CliPrefix(CliPrefix.None)>] Play of ParseResults<PlayArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Replay _ -> "replay a previously recorded game"
            | Simulate _ -> "simulate a full game and scrub through it as it unfolds"
            | Play _ -> "play interactively; AIs fill the seats the humans leave open"

let parseSimulatePlayers (simulate: ParseResults<SimulateArgs>) =
    simulate.GetResults <@ SimulateArgs.Player @>
    |> List.map (fun player ->
        let parts = player.Split ','

        let strategy =
            match parts with
            | [| _ |] -> Flip7.Strategy.Random
            | _ ->
                match Flip7.Strategy.TryParse parts[1] with
                | Some strategy -> strategy
                | None -> simulate.Raise $"invalid strategy for {parts[0]}: {parts[1]}"

        let targeting =
            match parts with
            | [| _ |]
            | [| _; _ |] -> Flip7.Targeting.ChoosesRandomly
            | _ ->
                match Flip7.Targeting.TryParse parts[2] with
                | Some targeting -> targeting
                | None -> simulate.Raise $"invalid targeting for {parts[0]}: {parts[2]}"

        parts[0], strategy, targeting
    )

// Every mode shares the same console lifecycle - cleared and cursor hidden on
// the way in, cleared and cursor restored on the way out, even on a crash -
// and this is the one place the program blocks: every mode is async all the
// way down to here
let private withConsole (run: Async<unit>) : int =
    Diagnostics.Debug.Assert(
        Console.WindowWidth = 80 && Console.WindowHeight = 24,
        "Console window should be 80x24, please resize it."
    )

    try
        Console.Clear()
        Console.CursorVisible <- false
        Async.RunSynchronously run
        Console.Clear()
        0
    finally
        Console.CursorVisible <- true

[<EntryPoint>]
let main args =
    let parser =
        ArgumentParser.Create<Arguments>(programName = "flip7", errorHandler = ProcessExiter())

    let results = parser.ParseCommandLine args

    match results.TryGetSubCommand() with
    | Some(Replay replay) ->
        let directory = replay.GetResult <@ ReplayArgs.Directory @>
        let pace = replay.TryGetResult <@ ReplayArgs.Pace @>
        let cacheCapacity = replay.TryGetResult <@ ReplayArgs.CacheCapacity @>
        withConsole (Replay.Run directory directory pace cacheCapacity)
    | Some(Simulate simulate) ->
        let players = parseSimulatePlayers simulate
        let seed = simulate.TryGetResult <@ SimulateArgs.Seed @>
        let pace = simulate.TryGetResult <@ SimulateArgs.Pace @>
        let cacheCapacity = simulate.TryGetResult <@ SimulateArgs.CacheCapacity @>
        withConsole (Simulate.Run players seed pace cacheCapacity)
    | Some(Play play) ->
        let names = play.GetResult <@ PlayArgs.Names @>
        let seed = play.TryGetResult <@ PlayArgs.Seed @>
        let pace = play.TryGetResult <@ PlayArgs.Pace @>
        withConsole (Play.Run names seed pace)
    | None ->
        eprintfn "%s" (parser.PrintUsage())
        1
