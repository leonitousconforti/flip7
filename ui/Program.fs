module Program

open System

open Argu

[<RequireQualifiedAccess>]
type public ReplayArgs =
    | [<MainCommand; ExactlyOnce>] Directory of directory: string
    | [<Unique>] Pace of milliseconds: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ReplayArgs.Directory _ -> "the directory of the recorded game"
            | ReplayArgs.Pace _ -> "milliseconds between ingested instants; lower fast-forwards the replay"

[<RequireQualifiedAccess>]
type public SimulateArgs =
    | [<MainCommand; Mandatory>] Players of players: string list
    | [<Unique>] Seed of seed: int
    | [<Unique>] Pace of milliseconds: int

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SimulateArgs.Players _ ->
                "one to five players, each a name optionally with a strategy: \"Alice,HitUntilScore 25\""
            | SimulateArgs.Seed _ -> "seed for the game's randomness, so the run is reproducible"
            | SimulateArgs.Pace _ -> "milliseconds between ingested instants; lower fast-forwards the game"

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

// Every mode shares the same console lifecycle: cleared and cursor hidden on
// the way in, cleared and cursor restored on the way out, even on a crash
let private withConsole (run: unit -> unit) : int =
    Diagnostics.Debug.Assert(
        Console.WindowWidth = 80 && Console.WindowHeight = 24,
        "Console window should be 80x24, please resize it."
    )

    try
        Console.Clear()
        Console.CursorVisible <- false
        run ()
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
        withConsole (fun () -> Replay.Run directory directory pace |> Async.RunSynchronously)
    | Some(Simulate simulate) ->
        let players = simulate.GetResult <@ SimulateArgs.Players @>
        let seed = simulate.TryGetResult <@ SimulateArgs.Seed @>
        let pace = simulate.TryGetResult <@ SimulateArgs.Pace @>
        withConsole (fun () -> Simulate.Run players seed pace)
    | Some(Play play) ->
        let names = play.GetResult <@ PlayArgs.Names @>
        let seed = play.TryGetResult <@ PlayArgs.Seed @>
        let pace = play.TryGetResult <@ PlayArgs.Pace @>
        withConsole (fun () -> Play.Run names seed pace)
    | None ->
        eprintfn "%s" (parser.PrintUsage())
        1
