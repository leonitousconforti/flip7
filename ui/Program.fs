module Program

open System

open Flip7

let private runReplay (source: string) (timeline: Instant array) : int =
    Console.Clear()
    Console.CursorVisible <- false

    try
        Replay.Run source timeline
    finally
        Console.CursorVisible <- true
        Console.Clear()

    0

[<EntryPoint>]
let main args =
    match Array.toList args with
    // Scrub through a previously persisted timeline
    | [ "--replay"; directory ] -> Persistence.ReadTimeline directory |> Seq.toArray |> runReplay directory

    // Simulate a full game and scrub through it immediately
    | "--simulate" :: names when names.Length > 0 && names.Length <= 5 ->
        let strategies = [
            Strategy.Random
            HitUntilScore 25u
            HitUntilNumCards 4u
            RandomWithProbability 0.75
            AlwaysHits
        ]

        let players =
            names
            |> List.mapi (fun index name -> name, strategies[index % strategies.Length])

        Timeline.Simulate players None None None None
        |> Seq.toArray
        |> runReplay "simulated game"

    | _ ->
        Interactive.runInteractive args
        0
