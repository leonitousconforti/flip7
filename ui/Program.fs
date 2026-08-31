module Program

open System

open Flip7

[<EntryPoint>]
let main args =
    match Array.toList args with
    // Replay a previously recorded game from a specified directory
    | [ "--replay"; directory ] ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Replay.Run directory
            0
        finally
            Console.CursorVisible <- true
            Console.Clear()

    // Simulate a game with specified player names and predefined strategies
    | "--simulate" :: names ->
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
        |> Replay.Run "simulated game"
        0

    // Run an interactive game with specified player names
    | "--interactive" :: names ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Interactive.Run names
            0
        finally
            Console.CursorVisible <- true
            Console.Clear()

    // Usage
    | _ ->
        printfn "Usage:"
        printfn "  flip7.exe --replay <directory>"
        printfn "  flip7.exe --simulate <player1> <player2> ..."
        printfn "  flip7.exe --interactive <player1> <player2> ..."

        1
