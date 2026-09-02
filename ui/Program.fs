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
            Replay.Run directory None
            Console.Clear()
            0
        with ex ->
            Console.CursorVisible <- true
            printfn "Error: %s" ex.Message
            1

    // Simulate a game with specified player names and predefined strategies
    | "--simulate" :: names ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Simulate.Run names
            Console.Clear()
            0
        with ex ->
            Console.CursorVisible <- true
            printfn "Error: %s" ex.Message
            1

    // Run an interactive game with specified player names
    | "--interactive" :: names ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Interactive.Run names
            Console.Clear()
            0
        with ex ->
            Console.CursorVisible <- true
            printfn "Error: %s" ex.Message
            1

    // Usage
    | _ ->
        printfn "Usage:"
        printfn "  flip7.exe --replay <directory>"
        printfn "  flip7.exe --simulate <player1> <player2> ..."
        printfn "  flip7.exe --interactive <player1> <player2> ..."
        1
