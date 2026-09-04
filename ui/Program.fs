module Program

open System

[<EntryPoint>]
let main args =
    System.Diagnostics.Debug.Assert(
        Console.WindowWidth = 80 && Console.WindowHeight = 24,
        "Console window should be 80x24, please resize it."
    )

    match Array.toList args with
    // Replay a previously recorded game from a specified directory
    | [ "--replay"; directory ] when not (IO.Directory.Exists directory) ->
        eprintfn $"Replay directory not found: {directory}"
        1

    | [ "--replay"; directory ] ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Replay.Run directory directory
            Console.Clear()
            0
        finally
            Console.CursorVisible <- true

    // Simulate a game with specified player names and predefined strategies
    | "--simulate" :: names ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Simulate.Run names
            Console.Clear()
            0
        finally
            Console.CursorVisible <- true

    // Play interactively against the adaptive AI and three regulars
    | [ "--play"; name ] ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Play.Run name
            Console.Clear()
            0
        finally
            Console.CursorVisible <- true

    // Run an interactive game with specified player names
    | "--interactive" :: names ->
        try
            Console.Clear()
            Console.CursorVisible <- false
            Interactive.Run names
            Console.Clear()
            0
        finally
            Console.CursorVisible <- true

    // Usage
    | _ ->
        printfn "Usage:"
        printfn "  flip7.exe --replay <directory>"
        printfn "  flip7.exe --simulate <player1,strategy1> <player2,strategy2> ..."
        printfn "  flip7.exe --interactive <player1> <player2> ..."
        printfn "  flip7.exe --play <your-name>"
        1
