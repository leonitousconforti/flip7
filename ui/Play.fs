module public Play

open System
open System.IO
open System.Threading

open Flip7

let private width = 80
let private playerSlots = 5
let private paceMilliseconds = 400
let private footerRow = 21
let private playDirectory = Path.Join("timelines", "play")

type private QuitException() =
    inherit Exception()

// A quit raised inside the pull surfaces wrapped by the task machinery, so
// unwrap before matching
let rec private IsQuit (error: exn) : bool =
    match error with
    | :? QuitException -> true
    | :? AggregateException as aggregate -> aggregate.InnerExceptions |> Seq.exists IsQuit
    | error when not (isNull error.InnerException) -> IsQuit error.InnerException
    | _ -> false

let private CaptionStyle (event: Event) : string list =
    match event with
    | Busted _ -> [ Ansi.BrightRed ]
    | Flip7Achieved _ -> [ Ansi.BrightMagenta ]
    | RoundEnded _ -> [ Ansi.BrightYellow ]
    | Froze _ -> [ Ansi.BrightCyan ]
    | _ -> []

let private padded (s: string) : string =
    s + String.replicate (max 0 (width - visualLength s)) " "

let private WriteFooter (text: string) : unit =
    Console.SetCursorPosition(0, footerRow)
    printf "%s" (padded text)

// Renders one instant as a full frame, overwriting in place like the replay
// view so nothing flickers
let private RenderFrame (round: int) (instant: Instant) (footer: string) : unit =
    let actor = instant.Event.Actor()
    let rule = String.replicate width "─"

    let status =
        let left = "play: first to 200pts wins"
        let right = $"round {round}"
        let middle =
            String.replicate (max 0 (width - visualLength left - visualLength right)) " "
        left + middle + right

    let caption = string instant.Event

    Console.SetCursorPosition(0, 0)
    printfn "%s" (padded rule)
    printfn "%s" (padded status)
    printfn "%s" (padded (caption |> centered width |> styled (CaptionStyle instant.Event)))
    printfn "%s" (padded rule)

    for player in instant.Players do
        let onlyPlayerNotBusted =
            instant.Players
            |> List.forall (fun p -> Hand.IsBust p.Hand || p.Name = player.Name)

        let probabilityToBust =
            Simulation.probabilityToBust instant.Deck instant.Discards player.Hand onlyPlayerNotBusted
            |> fun p -> p * 100.0

        let tentativeScore =
            if Hand.IsBust player.Hand then
                0u
            else
                Hand.Score player.Hand

        let preamble =
            sprintf
                "%s %s (%dpts + %dpts?, %.2f%%): "
                player.Name
                (bustEmoji probabilityToBust)
                player.FirmScore
                tentativeScore
                probabilityToBust

        handRows 40 preamble player.Hand
        |> fun (top, mid, bot) ->
            let isActor = actor = Some player.Name
            let styles = if isActor then [ Ansi.Inverse ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) ->
            let isBust = Hand.IsBust player.Hand
            let styles = if isBust then [ Ansi.Dim; Ansi.Italic ] else []
            styled styles top, styled styles mid, styled styles bot
        |> fun (top, mid, bot) -> [ padded top; padded mid; padded bot ]
        |> String.concat "\n"
        |> printfn "%s"

    for _ in 1 .. (playerSlots - List.length instant.Players) * 3 do
        printfn "%s" (padded "")

    printfn "%s" (padded rule)
    printfn "%s" (padded "")
    printf "%s" (padded footer)

let public Run (humanNames: string list) : unit =
    if humanNames.Length < 1 || humanNames.Length > 5 then
        raise (ArgumentException "Please provide one to five player names as command-line arguments.")

    if humanNames |> List.exists String.IsNullOrWhiteSpace then
        raise (ArgumentException "Player names must not be empty.")

    if humanNames |> List.distinct |> List.length <> humanNames.Length then
        raise (ArgumentException "Player names must be unique.")

    // The AIs fill whatever seats the humans leave open at the five-player table
    let botNames =
        [ "Ada"; "Bea"; "Cyd"; "Dee"; "Eve" ]
        |> List.take (playerSlots - humanNames.Length)

    match humanNames |> List.tryFind (fun name -> botNames |> List.contains name) with
    | Some taken -> raise (ArgumentException $"{taken} is taken by one of the AIs, please pick another name.")
    | None -> ()

    let random = Random()

    // The AI seats get distinct non-trivial strategies drawn fresh each game
    let pool = [
        HitUntilScore 22u
        HitUntilScore 26u
        HitUntilNumCards 5u
        HitUntilBustProbability 0.35
        HitUntilNaiveBustProbability 0.4
        SoftHitUntilScore(24u, 3.0)
        ChasesFlip7(20u, 5u)
        EmboldenedBySecondChance 22u
        HitWhileBehindLeader 12u
        MaximizesExpectedValue
    ]

    let naive =
        pool |> List.sortBy (fun _ -> random.Next()) |> List.take botNames.Length

    let players =
        (humanNames |> List.map (fun name -> name, Prompt)) @ List.zip botNames naive
        |> List.sortBy (fun _ -> random.Next())

    let directory =
        Path.Join(playDirectory, DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss")

    let promptHuman
        (player: Strategy.StrategyPlayer)
        (others: Strategy.StrategyPlayer list)
        (decks: Deck * Deck)
        : Strategy.HitOrStand =
        let deck, discards = decks

        let bust =
            Simulation.probabilityToBust deck discards player.Hand (List.isEmpty others)
            * 100.0

        let ev = Simulation.expectedValueOfHit deck discards player.Hand

        // Keys pressed while the bots were playing are not decisions
        while Console.KeyAvailable do
            Console.ReadKey true |> ignore

        $"{player.Name}: %d{Hand.Score player.Hand}pts held, %.0f{bust}%% bust, EV %+.1f{ev}   [h]it   [s]tand   [q]uit"
        |> centered width
        |> styled [ Ansi.Bright ]
        |> WriteFooter

        let rec read () =
            match (Console.ReadKey true).Key with
            | ConsoleKey.H -> Strategy.Hit
            | ConsoleKey.S -> Strategy.Stand
            | ConsoleKey.Q
            | ConsoleKey.Escape -> raise (QuitException())
            | _ -> read ()

        read ()

    let decide: Decider =
        fun strategy round turn player others _finished decks ->
            match strategy with
            | Prompt -> promptHuman player others decks
            | strategy -> Strategy.DecideWith random strategy round turn player others decks

    // The play loop is single-threaded: pulling the timeline drives the game,
    // so the human prompt blocks inside the pull and every instant is on disk
    // (write-through) before it is rendered
    let timeline =
        Timeline.SimulateWithDecider random decide players None None None None
        |> Persistence.WriteTimelineLazy directory

    let enumerator = timeline.GetAsyncEnumerator()

    let pull () =
        enumerator.MoveNextAsync().AsTask() |> Async.AwaitTask |> Async.RunSynchronously

    let mutable instants: Instant list = []
    let mutable rounds = 0
    let mutable finished = false

    Console.Clear()

    try
        try
            let mutable pulling = true

            while pulling do
                match pull () with
                | false ->
                    pulling <- false
                    finished <- true
                | true ->
                    let instant = enumerator.Current
                    instants <- instant :: instants
                    let displayRound = rounds + 1

                    if instant.Event.IsRoundEnded then
                        rounds <- rounds + 1

                    let footer =
                        "[q at your turn] quit" |> centered width |> styled [ Ansi.Dim; Ansi.Cyan ]

                    RenderFrame displayRound instant footer

                    Thread.Sleep(
                        if instant.Event.IsRoundEnded then
                            2 * paceMilliseconds
                        else
                            paceMilliseconds
                    )
        with error when IsQuit error ->
            ()
    finally
        enumerator.DisposeAsync().AsTask() |> Async.AwaitTask |> Async.RunSynchronously

    if finished && not (List.isEmpty instants) then
        let final = List.head instants
        let winner = final.Players |> List.maxBy (fun player -> player.FirmScore)

        $"game over: {winner.Name} wins with {winner.FirmScore}pts!   [any key]"
        |> centered width
        |> styled [ Ansi.BrightGreen ]
        |> WriteFooter

        Console.ReadKey true |> ignore
