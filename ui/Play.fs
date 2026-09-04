module public Play

open System
open System.IO
open System.Threading

open FSharp.Control

open Flip7
open Flip7.Analysis

let private width = 80
let private playerSlots = 5
let private paceMilliseconds = 400
let private footerRow = 21
let private historyDirectory = Path.Join("timelines", "play")

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

let public Run (humanName: string) : unit =
    let botNames = [ "Sage"; "Ada"; "Bea"; "Cyd" ]

    if humanName |> String.IsNullOrWhiteSpace then
        raise (ArgumentException "Please provide your name as a command-line argument.")

    if botNames |> List.contains humanName then
        raise (ArgumentException $"{humanName} is taken by one of the AIs, please pick another name.")

    let random = Random()

    // Every previously persisted play session is training data: Sage starts
    // each new game already knowing how you played the earlier ones. Games
    // that fail to load (quit mid-write, or from an older format) are skipped.
    let history =
        if Directory.Exists historyDirectory then
            Directory.GetDirectories historyDirectory
            |> Array.toList
            |> List.choose (fun dir ->
                try
                    Persistence.ReadTimeline dir
                    |> AsyncSeq.toListAsync
                    |> Async.RunSynchronously
                    |> Some
                with _ ->
                    None
            )
        else
            []

    let sage = SuperAI history

    // The naive table: three distinct non-trivial strategies drawn fresh each
    // game, so Sage has opponents to model too
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

    let naive = pool |> List.sortBy (fun _ -> random.Next()) |> List.take 3

    let players =
        [ humanName, Prompt; "Sage", Adaptive ] @ List.zip [ "Ada"; "Bea"; "Cyd" ] naive
        |> List.sortBy (fun _ -> random.Next())

    let directory =
        Path.Join(historyDirectory, DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss")

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

        $"your turn: %d{Hand.Score player.Hand}pts held, %.0f{bust}%% bust, EV %+.1f{ev}   [h]it   [s]tand   [q]uit"
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
        fun strategy round turn player others decks ->
            match strategy with
            | Prompt -> promptHuman player others decks
            | Adaptive -> sage.Decide random round turn player others decks
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

                    // The learning step: refit Sage's models of everyone at
                    // the table from all past games plus this one so far
                    if instant.Event.IsRoundEnded then
                        sage.Learn(List.rev instants)

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

        let learned =
            match sage.ModelOf humanName with
            | Some model ->
                let strategy, probability = List.head model.Posterior
                $"Sage read you as {strategy} (%.0f{probability * 100.0}%% sure, {model.Observations} decisions)"
            | None -> "Sage never saw you decide"

        Console.SetCursorPosition(0, footerRow - 1)

        $"game over: {winner.Name} wins with {winner.FirmScore}pts!"
        |> centered width
        |> styled [ Ansi.BrightGreen ]
        |> padded
        |> printfn "%s"

        $"{learned}   [any key]"
        |> centered width
        |> styled [ Ansi.BrightCyan ]
        |> padded
        |> printf "%s"

        Console.ReadKey true |> ignore
