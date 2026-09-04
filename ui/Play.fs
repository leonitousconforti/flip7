module public Play

open System
open System.IO
open System.Threading

open Flip7

let private width = 80
let private playerSlots = 5
let private paceMilliseconds = 400
let private footerRow = 21

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

// Console keys are broadcast as an event so a prompt can await one without
// holding a thread. A key fired while nobody is awaiting simply vanishes,
// which is exactly the rule that keys pressed during the bots' turns are not
// decisions. Awaiting resumes the game on the pump thread until its next
// async hop.
let private StartKeyPump () : IEvent<ConsoleKeyInfo> =
    let keyPressed = Event<ConsoleKeyInfo>()
    let pump =
        Thread(
            ThreadStart(fun () ->
                while true do
                    keyPressed.Trigger(Console.ReadKey true)
            )
        )

    pump.IsBackground <- true
    pump.Start()
    keyPressed.Publish

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

let private RenderFrame (round: int) (instant: Instant) (footer: string) : unit =
    let actor = instant.Event.Actor()
    let rule = String.replicate width "─"
    let caption = string instant.Event

    let status =
        let left = "play: first to 200pts wins"
        let right = $"round {round}"
        let leftWidth = visualLength left
        let rightWidth = visualLength right
        let middle = String.replicate (max 0 (width - leftWidth - rightWidth)) " "
        left + middle + right

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

    let botNames =
        [ "Alice"; "Bob"; "Chloe"; "Dave"; "Ethan" ]
        |> List.take (playerSlots - humanNames.Length)

    match humanNames |> List.tryFind (fun name -> botNames |> List.contains name) with
    | Some taken -> raise (ArgumentException $"{taken} is taken by one of the AIs, please pick another name.")
    | None -> ()

    let random = Random()
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

    let directory = Path.Join("timelines", DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss")
    let keys = StartKeyPump()

    let promptHuman
        (player: Strategy.StrategyPlayer)
        (others: Strategy.StrategyPlayer list)
        (decks: Deck * Deck)
        : Async<Strategy.HitOrStand> =
        let deck, discards = decks

        let bust =
            Simulation.probabilityToBust deck discards player.Hand (List.isEmpty others)
            * 100.0

        let ev = Simulation.expectedValueOfHit deck discards player.Hand

        $"{player.Name}: %d{Hand.Score player.Hand}pts in hand, %.0f{bust}%% bust, EV %+.1f{ev}   [h]it   [s]tand   [q]uit"
        |> centered width
        |> styled [ Ansi.Bright ]
        |> WriteFooter

        let rec read () = async {
            let! key = Async.AwaitEvent keys
            match key.Key with
            | ConsoleKey.H -> return Strategy.Hit
            | ConsoleKey.S -> return Strategy.Stand
            | ConsoleKey.Q -> return raise (QuitException())
            | ConsoleKey.Escape -> return raise (QuitException())
            | _ -> return! read ()
        }

        read ()

    let decider: Strategy.Decider =
        fun strategy round turn player others finished decks ->
            match strategy with
            | Prompt -> promptHuman player others decks
            | strategy -> Strategy.DecideWith random strategy round turn player others finished decks

    // The play loop is asynchronous end to end: pulling the timeline drives
    // the game, a prompt awaits its key without holding a thread, and every
    // instant is on disk (write-through) before it is rendered
    let timeline =
        Timeline.SimulateWithDecider random decider players None None None None
        |> Persistence.WriteTimelineLazy directory

    Console.Clear()

    let play = async {
        let enumerator = timeline.GetAsyncEnumerator()
        let mutable instants: Instant list = []
        let mutable rounds = 0
        let mutable finished = false

        try
            let mutable pulling = true

            while pulling do
                let! pulled = enumerator.MoveNextAsync().AsTask() |> Async.AwaitTask

                if not pulled then
                    pulling <- false
                    finished <- true
                else
                    let instant = enumerator.Current
                    instants <- instant :: instants
                    let displayRound = rounds + 1

                    if instant.Event.IsRoundEnded then
                        rounds <- rounds + 1

                    let footer =
                        "[q at your turn] quit" |> centered width |> styled [ Ansi.Dim; Ansi.Cyan ]

                    RenderFrame displayRound instant footer

                    do!
                        Async.Sleep(
                            if instant.Event.IsRoundEnded then
                                2 * paceMilliseconds
                            else
                                paceMilliseconds
                        )
        with error when IsQuit error ->
            ()

        do! enumerator.DisposeAsync().AsTask() |> Async.AwaitTask

        if finished && not (List.isEmpty instants) then
            let final = List.head instants
            let winner = final.Players |> List.maxBy (fun player -> player.FirmScore)

            $"game over: {winner.Name} wins with {winner.FirmScore}pts!   [any key]"
            |> centered width
            |> styled [ Ansi.BrightGreen ]
            |> WriteFooter

            do! Async.AwaitEvent keys |> Async.Ignore
    }

    Async.RunSynchronously play
