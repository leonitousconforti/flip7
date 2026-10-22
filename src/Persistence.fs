namespace Flip7

module public Persistence =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Threading

    open FSharp.Control

    let public WriteInstantAsync (directory: string) (instant: Instant) : Async<Instant> = async {
        let path = Directory.CreateDirectory directory
        let toDirectory = fun file -> Path.Join(path.FullName, file)
        let write (file: string) (lines: string seq) : Async<unit> =
            File.WriteAllLinesAsync(toDirectory file, lines) |> Async.AwaitTask

        let! writeDeckTask =
            instant.Deck |> Deck.Serialize |> write "deck.txt" |> Async.StartChild
        let! writeDiscardsTask =
            instant.Discards |> Deck.Serialize |> write "discards.txt" |> Async.StartChild
        let! writeEventTask =
            instant.Event |> Event.Serialize |> write "event.txt" |> Async.StartChild

        let! writePlayerTasks =
            instant.Players
            |> List.indexed
            |> List.map (fun (index, player) ->
                write $"player{index}.txt" [|
                    string player.Name
                    string player.Strategy
                    string player.Targeting
                    string player.FirmScore
                    String.Empty
                    yield! Hand.Serialize player.Hand
                |]
            )
            |> Async.Parallel
            |> Async.Ignore
            |> Async.StartChild

        do! writeDeckTask
        do! writeDiscardsTask
        do! writeEventTask
        do! writePlayerTasks

        return instant
    }

    let public ReadInstantAsync (directory: string) : Async<Result<Instant, exn>> = async {
        try
            let toDirectory = fun file -> Path.Join(directory, file)
            let read = fun file -> File.ReadAllLinesAsync(toDirectory file) |> Async.AwaitTask

            let playerFiles =
                Directory.GetFiles(directory, "player*.txt")
                |> Array.sortBy (fun file -> int (Path.GetFileNameWithoutExtension file).[6..])

            let! readDeckTask = read "deck.txt" |> Async.StartChild
            let! readDiscardsTask = read "discards.txt" |> Async.StartChild
            let! readEventTask = read "event.txt" |> Async.StartChild

            let! readPlayerTasks =
                playerFiles
                |> Array.map (fun file -> async {
                    let! lines = File.ReadAllLinesAsync file |> Async.AwaitTask
                    return {
                        Name = lines[0]
                        Strategy = lines[1] |> Strategy.Parse
                        Targeting = lines[2] |> Targeting.Parse
                        FirmScore = lines[3] |> uint
                        Hand = lines |> Seq.skip 5 |> Hand.Deserialize
                    }
                })
                |> Async.Parallel
                |> Async.StartChild

            let! deck = readDeckTask
            let! discards = readDiscardsTask
            let! event = readEventTask
            let! players = readPlayerTasks

            return
                Ok {
                    Event = event |> Event.Deserialize
                    Players = players |> List.ofArray
                    Deck = deck |> Deck.Deserialize
                    Discards = discards |> Deck.Deserialize
                }
        with exn ->
            return Error exn
    }

    let public WriteTimelineLazyFrom (directory: string) (startIndex: int) (timeline: Timeline) : Timeline = asyncSeq {
        let stagingDirectory = Path.Join(directory, ".staging")
        let mutable index = startIndex

        if Directory.Exists stagingDirectory then
            Directory.Delete(stagingDirectory, true)

        for instant in timeline do
            let instantDirectory = Path.Join(directory, string index)
            let! written = WriteInstantAsync stagingDirectory instant
            Directory.Move(stagingDirectory, instantDirectory)
            index <- index + 1
            yield written
    }

    let public WriteTimelineLazy (directory: string) (timeline: Timeline) : Timeline =
        WriteTimelineLazyFrom directory 0 timeline

    let public WriteTimelineEager (directory: string) (timeline: Timeline) : Timeline =
        // The cache lives outside the sequence so re-enumeration replays it
        // rather than re-pulling the source and re-writing the files
        let written = timeline |> WriteTimelineLazy directory |> AsyncSeq.cache

        asyncSeq {
            do! written |> AsyncSeq.iter ignore
            yield! written
        }

    let public ReadTimeline (directory: string) : Timeline = asyncSeq {
        let instantDirectories =
            Directory.GetDirectories directory
            |> Array.choose (fun dir ->
                match Int32.TryParse(Path.GetFileName dir) with
                | true, index -> Some(index, dir)
                | false, _ -> None
            )
            |> Array.sortBy fst

        for _, instantDirectory in instantDirectories do
            let! maybeInstant = ReadInstantAsync instantDirectory
            yield maybeInstant |> Result.defaultWith (fun exn -> raise exn)
    }

    type private TimelineStoreMessage =
        | Read of index: int * reply: AsyncReplyChannel<Result<Instant, exn>>
        | Snapshot of reply: AsyncReplyChannel<int * bool * int list>
        // The ingest delay is up: look for the frontier on disk
        | Probe
        // The frontier read landed
        | Ingest of Result<Instant, exn>

    /// <summary>
    /// A view of a timeline directory as it fills: instants are published
    /// atomically (staged then renamed by the writer), so once
    /// {directory}/{index} exists its contents are complete and the store can
    /// tail the directory for growth. Built on a MailboxProcessor: a single
    /// agent owns all of the mutable state - the frontier, the seating, the
    /// round ends, and the LRU cache - and every operation arrives as a
    /// message, so nothing is locked because nothing is shared. Ingestion is
    /// folded into the same message loop: the agent looks for the frontier once
    /// per ingest delay, and the frontier read runs on its own and posts what
    /// it read back as a message, so a slow instant at the frontier never camps
    /// on the mailbox and viewer reads take precedence over background tailing.
    /// Once the game is complete the agent stops looking, and only the viewer
    /// wakes it. Only lightweight metadata is held in memory plus a small cache
    /// of recently viewed instants: memory stays flat no matter how long the
    /// timeline grows, and there is no channel to the producer at all - the
    /// disk is the only source. With emitPlayersInSeatedOrder (off by default),
    /// every emitted instant lists its players in the seating order stored by
    /// the TableSeated instant that opens the timeline, rather than the stored
    /// rotation order. Dispose stops the agent.
    /// </summary>
    type public TimelineStore
        (directory: string, ?ingestDelayMilliseconds: int64, ?cacheCapacity: int, ?emitPlayersInSeatedOrder: bool)
        =
        let ingestDelayMilliseconds = defaultArg ingestDelayMilliseconds 100L
        let cacheCapacity = defaultArg cacheCapacity 64
        let emitPlayersInSeatedOrder = defaultArg emitPlayersInSeatedOrder false

        let cts = new CancellationTokenSource()
        // Taken once: the agent may still start work with it after Dispose has
        // disposed of the source
        let token = cts.Token

        let agent =
            MailboxProcessor.Start(
                (fun (inbox: MailboxProcessor<TimelineStoreMessage>) -> async {
                    // Confined to the agent: only the loop below ever touches
                    // any of this, so no lock guards it. Each cached instant
                    // carries the use that last touched it, so the least
                    // recently used is the one with the oldest
                    let cache = Dictionary<int, Instant * int64>(cacheCapacity + 1)
                    let mutable uses = 0L

                    let mutable count = 0
                    let mutable isComplete = false
                    let mutable roundEnds: int list = []
                    let mutable seating: string list option = None

                    let seatPlayers (instant: Instant) : Instant =
                        match seating with
                        | None -> instant
                        | Some seating -> {
                            instant with
                                Players =
                                    seating
                                    |> List.choose (fun name ->
                                        instant.Players |> List.tryFind (fun player -> player.Name = name)
                                    )
                          }

                    let read (index: int) : Async<Result<Instant, exn>> =
                        ReadInstantAsync(Path.Combine(directory, string index))

                    // An index read before it was ingested is remembered again
                    // when it is, which only marks it used again: it is never
                    // cached twice, so it never holds the cache past its
                    // capacity
                    let remember (index: int) (instant: Instant) : unit =
                        uses <- uses + 1L
                        cache[index] <- (instant, uses)

                        if cache.Count > cacheCapacity then
                            let oldest = cache |> Seq.minBy (fun entry -> snd entry.Value)
                            cache.Remove oldest.Key |> ignore

                    let lookup (index: int) : Async<Result<Instant, exn>> = async {
                        match cache.TryGetValue index with
                        | true, (instant, _) ->
                            remember index instant
                            return Ok instant
                        | _ ->
                            let! maybeInstant = read index
                            match maybeInstant with
                            | Error _ -> ()
                            | Ok instant -> remember index instant

                            return maybeInstant
                    }

                    // The loop waits on the mailbox alone, so the pace arrives
                    // as a message: after the ingest delay, or straight away
                    // without one, which keeps the agent looking as fast as
                    // the loop turns until the frontier appears
                    let probeLater () : unit =
                        if ingestDelayMilliseconds > 0L then
                            Async.Start(
                                async {
                                    do! Async.Sleep(int (min ingestDelayMilliseconds (int64 Int32.MaxValue)))
                                    inbox.Post Probe
                                },
                                token
                            )
                        else
                            inbox.Post Probe

                    // Until the game is complete either a Probe is on its way
                    // or the frontier read is out, never both: each instant is
                    // read once, and once the game is over only the viewer
                    // wakes the agent
                    probeLater ()

                    while true do
                        match! inbox.Receive() with
                        | Snapshot channel -> channel.Reply(count, isComplete, roundEnds)
                        | Read(index, channel) ->
                            let! instant = lookup index
                            instant |> Result.map seatPlayers |> channel.Reply
                        | Probe ->
                            // The frontier read runs on its own so a slow
                            // instant never camps on the mailbox: scrubbing
                            // reads are served the moment they arrive. The
                            // viewer can never request the frontier itself (a
                            // cursor stays below Count), so nothing waits on
                            // this read but the loop.
                            if Directory.Exists(Path.Combine(directory, string count)) then
                                let frontier = read count

                                Async.Start(
                                    async {
                                        let! instant = frontier
                                        inbox.Post(Ingest instant)
                                    },
                                    token
                                )
                            else
                                probeLater ()
                        | Ingest frontier ->
                            match frontier with
                            | Error _ -> ()
                            | Ok instant ->
                                remember count instant

                                if count = 0 && emitPlayersInSeatedOrder then
                                    seating <- Some(instant.Players |> List.map (fun player -> player.Name))

                                if instant.Event.IsRoundEnded then
                                    roundEnds <- roundEnds @ [ count ]
                                    if instant.Players |> List.exists (fun player -> player.FirmScore >= 200u) then
                                        isComplete <- true

                            count <- count + 1

                            if not isComplete then
                                probeLater ()
                }),
                token
            )

        member _.Count = async {
            let! count, _, _ = agent.PostAndAsyncReply Snapshot
            return count
        }

        member _.IsComplete = async {
            let! _, isComplete, _ = agent.PostAndAsyncReply Snapshot
            return isComplete
        }

        member _.RoundEnds = async {
            let! _, _, roundEnds = agent.PostAndAsyncReply Snapshot
            return roundEnds
        }

        member _.Snapshot: Async<int * bool * int list> = agent.PostAndAsyncReply Snapshot
        member _.Read(index: int) : Async<Result<Instant, exn>> =
            agent.PostAndAsyncReply(fun channel -> Read(index, channel))

        interface IDisposable with
            member _.Dispose() =
                cts.Cancel()
                cts.Dispose()

        // The round number (starting at 1) of the instant at the cursor, given
        // the ascending indices of the RoundEnded instants seen so far. A
        // RoundEnded instant belongs to the round it closes.
        static member inline public RoundOf (roundEnds: int list) (cursor: int) : int =
            1 + (roundEnds |> List.filter (fun index -> index < cursor) |> List.length)

        // The index of the nearest RoundEnded instant strictly after the
        // cursor, or the newest known index when that round has not ended yet
        static member inline public NextRoundEnded (roundEnds: int list) (newest: int) (cursor: int) : int =
            roundEnds
            |> List.tryFind (fun index -> index > cursor)
            |> Option.defaultValue newest

        // The index of the nearest RoundEnded instant strictly before the
        // cursor, or the start of the timeline when there is none
        static member inline public PrevRoundEnded (roundEnds: int list) (cursor: int) : int =
            roundEnds
            |> List.tryFindBack (fun index -> index < cursor)
            |> Option.defaultValue 0
