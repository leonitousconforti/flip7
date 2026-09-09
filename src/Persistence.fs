namespace Flip7

module public Persistence =
    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.IO
    open System.Threading
    open System.Threading.Tasks

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
                        FirmScore = lines[2] |> uint
                        Hand = lines |> Seq.skip 4 |> Hand.Deserialize
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
        | Snapshot of reply: AsyncReplyChannel<int * bool * int array>

    /// <summary>
    /// A view of a timeline directory as it fills: instants are published
    /// atomically (staged then renamed by the writer), so once
    /// {directory}/{index} exists its contents are complete and the store can
    /// tail the directory for growth. Built on a MailboxProcessor: a single
    /// agent owns all of the mutable state - the frontier, the round ends, and
    /// the LRU cache - and every operation arrives as a message, so nothing is
    /// locked because nothing is shared. Ingestion is folded into the same
    /// message loop, but the frontier read runs as its own task that the loop
    /// polls: a slow instant at the frontier never camps on the mailbox, so
    /// viewer reads take precedence over background tailing. Only lightweight
    /// metadata is held in memory plus a small cache of recently viewed
    /// instants: memory stays flat no matter how long the timeline grows, and
    /// there is no channel to the producer at all - the disk is the only
    /// source. Dispose stops the agent.
    /// </summary>
    type public TimelineStore(directory: string, ?ingestDelayMilliseconds: int64, ?cacheCapacity: int) =
        let ingestDelayMilliseconds = defaultArg ingestDelayMilliseconds 100L
        let cacheCapacity = defaultArg cacheCapacity 64

        let cts = new CancellationTokenSource()

        let agent =
            MailboxProcessor.Start(
                (fun (inbox: MailboxProcessor<TimelineStoreMessage>) -> async {
                    // Confined to the agent: only the loop below ever touches
                    // any of this, so no lock guards it
                    let recency = LinkedList<int * Instant>()
                    let cache = Dictionary<int, LinkedListNode<int * Instant>>(cacheCapacity)
                    let pacer = Stopwatch.StartNew()

                    // The frontier read runs as its own task so a slow instant
                    // never camps on the mailbox: scrubbing reads are served
                    // the moment they arrive, and the loop folds the frontier
                    // in when its read lands. The viewer can never request the
                    // frontier itself (a cursor stays below Count), so nothing
                    // ever waits on this task but the loop.
                    let mutable frontier: Task<Result<Instant, exn>> option = None

                    let mutable count = 0
                    let mutable isComplete = false
                    let mutable roundEnds: int array = ResizeArray(16).ToArray()

                    let read (index: int) : Async<Result<Instant, exn>> =
                        ReadInstantAsync(Path.Join(directory, string index))

                    let remember (index: int) (instant: Instant) : unit =
                        cache[index] <- recency.AddFirst((index, instant))
                        if cache.Count > cacheCapacity then
                            cache.Remove(fst recency.Last.Value) |> ignore
                            recency.RemoveLast()

                    let lookup (index: int) : Async<Result<Instant, exn>> = async {
                        match cache.TryGetValue index with
                        | true, node ->
                            recency.Remove node
                            recency.AddFirst node
                            return node.Value |> snd |> Ok
                        | _ ->
                            let! maybeInstant = read index
                            match maybeInstant with
                            | Error _ -> ()
                            | Ok instant -> remember index instant

                            return maybeInstant
                    }

                    while true do
                        let due = ingestDelayMilliseconds - pacer.ElapsedMilliseconds
                        let timeout = due |> int |> max 0

                        match! inbox.TryReceive timeout with
                        | Some(Snapshot channel) -> channel.Reply(count, isComplete, roundEnds)
                        | Some(Read(index, channel)) ->
                            let! instant = lookup index
                            channel.Reply instant
                        | None -> ()

                        match frontier with
                        | Some frontierTask when frontierTask.IsCompletedSuccessfully ->
                            frontier <- None

                            match frontierTask.Result with
                            | Error _ -> count <- count + 1
                            | Ok instant ->
                                remember count instant
                                if instant.Event.IsRoundEnded then
                                    roundEnds <- Array.append roundEnds [| count |]
                                    if instant.Players |> List.exists (fun player -> player.FirmScore >= 200u) then
                                        isComplete <- true

                                count <- count + 1
                        | _ -> ()

                        if pacer.ElapsedMilliseconds >= ingestDelayMilliseconds then
                            if
                                frontier.IsNone
                                && not isComplete
                                && Directory.Exists(Path.Join(directory, string count))
                            then
                                frontier <- Some(Async.StartAsTask(read count, cancellationToken = cts.Token))

                            pacer.Restart()
                }),
                cts.Token
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

        member _.Snapshot: Async<int * bool * int array> = agent.PostAndAsyncReply Snapshot
        member _.Read(index: int) : Async<Result<Instant, exn>> =
            agent.PostAndAsyncReply(fun channel -> Read(index, channel))

        interface IDisposable with
            member _.Dispose() =
                cts.Cancel()
                cts.Dispose()

        // The round number (starting at 1) of the instant at the cursor, given the
        // ascending indices of the RoundEnded instants seen so far. A RoundEnded
        // instant belongs to the round it closes.
        static member inline public RoundOf (roundEnds: int array) (cursor: int) : int =
            1 + (roundEnds |> Array.filter (fun index -> index < cursor) |> Array.length)

        // The index of the nearest RoundEnded instant strictly after the cursor, or
        // the newest known index when that round has not ended yet
        static member inline public NextRoundEnded (roundEnds: int array) (newest: int) (cursor: int) : int =
            roundEnds
            |> Array.tryFind (fun index -> index > cursor)
            |> Option.defaultValue newest

        // The index of the nearest RoundEnded instant strictly before the cursor, or
        // the start of the timeline when there is none
        static member inline public PrevRoundEnded (roundEnds: int array) (cursor: int) : int =
            roundEnds
            |> Array.tryFindBack (fun index -> index < cursor)
            |> Option.defaultValue 0
