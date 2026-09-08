namespace Flip7

module public Persistence =
    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.IO

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

    let public ReadInstantAsync (directory: string) : Async<Result<Instant, FormatException>> = async {
        let toDirectory = fun file -> Path.Join(directory, file)
        let read = fun file -> File.ReadAllLinesAsync(toDirectory file) |> Async.AwaitTask

        let playerFiles =
            Directory.GetFiles(directory, "player*.txt")
            |> Array.sortBy (fun file -> int (Path.GetFileNameWithoutExtension file).[6..])

        let! readDeckTask = read "deck.txt" |> Async.StartChild
        let! readDiscardsTask = read "discards.txt" |> Async.StartChild
        let! readEventTask = read "event.txt" |> Async.StartChild

        try
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
        with :? FormatException as exn ->
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

    /// <summary>
    /// A view of a timeline directory as it fills: instants are published
    /// atomically (staged then renamed by the writer), so once {directory}/{index}
    /// exists its contents are complete and the store can tail the directory for
    /// growth. Only lightweight metadata is held in memory plus a small cache of
    /// recently viewed instants: memory stays flat no matter how long the timeline
    /// grows, and there is no channel to the producer at all - the disk is the
    /// only source.
    /// </summary>
    type public TimelineStore(directory: string, ?ingestDelayMilliseconds: int64, ?cacheCapacity: int) =
        let ingestDelayMilliseconds = defaultArg ingestDelayMilliseconds 100L
        let cacheCapacity = defaultArg cacheCapacity 64

        let ingestPacer = Stopwatch.StartNew()
        let roundEnds = ResizeArray<int>(16)
        let cache = Dictionary<int, LinkedListNode<int * Instant>>(cacheCapacity)
        let recency = LinkedList<int * Instant>()

        let mutable count: int = 0
        let mutable isComplete: bool = false

        member _.Count = count
        member _.RoundEnds = roundEnds

        member _self.Read(index: int) : Async<Result<Instant, FormatException>> = async {
            match cache.TryGetValue index with
            | true, node ->
                recency.Remove node
                recency.AddFirst node
                return node.Value |> snd |> Ok
            | _, _ ->
                let directory' = Path.Join(directory, string index)
                let! maybeInstant = ReadInstantAsync directory'

                match maybeInstant with
                | Error _ -> ()
                | Ok instant ->
                    cache[index] <- recency.AddFirst((index, instant))
                    if cache.Count > cacheCapacity then
                        cache.Remove(fst recency.Last.Value) |> ignore
                        recency.RemoveLast()

                return maybeInstant
        }

        member self.Ingest() : Async<unit> = async {
            if
                ingestPacer.ElapsedMilliseconds >= ingestDelayMilliseconds
                && Directory.Exists(Path.Join(directory, string count))
                && not isComplete
            then
                match! self.Read count with
                | Error _ -> ()
                | Ok instant ->
                    if instant.Event.IsRoundEnded then
                        roundEnds.Add count
                        if instant.Players |> List.exists (fun player -> player.FirmScore >= 200u) then
                            isComplete <- true

                count <- count + 1
                ingestPacer.Restart()
        }
