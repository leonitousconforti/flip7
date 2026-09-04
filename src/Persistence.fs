namespace Flip7

module public Persistence =
    open System.IO
    open System

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
                    $"{player.Name}"
                    $"{player.Strategy}"
                    $"{player.FirmScore}"
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

    let public ReadInstantAsync (directory: string) : Async<Instant> = async {
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

        return {
            Event = event |> Event.Deserialize
            Players = players |> List.ofSeq
            Deck = deck |> Deck.Deserialize
            Discards = discards |> Deck.Deserialize
        }
    }

    let public WriteTimelineLazy (directory: string) (timeline: Timeline) : Timeline = asyncSeq {
        let stagingDirectory = Path.Join(directory, ".staging")
        let mutable index = 0

        if Directory.Exists stagingDirectory then
            Directory.Delete(stagingDirectory, true)

        for instant in timeline do
            let instantDirectory = Path.Join(directory, $"{index}")
            let! written = WriteInstantAsync stagingDirectory instant
            Directory.Move(stagingDirectory, instantDirectory)
            index <- index + 1
            yield written
    }

    let public WriteTimelineEager (directory: string) (timeline: Timeline) : Timeline =
        let written = timeline |> WriteTimelineLazy directory |> AsyncSeq.cache
        written |> AsyncSeq.iter ignore |> Async.RunSynchronously |> ignore
        written

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
            let! instant = ReadInstantAsync instantDirectory
            yield instant
    }
