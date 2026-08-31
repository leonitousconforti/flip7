namespace Flip7

module public Persistence =
    open System.IO
    open System

    let public WriteInstant (directory: string) (instant: Instant) : Instant =
        let path = Directory.CreateDirectory directory
        let toDirectory = fun file -> Path.Combine(path.FullName, file)
        let write = fun file lines -> File.WriteAllLines(toDirectory file, lines)

        instant.Deck |> Deck.Serialize |> write "deck.txt"
        instant.Discards |> Deck.Serialize |> write "discards.txt"
        instant.Event |> Event.Serialize |> write "event.txt"

        instant.Players
        |> List.iteri (fun index player ->
            write $"player{index}.txt" [|
                $"{player.Name}"
                $"{player.Strategy}"
                $"{player.FirmScore}"
                String.Empty
                yield! Hand.Serialize player.Hand
            |]
        )

        instant

    let public ReadInstant (directory: string) : Instant =
        let toDirectory = fun file -> Path.Combine(directory, file)
        let read = fun file -> File.ReadLines(toDirectory file)

        let deck = read "deck.txt" |> Deck.Deserialize
        let discards = read "discards.txt" |> Deck.Deserialize
        let event = read "event.txt" |> Event.Deserialize

        let playerFiles =
            Directory.GetFiles(directory, "player*.txt")
            |> Array.sortBy (fun file -> int (Path.GetFileNameWithoutExtension file).[6..])

        let players =
            playerFiles
            |> Array.map (fun file ->
                let lines = File.ReadLines file |> Seq.cache
                let name = lines |> Seq.item 0
                let strategy = lines |> Seq.item 1 |> Strategy.Parse
                let firmScore = lines |> Seq.item 2 |> uint
                let hand = lines |> Seq.skip 4 |> Hand.Deserialize

                {
                    Name = name
                    Strategy = strategy
                    FirmScore = firmScore
                    Hand = hand
                }
            )
            |> Array.toList

        {
            Event = event
            Players = players
            Deck = deck
            Discards = discards
        }

    let public WriteTimelineLazy (timeline: Timeline) : Timeline =
        let identifier = DateTime.UtcNow.ToString "s"
        let timelineDirectory = Path.Combine("timelines", identifier)

        timeline
        |> Seq.mapi (fun index instant ->
            let instantDirectory = Path.Combine(timelineDirectory, $"{index}")
            WriteInstant instantDirectory instant
        )

    let public WriteTimelineEager (timeline: Timeline) : Timeline =
        let written = timeline |> WriteTimelineLazy |> Seq.cache
        written |> Seq.iter ignore
        written

    let public ReadTimeline (directory: string) : Timeline =
        let instantDirectories =
            Directory.GetDirectories directory
            |> Array.sortBy (fun dir -> int (Path.GetFileName dir))

        instantDirectories |> Array.toSeq |> Seq.map ReadInstant
