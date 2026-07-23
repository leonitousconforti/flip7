namespace Flip7

module public Persistence =
    open System.IO
    open System

    let public WriteInstant
        (directory: string)
        (instant: Player * Player list * (Deck * Deck))
        : Player * Player list * (Deck * Deck) =
        let player, otherPlayers, (deck, discards) = instant

        let path = Directory.CreateDirectory directory
        let toDirectory = fun file -> Path.Combine(path.FullName, file)
        let write = fun file lines -> File.WriteAllLines(toDirectory file, lines)

        deck |> Deck.Serialize |> write "deck.txt"
        discards |> Deck.Serialize |> write "discards.txt"

        player :: otherPlayers
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

    let public ReadInstant (directory: string) : Player * Player list * (Deck * Deck) =
        let toDirectory = fun file -> Path.Combine(directory, file)
        let read = fun file -> File.ReadLines(toDirectory file)

        let deck = read "deck.txt" |> Deck.Deserialize
        let discards = read "discards.txt" |> Deck.Deserialize

        let playerFiles =
            Directory.GetFiles(directory, "player*.txt")
            |> Array.sortBy (fun file -> int (Path.GetFileNameWithoutExtension file).[6..])

        let players =
            playerFiles
            |> Array.map (fun file ->
                let lines = read file |> Seq.cache
                let name = lines |> Seq.take 1 |> Seq.head
                let strategy = lines |> Seq.skip 1 |> Seq.take 1 |> Seq.head |> Strategy.Parse
                let firmScore = lines |> Seq.skip 2 |> Seq.take 1 |> Seq.head |> uint
                let hand = lines |> Seq.skip 4 |> Hand.Deserialize

                {
                    Name = name
                    Strategy = strategy
                    FirmScore = firmScore
                    Hand = hand
                }
            )
            |> Array.toList

        match players with
        | player :: otherPlayers -> player, otherPlayers, (deck, discards)
        | [] -> failwith "No players found in the instant"

    let public WriteTimelineLazy (timeline: Timeline) : Timeline =
        let identifier = DateTime.UtcNow.ToString "s"
        let timelineDirectory = Path.Combine("timelines", identifier)

        timeline
        |> Seq.mapi (fun index instant ->
            let instantDirectory = Path.Combine(timelineDirectory, $"{index}")
            WriteInstant instantDirectory instant
        )

    let public WriteTimelineEager (timeline: Timeline) : Timeline =
        let cachedTimeline = timeline |> Seq.cache
        cachedTimeline |> WriteTimelineLazy |> Seq.toArray |> ignore
        cachedTimeline

    let public ReadTimeline (directory: string) : Timeline =
        let instantDirectories =
            Directory.GetDirectories directory
            |> Array.sortBy (fun dir -> int (Path.GetFileName dir))

        instantDirectories |> Array.toSeq |> Seq.map ReadInstant
