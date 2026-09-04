module public Simulate

open System

open FSharp.Control

open Flip7

let public Run (playerNamesAndStrategies: string list) : unit =
    let parse =
        fun (nameAndStrategy: string) ->
            let parts = nameAndStrategy.Split ","
            let name = parts[0]
            let strategy =
                if parts.Length > 1 then
                    Strategy.Parse parts[1]
                else
                    Strategy.Random

            name, strategy

    let now = DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss"
    let directory = IO.Path.Join("timelines", now)
    let replayName = $"simulated game {now}"

    let players = playerNamesAndStrategies |> List.map parse
    let seededHands = Some Map.empty
    let seededScores = Some Map.empty
    let seededDeck = None
    let seededDiscards = None

    if players.Length <= 0 then
        raise (ArgumentException "Please provide at least one player name as a command-line argument.")
    if players.Length > 5 then
        raise (ArgumentException "Please provide no more than five player names as command-line arguments.")
    if players |> List.map fst |> List.distinct |> List.length <> players.Length then
        raise (ArgumentException "Player names must be unique.")

    use cancellation = new Threading.CancellationTokenSource()
    let producer =
        Timeline.Simulate players seededHands seededScores seededDeck seededDiscards
        |> Persistence.WriteTimelineLazy directory
        |> AsyncSeq.takeWhile (fun _ -> not cancellation.IsCancellationRequested)
        |> AsyncSeq.iter ignore
        |> Async.StartAsTask

    Replay.Run replayName directory
    cancellation.Cancel()
    producer.Wait()
