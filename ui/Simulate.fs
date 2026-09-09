module public Simulate

open System

open FSharp.Control

open Flip7

let public Run (players: (string * Strategy) list) (seed: int option) (pace: int option) : Async<unit> = async {
    let now = DateTime.Now.ToString "yyyy-MM-ddTHH-mm-ss"
    let directory = IO.Path.Join("timelines", now)
    let replayName = $"simulated game {now}"

    if players.Length <= 0 then
        raise (ArgumentException "Please provide at least one player name as a command-line argument.")
    if players.Length > 5 then
        raise (ArgumentException "Please provide no more than five player names as command-line arguments.")
    if players |> List.map fst |> List.distinct |> List.length <> players.Length then
        raise (ArgumentException "Player names must be unique.")

    let random =
        match seed with
        | Some value -> Random(value)
        | None -> Random()

    use cancellation = new Threading.CancellationTokenSource()

    let! producer =
        Timeline.SimulateWith random players
        |> Persistence.WriteTimelineLazy directory
        |> AsyncSeq.takeWhile (fun _ -> not cancellation.IsCancellationRequested)
        |> AsyncSeq.iter ignore
        |> Async.StartChild

    do! Replay.Run replayName directory pace
    cancellation.Cancel()
    do! producer
}
