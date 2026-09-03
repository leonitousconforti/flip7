module public Simulate

open System

open Flip7

let public Run (playerNamesAndStrategies: string list) : unit =
    System.Diagnostics.Debug.Assert(
        playerNamesAndStrategies.Length > 0,
        "Please provide at least one player name as a command-line argument."
    )
    System.Diagnostics.Debug.Assert(
        playerNamesAndStrategies.Length <= 5,
        "Please provide no more than five player names as command-line arguments."
    )
    System.Diagnostics.Debug.Assert(
        playerNamesAndStrategies
        |> List.forall (fun name -> not (String.IsNullOrWhiteSpace name)),
        "Player names cannot be empty or whitespace."
    )
    System.Diagnostics.Debug.Assert(
        playerNamesAndStrategies |> List.distinct |> List.length = playerNamesAndStrategies.Length,
        "Player names must be unique."
    )

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

    let now = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"
    let replayName = sprintf "simulated game %s" now

    let players = playerNamesAndStrategies |> List.map parse
    let seededHands = Some Map.empty
    let seededScores = Some Map.empty
    let seededDeck = None
    let seededDiscards = None

    Timeline.Simulate players seededHands seededScores seededDeck seededDiscards
    |> Seq.toArray
    |> Some
    |> Replay.Run replayName
