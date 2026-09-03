module public Simulate

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
