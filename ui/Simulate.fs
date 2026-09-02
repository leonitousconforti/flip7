module public Simulate

open Flip7

let public Run (names: string list) : unit =
    let strategies = [
        Strategy.Random
        HitUntilScore 25u
        HitUntilNumCards 4u
        RandomWithProbability 0.75
        AlwaysHits
    ]

    let players =
        names
        |> List.mapi (fun index name -> name, strategies[index % strategies.Length])

    let now = System.DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss"
    let replayName = sprintf "simulated game %s" now

    Timeline.Simulate players None None None None
    |> Seq.toArray
    |> Some
    |> Replay.Run replayName
