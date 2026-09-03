module PersistenceTests

open System
open System.IO
open Xunit
open Flip7

let private inTempDirectory (test: string -> unit) : unit =
    let directory = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())

    try
        test directory
    finally
        if Directory.Exists directory then
            Directory.Delete(directory, true)

[<Fact>]
let ``WriteInstant and ReadInstant round-trip`` () =
    inTempDirectory (fun directory ->
        let instant = {
            Event = Drew("Alice", ValueCard Card.Seven)
            Players = [
                Player.Make("Alice", HitUntilScore 45u, 100u, [ ValueCard Card.Seven; ModifierCard Card.Double ])
                Player.Make("Bob", RandomWithProbability 0.25, 55u, [])
                Player.Make("Carol", AlwaysStands, 0u, [ ActionCard Card.SecondChance ])
            ]
            Deck = Deck.Full |> Map.add (ValueCard Card.Seven) 3u
            Discards = Deck.Empty |> Map.add (ValueCard Card.Twelve) 2u
        }

        Persistence.WriteInstant directory instant |> ignore
        Assert.Equal(instant, Persistence.ReadInstant directory)
    )

[<Fact>]
let ``a written timeline reads back identically`` () =
    inTempDirectory (fun directory ->
        let original =
            Timeline.SimulateWith
                (System.Random 42)
                [ "Alice", Strategy.Random; "Bob", HitUntilScore 25u ]
                None
                None
                None
                None
            |> Seq.toList

        Persistence.WriteTimelineEager directory original |> ignore

        Assert.Equal<Instant list>(original, Persistence.ReadTimeline directory |> Seq.toList)
    )
