module PersistenceTests

open System
open System.IO
open FSharp.Control
open Xunit
open Flip7

let private inTempDirectory (test: string -> unit) : unit =
    let directory = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString())

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

        Persistence.WriteInstantAsync directory instant
        |> Async.RunSynchronously
        |> ignore

        Assert.Equal(instant, Persistence.ReadInstantAsync directory |> Async.RunSynchronously)
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
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously

        Persistence.WriteTimelineEager directory (AsyncSeq.ofSeq original) |> ignore

        let readBack =
            Persistence.ReadTimeline directory
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously

        Assert.Equal<Instant list>(original, readBack)
    )
