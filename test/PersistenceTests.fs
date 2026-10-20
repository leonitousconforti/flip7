module PersistenceTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open FSharp.Control
open Xunit
open Flip7
open Support

let private inTempDirectory (test: string -> unit) : unit =
    let directory = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString())

    try
        test directory
    finally
        if Directory.Exists directory then
            Directory.Delete(directory, true)

// Drains a writer over the instants, as the terminal app drains a game to
// disk, so that every instant is on disk once it returns
let private writeAll (writer: Timeline -> Timeline) (instants: Instant list) : unit =
    instants
    |> AsyncSeq.ofSeq
    |> writer
    |> AsyncSeq.iter ignore
    |> Async.RunSynchronously

// Everything a reader finds in the directory, in order
let private readBack (directory: string) : Instant list =
    Persistence.ReadTimeline directory |> played

// The indices of the instant directories written so far, ascending
let private instantDirectories (directory: string) : int list =
    Directory.GetDirectories directory
    |> Seq.choose (fun path ->
        match Int32.TryParse(Path.GetFileName path) with
        | true, index -> Some index
        | false, _ -> None
    )
    |> Seq.sort
    |> List.ofSeq

// Two seats play a game short enough to write to disk in a moment, but long
// enough to rotate the seats and span several rounds
let private twoSeats: Lineup = [
    "Alice", Strategy.Random, Targeting.ChoosesRandomly
    "Bob", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
]

let private gameOf (seed: int) : Timeline =
    Timeline.SimulateWith (Random seed) twoSeats

// ---- instants ----

[<Fact>]
let ``an instant written to a directory reads back identically`` () =
    inTempDirectory (fun directory ->
        // Each field the files hold, with strategies and targetings that take
        // arguments and that do not, a targeting that takes sides twice over
        // names written with escapes, a hand of each kind of card, an empty
        // hand, a firm score of zero and two partly filled decks
        let instant = {
            Event = Drew("Alice", ValueCard Card.Seven)
            Players = [
                Player.Make(
                    "Alice",
                    Strategy.HitUntilScore 45u,
                    100u,
                    [ ValueCard Card.Seven; ModifierCard Card.Double ]
                )
                Player.Make(
                    "Bob",
                    Strategy.RandomWithProbability 0.25,
                    55u,
                    [],
                    Targeting.TakesSides(
                        Some "Ann \"Bob\" \\ for",
                        Some "",
                        Targeting.TakesSides(None, Some "Carol", Targeting.PlaysSpitefully)
                    )
                )
                Player.Make(
                    "Carol",
                    Strategy.AlwaysStands,
                    0u,
                    [ ActionCard Card.SecondChance ],
                    Targeting.ChoosesExternally "Sage"
                )
            ]
            Deck = Deck.Full |> Map.add (ValueCard Card.Seven) 3u
            Discards = deckOf [ ValueCard Card.Twelve; ValueCard Card.Twelve ]
        }

        let written =
            Persistence.WriteInstantAsync directory instant |> Async.RunSynchronously

        Assert.Equal(instant, written)
        Assert.Equal(Ok instant, Persistence.ReadInstantAsync directory |> Async.RunSynchronously)
    )

// ---- timelines ----

[<Fact>]
let ``an eager write has every instant on disk before it yields the first, and enumerating it again writes nothing more``
    ()
    =
    inTempDirectory (fun directory ->
        // The whole game rather than a few instants, because this read-back is
        // where the hands of a real game are shown to survive the files
        let game = gameOf 42 |> played

        // How many instants the writer has taken from the game
        let pulls = ref 0

        let source = asyncSeq {
            for instant in game do
                pulls.Value <- pulls.Value + 1
                yield instant
        }

        let eager = Persistence.WriteTimelineEager directory source

        let yielded, onDiskAtEachYield =
            eager
            |> AsyncSeq.map (fun instant -> instant, (instantDirectories directory).Length)
            |> played
            |> List.unzip

        let yieldedAgain = played eager

        Assert.Equal<int list>(List.replicate game.Length game.Length, onDiskAtEachYield)
        Assert.Equal<Instant list>(game, yielded)
        Assert.Equal<Instant list>(game, yieldedAgain)
        // Each instant is written as it is pulled, so a second pass that
        // pulled the game again would write it all again
        Assert.Equal(game.Length, pulls.Value)
        Assert.Equal<Instant list>(game, readBack directory)
    )

[<Fact>]
let ``a continuation appends to a persisted timeline, and a write that crashed is not read back`` () =
    inTempDirectory (fun directory ->
        // Ten instants put the continuation in directory 10, which sorts
        // between 1 and 2 by name, so reading it back in order means reading
        // the directories in numeric order
        let first = gameOf 7 |> AsyncSeq.truncate 10 |> played
        writeAll (Persistence.WriteTimelineLazy directory) first

        // An edit fork appends behind the instants already on disk
        let edited = { List.last first with Event = Edited "Alice" }
        writeAll (Persistence.WriteTimelineLazyFrom directory first.Length) [ edited ]

        // A writer stages each instant in .staging and renames it once it is
        // whole, so a crash part way through leaves a half-written instant there
        let staging = Directory.CreateDirectory(Path.Join(directory, ".staging"))
        File.WriteAllText(Path.Join(staging.FullName, "player5.txt"), "Alice")

        Assert.Equal<int list>([ 0..10 ], instantDirectories directory)
        Assert.Equal<Instant list>(first @ [ edited ], readBack directory)
    )

// ---- TimelineStore ----

// How long a store may take to answer, or to catch up with the disk, before
// the test gives up on it: an agent that died or stopped ingesting would
// otherwise hang the test rather than fail it
let private patience = TimeSpan.FromSeconds 5.0

let private answered (question: Async<'T>) : 'T =
    Async.RunSynchronously(question, int patience.TotalMilliseconds)

// Asks the store for its count, completion and round ends every millisecond
// until they satisfy isReady
let private awaitSnapshot
    (store: Persistence.TimelineStore)
    (isReady: int * bool * int list -> bool)
    : int * bool * int list =
    let clock = Stopwatch.StartNew()

    let rec poll () =
        let snapshot = answered store.Snapshot

        if isReady snapshot then
            snapshot
        elif clock.Elapsed > patience then
            failwith $"the store stopped at %A{snapshot}"
        else
            Thread.Sleep 1
            poll ()

    poll ()

[<Fact>]
let ``a store over a written game knows every instant, where each round ends, and that the game is over`` () =
    inTempDirectory (fun directory ->
        let game = gameOf 42 |> played
        writeAll (Persistence.WriteTimelineLazy directory) game

        // A cache of two instants is small enough that the reads below miss
        // it, evict from it and hit it in turn
        use store =
            new Persistence.TimelineStore(
                directory,
                ingestDelayMilliseconds = 0L,
                cacheCapacity = 2,
                emitPlayersInSeatedOrder = true
            )

        let count, _, roundEnds = awaitSnapshot store (fun (_, isComplete, _) -> isComplete)

        let roundEndedAt =
            game
            |> List.indexed
            |> List.filter (fun (_, instant) -> instant.Event.IsRoundEnded)
            |> List.map fst

        Assert.Equal(game.Length, count)
        Assert.Equal<int list>(roundEndedAt, roundEnds)

        // An instant is stored with the next player to act first, and the
        // store hands it out with everyone in the order the TableSeated
        // instant seated them instead
        let seating = game.Head.Players |> List.map _.Name

        let seated (instant: Instant) : Instant = {
            instant with
                Players =
                    instant.Players
                    |> List.sortBy (fun player -> List.findIndex ((=) player.Name) seating)
        }

        // Ingestion leaves the last two instants in the cache. With room for
        // two, the second read of 5 comes straight after 1 and is served from
        // the cache, while 0, 30 and the newest instant have been evicted by
        // the time each is read, so they come from disk
        let reads = [ 0; 5; 1; 5; 30; 0; game.Length - 1 ]

        Assert.True(
            reads |> List.exists (fun index -> game[index] <> seated game[index]),
            "every instant read was stored in seated order, so the seating is never applied: pick other indices"
        )

        Assert.Equal<Result<Instant, exn> list>(
            reads |> List.map (fun index -> Ok(seated game[index])),
            reads |> List.map (store.Read >> answered)
        )

        // The newest instant is at Length - 1, so there is no directory at Length
        match answered (store.Read game.Length) with
        | Ok instant -> failwith $"read {instant.Event} past the end of the game"
        | Error error ->
            Assert.IsType<DirectoryNotFoundException>(error) |> ignore
            Assert.Contains(Path.Join(directory, string game.Length), error.Message)

        // A read that fails must not take the agent down with it, or every
        // later question would go unanswered
        Assert.Equal((count, true, roundEnds), answered store.Snapshot)
    )

[<Fact>]
let ``a store tails a timeline that is still being written`` () =
    inTempDirectory (fun directory ->
        let game = gameOf 42 |> played

        // As in the terminal app, the store opens before the writer has made
        // the directory, and the writer goes one instant at a time
        use store = new Persistence.TimelineStore(directory, ingestDelayMilliseconds = 0L)

        let afterEachWrite =
            Persistence.WriteTimelineLazy directory (AsyncSeq.ofSeq game)
            |> AsyncSeq.mapi (fun index _ ->
                let count, isComplete, _ =
                    awaitSnapshot store (fun (count, _, _) -> int64 count > index)
                count, isComplete
            )
            |> played

        // The game is over only at its last instant, the RoundEnded that ends
        // it, and every RoundEnded before that one leaves the game open
        let expected = [ for count in 1 .. game.Length -> count, (count = game.Length) ]

        Assert.Equal<(int * bool) list>(expected, afterEachWrite)
    )

[<Fact>]
let ``a store that reads an instant before ingesting it still caches the instants it used most recently`` () =
    inTempDirectory (fun directory ->
        let game = gameOf 42 |> AsyncSeq.truncate 3 |> played

        // Instant 0 is written last, so the store cannot ingest anything until
        // it has read instant 1 from disk, and ingesting then remembers
        // instant 1 a second time
        writeAll (Persistence.WriteTimelineLazyFrom directory 1) game.Tail

        use store =
            new Persistence.TimelineStore(directory, ingestDelayMilliseconds = 0L, cacheCapacity = 2)

        Assert.Equal(Ok game[1], answered (store.Read 1))

        writeAll (Persistence.WriteTimelineLazyFrom directory 0) [ game.Head ]
        awaitSnapshot store (fun (count, _, _) -> count = game.Length) |> ignore

        // With the instants gone from disk, only the cache can answer.
        // Ingesting 1 again made it more recently used than 0, so ingesting 2
        // evicted 0 rather than 1
        for index in 0..2 do
            Directory.Delete(Path.Join(directory, string index), true)

        Assert.Equal<Result<Instant, exn> list>(
            [ Ok game[1]; Ok game[2] ],
            [ answered (store.Read 1); answered (store.Read 2) ]
        )

        match answered (store.Read 0) with
        | Ok instant -> failwith $"read {instant.Event} from a cache that should have evicted it"
        | Error error -> Assert.IsType<DirectoryNotFoundException>(error) |> ignore
    )

[<Fact>]
let ``round navigation counts a RoundEnded in the round it closes and moves strictly past the cursor`` () =
    // Rounds end at instants 4 and 9, and 12 is the newest instant known
    let roundEnds = [ 4; 9 ]
    let newest = 12

    // A RoundEnded is the last instant of its round, and the next round starts
    // on the instant after it
    Assert.Equal<int list>(
        [ 1; 1; 2; 2; 3 ],
        [ 0; 4; 5; 9; 10 ]
        |> List.map (fun cursor -> Persistence.TimelineStore.RoundOf roundEnds cursor)
    )

    // Forward from a RoundEnded goes on to the next one rather than staying
    // put, and past the last one it stops at the newest instant
    Assert.Equal<int list>(
        [ 4; 9; 12; 12 ],
        [ 0; 4; 9; 11 ]
        |> List.map (fun cursor -> Persistence.TimelineStore.NextRoundEnded roundEnds newest cursor)
    )

    // Back from a RoundEnded goes on to the one before it, and before the
    // first one it stops at the start of the timeline
    Assert.Equal<int list>(
        [ 9; 4; 0; 0 ],
        [ 10; 9; 4; 2 ]
        |> List.map (fun cursor -> Persistence.TimelineStore.PrevRoundEnded roundEnds cursor)
    )
