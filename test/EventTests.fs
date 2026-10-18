module EventTests

open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// ---- Serialize and Deserialize ----

[<Fact>]
let ``saved event lines keep a name with a colon in it, and empty deals and rounds read back`` () =
    let events = [
        RoundEnded(Map.ofList [ "Dr: Who", 45u ])
        RoundEnded Map.empty
        Dealt3("Alice", "Alice", [])
    ]

    // The exact lines are pinned as well as the round trip, because a rename
    // made in both directions would still round-trip
    let lines = events |> List.map Event.Serialize

    Assert.Equal<string array list>(
        [
            [| "RoundEnded"; "Dr: Who: 45" |]
            [| "RoundEnded" |]
            [| "Dealt3"; "Alice"; "Alice" |]
        ],
        lines
    )

    // Deserialize splits a score line at its last ": ", so a colon inside the
    // name stays in the name
    Assert.Equal<Event list>(events, lines |> List.map Event.Deserialize)

// Names that hold the ": " a score line is split at, so the round trip meets
// them far more often than genName alone would make them
let private genScoreName: Gen<string> =
    Gen.frequency [ 1, Gen.elements [ "a: b"; "x: "; ": " ]; 3, genName ]

let private genEventWithColons: Gen<Event> =
    Gen.frequency [
        3, genEvent
        1, Gen.zip genScoreName genUint |> Gen.listOf |> Gen.map (Map.ofList >> RoundEnded)
    ]

[<Fact>]
let ``an event round-trips through serialization`` () =
    let property =
        Prop.forAll
            (Arb.fromGen genEventWithColons)
            (fun event ->
                (Event.Deserialize(Event.Serialize event) = event)
                |> Prop.label "the lines read back as the same event"
            )

    // Each case is a few string operations, and a hundred meets every kind of
    // event several times over
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

[<Fact>]
let ``Deserialize rejects lines that are not an event`` () =
    let refusal (lines: string list) =
        Assert.Throws<System.FormatException>(fun () -> Event.Deserialize lines |> ignore).Message

    Assert.Contains("Bogus", refusal [ "Bogus" ])
    Assert.Contains("Drew", refusal [ "Drew"; "Alice" ])
    Assert.Contains("Alice 45", refusal [ "RoundEnded"; "Alice 45" ])

// ---- Actor ----

[<Fact>]
let ``an event's actor is whoever's hand it changed`` () =
    // A freeze, a passed second chance and a deal3 change the target's hand,
    // not the drawer's, and an edit is the editor's doing rather than a
    // player's
    let expected = [
        Froze("Alice", "Bob"), Some "Bob"
        SecondChancePassed("Alice", "Bob"), Some "Bob"
        Dealt3("Alice", "Bob", []), Some "Bob"
        Busted("Alice", ValueCard Card.Five), Some "Alice"
        Edited "Alice", None
    ]

    Assert.Equal<(Event * string option) list>(expected, expected |> List.map (fun (event, _) -> event, event.Actor()))

// ---- captions ----

[<Fact>]
let ``a self-freeze reads as freezing themselves`` () =
    Assert.Equal("Alice drew Freeze and froze themselves", string (Froze("Alice", "Alice")))
    Assert.Equal("Alice drew Freeze and froze Bob", string (Froze("Alice", "Bob")))
