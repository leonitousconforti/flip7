module EventTests

open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// ---- Serialize and Deserialize ----

// Whom a deal3's second chances were passed on to is saved after its cards,
// each on a line no card's reads like, so a name reads back as a name even
// where it reads as a card
[<Fact>]
let ``saved event lines keep a name with a colon in it, a deal's passes apart from its cards, and empty deals and rounds read back``
    ()
    =
    let secondChance = ActionCard Card.SecondChance

    let events = [
        RoundEnded(Map.ofList [ "Dr: Who", 45u ])
        RoundEnded Map.empty
        Dealt3("Alice", "Alice", [], [])
        Dealt3("Alice", "Bob", [ secondChance; ValueCard Card.Five; secondChance ], [ "Carol"; "5" ])
    ]

    // The exact lines are pinned as well as the round trip, because a rename
    // made in both directions would still round-trip
    let lines = events |> List.map Event.Serialize

    Assert.Equal<string array list>(
        [
            [| "RoundEnded"; "Dr: Who: 45" |]
            [| "RoundEnded" |]
            [| "Dealt3"; "Alice"; "Alice" |]
            [|
                "Dealt3"
                "Alice"
                "Bob"
                "SecondChance"
                "5"
                "SecondChance"
                "passed: Carol"
                "passed: 5"
            |]
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

    // A deal3 lists every card before whom any second chance went to
    Assert.Contains("Dealt3", refusal [ "Dealt3"; "Alice"; "Bob"; "passed: Carol"; "5" ])
    Assert.Contains("Bogus", refusal [ "Dealt3"; "Alice"; "Bob"; "Bogus" ])

// Builds before a deal3 recorded whom its second chances were passed on to
// saved only its cards, which read back as passing none on
[<Fact>]
let ``a deal3 saved before its passes were recorded reads back as passing none on`` () =
    Assert.Equal(
        Dealt3("Chloe", "Alice", [ ValueCard Card.Two; ValueCard Card.Eight; ValueCard Card.Twelve ], []),
        Event.Deserialize [ "Dealt3"; "Chloe"; "Alice"; "2"; "8"; "12" ]
    )

// ---- ToString ----

// A deal3's caption names whom its second chances were passed on to, in order
[<Fact>]
let ``a deal3's caption names whom its second chances were passed on to`` () =
    let secondChance = ActionCard Card.SecondChance

    Assert.Equal(
        "Alice drew Deal3, dealing Bob: SecondChance, 5, SecondChance, passing SecondChance to Carol then Dave",
        string (Dealt3("Alice", "Bob", [ secondChance; ValueCard Card.Five; secondChance ], [ "Carol"; "Dave" ]))
    )

    Assert.Equal("Alice drew Deal3, dealing Bob: 5", string (Dealt3("Alice", "Bob", [ ValueCard Card.Five ], [])))

// ---- Actor ----

[<Fact>]
let ``an event's actor is whoever's hand it changed`` () =
    // A freeze, a passed second chance and a deal3 change the target's hand,
    // not the drawer's, and an edit is the editor's doing rather than a
    // player's
    let expected = [
        Froze("Alice", "Bob"), Some "Bob"
        SecondChancePassed("Alice", "Bob"), Some "Bob"
        Dealt3("Alice", "Bob", [], []), Some "Bob"
        Busted("Alice", ValueCard Card.Five), Some "Alice"
        Edited "Alice", None
    ]

    Assert.Equal<(Event * string option) list>(expected, expected |> List.map (fun (event, _) -> event, event.Actor()))

// ---- captions ----

[<Fact>]
let ``a self-freeze reads as freezing themselves`` () =
    Assert.Equal("Alice drew Freeze and froze themselves", string (Froze("Alice", "Alice")))
    Assert.Equal("Alice drew Freeze and froze Bob", string (Froze("Alice", "Bob")))
