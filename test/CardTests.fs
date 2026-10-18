module CardTests

open Xunit
open Flip7

// Deck.Full holds every card in the game, in Card's structural order: actions,
// then modifiers, then values. Each test covers all of them.

// ---- scores ----

[<Fact>]
let ``every card scores what its face shows`` () =
    let values =
        [
            Card.Zero
            Card.One
            Card.Two
            Card.Three
            Card.Four
            Card.Five
            Card.Six
            Card.Seven
            Card.Eight
            Card.Nine
            Card.Ten
            Card.Eleven
            Card.Twelve
        ]
        |> List.mapi (fun n card -> ValueCard card, { ScoreBuckets.Zero with ValuePoints = uint n })

    let modifiers =
        [
            Card.Plus2, 2u
            Card.Plus4, 4u
            Card.Plus6, 6u
            Card.Plus8, 8u
            Card.Plus10, 10u
        ]
        |> List.map (fun (card, points) -> ModifierCard card, { ScoreBuckets.Zero with ModifierPoints = points })

    let multiplier =
        ModifierCard Card.Double, { ScoreBuckets.Zero with Multiplier = 2u }

    let actions =
        [ Card.Deal3; Card.Freeze; Card.SecondChance ]
        |> List.map (fun card -> ActionCard card, ScoreBuckets.Zero)

    let expected = values @ modifiers @ [ multiplier ] @ actions
    // Sorted, the table's cards are Deck.Full's, so it names every card once
    Assert.Equal<Card list>(List.ofSeq Deck.Full.Keys, expected |> List.map fst |> List.sort)

    // F# = compares every bucket, where Assert.Equal would compare totals only
    // and pass a Double worth nothing
    for card, worth in expected do
        Assert.True((card.Value = worth), $"{card} is worth {card.Value}, expected {worth}")

// ---- strings ----

// These strings are the on-disk format: Event.Serialize and Persistence write
// every card this way and read it back with Card.Parse
[<Fact>]
let ``every card is written the way saved games spell it`` () =
    let cards = List.ofSeq Deck.Full.Keys

    Assert.Equal<string list>(
        [
            "Deal3"
            "Freeze"
            "SecondChance"
            "+2"
            "+4"
            "+6"
            "+8"
            "+10"
            "x2"
            "0"
            "1"
            "2"
            "3"
            "4"
            "5"
            "6"
            "7"
            "8"
            "9"
            "10"
            "11"
            "12"
        ],
        cards |> List.map string
    )

    for card in cards do
        Assert.Equal(card, Card.Parse(string card))
