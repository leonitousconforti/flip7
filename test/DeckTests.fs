module DeckTests

open Xunit
open Flip7

[<Fact>]
let ``Empty deck is empty`` () = Assert.True(Deck.IsEmpty Deck.Empty)

[<Fact>]
let ``Full deck is not empty`` () = Assert.False(Deck.IsEmpty Deck.Full)

[<Fact>]
let ``Count of Empty deck is 0`` () = Assert.Equal(0u, Deck.Count Deck.Empty)

[<Fact>]
let ``Count of Full deck is 94`` () = Assert.Equal(94u, Deck.Count Deck.Full)

[<Fact>]
let ``Full deck has correct per-card counts`` () =
    Assert.Equal(1u, Deck.Full[ValueCard Card.Zero])
    Assert.Equal(1u, Deck.Full[ValueCard Card.One])
    Assert.Equal(12u, Deck.Full[ValueCard Card.Twelve])
    Assert.Equal(1u, Deck.Full[ModifierCard Card.Double])
    Assert.Equal(3u, Deck.Full[ActionCard Card.Deal3])
    Assert.Equal(3u, Deck.Full[ActionCard Card.SecondChance])

[<Fact>]
let ``Draw1 returns exactly one card`` () =
    let _, _, cards = Deck.Draw1 Deck.Full Deck.Empty
    Assert.Equal(1, List.length cards)

[<Fact>]
let ``Draw1 reduces deck count by 1`` () =
    let newDeck, _, _ = Deck.Draw1 Deck.Full Deck.Empty
    Assert.Equal(93u, Deck.Count newDeck)

[<Fact>]
let ``Draw3 returns exactly three cards`` () =
    let _, _, cards = Deck.Draw3 Deck.Full Deck.Empty
    Assert.Equal(3, List.length cards)

[<Fact>]
let ``Draw3 reduces deck count by 3`` () =
    let newDeck, _, _ = Deck.Draw3 Deck.Full Deck.Empty
    Assert.Equal(91u, Deck.Count newDeck)

[<Fact>]
let ``pdf values sum to 1`` () =
    let total = Deck.pdf Deck.Full |> Map.fold (fun acc _ v -> acc + v) 0.0
    Assert.InRange(total, 0.9999, 1.0001)

[<Fact>]
let ``cdf last entry is 1`` () =
    let lastValue = Deck.cdf Deck.Full |> Map.toList |> List.last |> snd
    Assert.InRange(lastValue, 0.9999, 1.0001)

[<Fact>]
let ``Draw returns the requested number of cards`` () =
    let _, _, cards = Deck.Draw Deck.Full Deck.Empty 5u
    Assert.Equal(5, List.length cards)

[<Fact>]
let ``Draw reduces deck count by the requested amount`` () =
    let newDeck, _, _ = Deck.Draw Deck.Full Deck.Empty 5u
    Assert.Equal(89u, Deck.Count newDeck)

[<Fact>]
let ``Draw from single-card deck continues from discards`` () =
    let singleCard = Deck.Empty |> Map.add (ValueCard Card.Five) 1u
    let _, _, cards = Deck.Draw singleCard Deck.Full 3u
    Assert.Equal(3, List.length cards)

[<Fact>]
let ``Draw 1 card from single-card deck returns that card`` () =
    let singleCard = Deck.Empty |> Map.add (ValueCard Card.Five) 1u
    let _, _, cards = Deck.Draw singleCard Deck.Empty 1u
    Assert.Equal<Card list>([ ValueCard Card.Five ], cards)
