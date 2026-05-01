module DeckTests

open Xunit
open Flip7

[<Fact>]
let ``Empty deck is empty`` () = Assert.True(Deck.IsEmpty Deck.Empty)

[<Fact>]
let ``Full deck is not empty`` () = Assert.False(Deck.IsEmpty Deck.Full)

[<Fact>]
let ``Count of Empty deck is 0`` () = Assert.Equal(0I, Deck.Count Deck.Empty)

[<Fact>]
let ``Count of Full deck is 94`` () = Assert.Equal(94I, Deck.Count Deck.Full)

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
    Assert.Equal(1, List.length [ cards ])

[<Fact>]
let ``Draw1 reduces deck count by 1`` () =
    let deck', _, _ = Deck.Draw1 Deck.Full Deck.Empty
    Assert.Equal(93I, Deck.Count deck')

[<Fact>]
let ``Draw3 returns exactly three cards`` () =
    let _, _, cards = Deck.Draw3 Deck.Full Deck.Empty
    Assert.Equal(3, List.length cards)

[<Fact>]
let ``Draw3 reduces deck count by 3`` () =
    let deck', _, _ = Deck.Draw3 Deck.Full Deck.Empty
    Assert.Equal(91I, Deck.Count deck')

[<Fact>]
let ``pdf of Full deck - values sum to 1`` () =
    let total = Deck.pdf Deck.Full |> Map.fold (fun acc _ v -> acc + v) 0.0
    Assert.InRange(total, 0.9999, 1.0001)

[<Fact>]
let ``pdf of Full deck - each card has correct probability`` () =
    let pdf = Deck.pdf Deck.Full
    Assert.InRange(pdf[ValueCard Card.Zero], 1.0 / 94.0 - 1e-9, 1.0 / 94.0 + 1e-9)
    Assert.InRange(pdf[ValueCard Card.One], 1.0 / 94.0 - 1e-9, 1.0 / 94.0 + 1e-9)
    Assert.InRange(pdf[ValueCard Card.Seven], 7.0 / 94.0 - 1e-9, 7.0 / 94.0 + 1e-9)
    Assert.InRange(pdf[ValueCard Card.Eleven], 11.0 / 94.0 - 1e-9, 11.0 / 94.0 + 1e-9)
    Assert.InRange(pdf[ValueCard Card.Twelve], 12.0 / 94.0 - 1e-9, 12.0 / 94.0 + 1e-9)
    Assert.InRange(pdf[ActionCard Card.Deal3], 3.0 / 94.0 - 1e-9, 3.0 / 94.0 + 1e-9)

[<Fact>]
let ``pdf of Empty deck - all values are 0.0`` () =
    let pdf = Deck.pdf Deck.Empty
    Assert.True(Map.forall (fun _ v -> v = 0.0) pdf)

[<Fact>]
let ``pdf of single-card deck - that card has probability 1.0, rest are 0.0`` () =
    let pdf = Deck.Empty |> Map.add (ValueCard Card.Five) 1u |> Deck.pdf
    Assert.InRange(pdf[ValueCard Card.Five], 1.0 - 1e-9, 1.0 + 1e-9)
    Assert.True(Map.forall (fun card v -> card = ValueCard Card.Five || v = 0.0) pdf)

[<Fact>]
let ``pdf of two-card-type deck - probabilities reflect counts`` () =
    let pdf =
        Deck.Empty
        |> Map.add (ValueCard Card.One) 1u
        |> Map.add (ValueCard Card.Three) 3u
        |> Deck.pdf
    Assert.InRange(pdf[ValueCard Card.One], 0.25 - 1e-9, 0.25 + 1e-9)
    Assert.InRange(pdf[ValueCard Card.Three], 0.75 - 1e-9, 0.75 + 1e-9)

[<Fact>]
let ``cdf of Full deck - last entry is 1`` () =
    let lastValue = Deck.cdf Deck.Full |> Map.toList |> List.last |> snd
    Assert.InRange(lastValue, 0.9999, 1.0001)

[<Fact>]
let ``cdf of Full deck - values are monotonically non-decreasing`` () =
    let values = Deck.cdf Deck.Full |> Map.toList |> List.sortBy fst |> List.map snd
    let pairs = List.pairwise values
    Assert.True(List.forall (fun (a, b) -> b >= a) pairs)

[<Fact>]
let ``cdf of Empty deck - all values are 0.0`` () =
    let cdf = Deck.cdf Deck.Empty
    Assert.True(Map.forall (fun _ v -> v = 0.0) cdf)

[<Fact>]
let ``cdf of single-card deck - cards before Five are 0.0, Five and after are 1.0`` () =
    let cdf = Deck.Empty |> Map.add (ValueCard Card.Five) 1u |> Deck.cdf
    let sorted = cdf |> Map.toList |> List.sortBy fst
    let beforeFive =
        sorted
        |> List.takeWhile (fun (card, _) -> card < ValueCard Card.Five)
        |> List.map snd
    let fiveAndAfter =
        sorted
        |> List.skipWhile (fun (card, _) -> card < ValueCard Card.Five)
        |> List.map snd
    Assert.True(List.forall (fun v -> v = 0.0) beforeFive)
    Assert.True(List.forall (fun v -> abs (v - 1.0) < 1e-9) fiveAndAfter)

[<Fact>]
let ``cdf of two-card-type deck - cumulative values are correct`` () =
    let cdf =
        Deck.Empty
        |> Map.add (ValueCard Card.One) 1u
        |> Map.add (ValueCard Card.Three) 3u
        |> Deck.cdf
    Assert.InRange(cdf[ValueCard Card.One], 0.25 - 1e-9, 0.25 + 1e-9)
    Assert.InRange(cdf[ValueCard Card.Two], 0.25 - 1e-9, 0.25 + 1e-9)
    Assert.InRange(cdf[ValueCard Card.Three], 1.0 - 1e-9, 1.0 + 1e-9)

[<Fact>]
let ``Draw returns the requested number of cards`` () =
    let _, _, cards = Deck.Draw Deck.Full Deck.Empty 5u
    Assert.Equal(5, List.length cards)

[<Fact>]
let ``Draw reduces deck count by the requested amount`` () =
    let deck', _, _ = Deck.Draw Deck.Full Deck.Empty 5u
    Assert.Equal(89I, Deck.Count deck')

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
