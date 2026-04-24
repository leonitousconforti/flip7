module CardTests

open Xunit
open Flip7

[<Fact>]
let ``ValueCard Zero has 0 value points`` () =
    let card = ValueCard Card.Zero
    Assert.Equal(0u, card.Value.ValuePoints)

[<Fact>]
let ``ValueCard Seven has 7 value points`` () =
    let card = ValueCard Card.Seven
    Assert.Equal(7u, card.Value.ValuePoints)

[<Fact>]
let ``ValueCard Twelve has 12 value points`` () =
    let card = ValueCard Card.Twelve
    Assert.Equal(12u, card.Value.ValuePoints)

[<Theory>]
[<InlineData(2u)>]
[<InlineData(4u)>]
[<InlineData(6u)>]
[<InlineData(8u)>]
[<InlineData(10u)>]
let ``Plus modifier cards have correct modifier points`` (points: uint) =
    let card =
        match points with
        | 2u -> ModifierCard Card.Plus2
        | 4u -> ModifierCard Card.Plus4
        | 6u -> ModifierCard Card.Plus6
        | 8u -> ModifierCard Card.Plus8
        | 10u -> ModifierCard Card.Plus10
        | _ -> failwith "unexpected"

    Assert.Equal(points, card.Value.ModifierPoints)
    Assert.Equal(0u, card.Value.ValuePoints)
    Assert.Equal(1u, card.Value.Multiplier)

[<Fact>]
let ``Double card has multiplier of 2 and no points`` () =
    let card = ModifierCard Card.Double
    Assert.Equal(2u, card.Value.Multiplier)
    Assert.Equal(0u, card.Value.ModifierPoints)
    Assert.Equal(0u, card.Value.ValuePoints)

[<Fact>]
let ``All action cards have Zero score`` () =
    Assert.Equal(ScoreBuckets.Zero, (ActionCard Card.Deal3).Value)
    Assert.Equal(ScoreBuckets.Zero, (ActionCard Card.Freeze).Value)
    Assert.Equal(ScoreBuckets.Zero, (ActionCard Card.SecondChance).Value)

[<Fact>]
let ``ValueCard ToString returns numeric string`` () =
    Assert.Equal("0", (ValueCard Card.Zero).ToString())
    Assert.Equal("7", (ValueCard Card.Seven).ToString())
    Assert.Equal("12", (ValueCard Card.Twelve).ToString())

[<Fact>]
let ``ModifierCard ToString returns name`` () =
    Assert.Equal("+2", (ModifierCard Card.Plus2).ToString())
    Assert.Equal("+10", (ModifierCard Card.Plus10).ToString())
    Assert.Equal("x2", (ModifierCard Card.Double).ToString())

[<Fact>]
let ``ActionCard ToString returns name`` () =
    Assert.Equal("Deal3", (ActionCard Card.Deal3).ToString())
    Assert.Equal("Freeze", (ActionCard Card.Freeze).ToString())
    Assert.Equal("SecondChance", (ActionCard Card.SecondChance).ToString())
