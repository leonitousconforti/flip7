module HandTests

open Xunit
open Flip7

[<Fact>]
let ``HasFlip7Bonus is false for empty hand`` () = Assert.False(Hand.HasFlip7Bonus [])

[<Fact>]
let ``HasFlip7Bonus is false for fewer than 7 value cards`` () =
    let hand = [ ValueCard Card.One; ValueCard Card.Two; ValueCard Card.Three ]
    Assert.False(Hand.HasFlip7Bonus hand)

[<Fact>]
let ``HasFlip7Bonus is true for exactly 7 value cards`` () =
    let hand = [
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Seven
    ]

    Assert.True(Hand.HasFlip7Bonus hand)

[<Fact>]
let ``HasFlip7Bonus is true for more than 7 value cards`` () =
    let hand = [
        ValueCard Card.Zero
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Seven
    ]

    Assert.True(Hand.HasFlip7Bonus hand)

[<Fact>]
let ``HasFlip7Bonus only counts value cards not action or modifier`` () =
    let hand = [
        ActionCard Card.Deal3
        ModifierCard Card.Double
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
    ]

    Assert.False(Hand.HasFlip7Bonus hand)

[<Fact>]
let ``Score of empty hand is 0`` () =
    let score = Hand.Score []
    Assert.Equal(0u, score)

[<Fact>]
let ``Score sums value points without bonus for fewer than 7 value cards`` () =
    let hand = [ ValueCard Card.Three; ValueCard Card.Four ]
    Assert.Equal(3u + 4u, Hand.Score hand)

[<Fact>]
let ``Score includes 15 point bonus for 7 or more value cards`` () =
    let hand = [
        ValueCard Card.One
        ValueCard Card.Two
        ValueCard Card.Three
        ValueCard Card.Four
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Seven
    ]

    Assert.Equal(1u + 2u + 3u + 4u + 5u + 6u + 7u + 15u, Hand.Score hand)

[<Fact>]
let ``Score adds modifier points`` () =
    let hand = [ ValueCard Card.Three; ModifierCard Card.Plus4 ]
    Assert.Equal(3u + 4u, Hand.Score hand)

[<Fact>]
let ``Score with Double modifier doubles the total`` () =
    let hand = [ ValueCard Card.Five; ModifierCard Card.Double ]
    Assert.Equal(5u * 2u, Hand.Score hand)

[<Fact>]
let ``Score with action cards ignores their value`` () =
    let hand = [ ValueCard Card.Five; ActionCard Card.Freeze ]
    Assert.Equal(5u, Hand.Score hand)

[<Fact>]
let ``IsBust is false for empty hand`` () =
    let hand = []
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust is false when all value cards are unique`` () =
    let hand = [ ValueCard Card.One; ValueCard Card.Two; ValueCard Card.Three ]
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust is true when a value card appears twice`` () =
    let hand = [ ValueCard Card.Five; ValueCard Card.Five ]
    Assert.True(Hand.IsBust hand)

[<Fact>]
let ``IsBust is false when SecondChance appears before the duplicate`` () =
    let hand = [ ActionCard Card.SecondChance; ValueCard Card.Five; ValueCard Card.Five ]
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust is false when duplicate precedes SecondChance`` () =
    let hand = [ ValueCard Card.Five; ValueCard Card.Five; ActionCard Card.SecondChance ]
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust is true when multiple duplicates are present but only one SecondChance`` () =
    let hand = [
        ValueCard Card.Five
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Six
        ActionCard Card.SecondChance
    ]
    Assert.True(Hand.IsBust hand)

[<Fact>]
let ``IsBust is false when multiple duplicates are present with two SecondChance`` () =
    let hand = [
        ValueCard Card.Five
        ActionCard Card.SecondChance
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Six
        ActionCard Card.SecondChance
    ]
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust is false with one duplicate pair with two SecondChance`` () =
    let hand = [
        ActionCard Card.SecondChance
        ValueCard Card.Six
        ValueCard Card.Six
        ActionCard Card.SecondChance
    ]
    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``IsBust ignores modifier cards`` () =
    let hand = [
        ModifierCard Card.Double
        ModifierCard Card.Double
        ValueCard Card.One
        ValueCard Card.Two
    ]

    Assert.False(Hand.IsBust hand)
