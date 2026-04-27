module ScoreBucketsTests

open Xunit
open Flip7

[<Fact>]
let ``Zero has total of 0`` () =
    let sb = ScoreBuckets.Zero
    Assert.Equal(0u, ScoreBuckets.Total sb)

[<Fact>]
let ``Total sums all buckets then multiplies`` () =
    let sb = {
        ModifierPoints = 3u
        ValuePoints = 5u
        BonusPoints = 2u
        Multiplier = 2u
    }
    Assert.Equal(15u, ScoreBuckets.Total sb)

[<Fact>]
let ``Total with multiplier 1 returns plain sum`` () =
    let sb = { ScoreBuckets.Zero with ValuePoints = 7u }
    Assert.Equal(7u, ScoreBuckets.Total sb)

[<Fact>]
let ``Addition adds points and multiplies multipliers`` () =
    let a = {
        ModifierPoints = 2u
        ValuePoints = 3u
        BonusPoints = 0u
        Multiplier = 2u
    }

    let b = {
        ModifierPoints = 1u
        ValuePoints = 4u
        BonusPoints = 5u
        Multiplier = 3u
    }

    let result = a + b
    Assert.Equal(3u, result.ModifierPoints)
    Assert.Equal(7u, result.ValuePoints)
    Assert.Equal(5u, result.BonusPoints)
    Assert.Equal(6u, result.Multiplier)

[<Fact>]
let ``Subtraction subtracts points and divides multipliers`` () =
    let a = {
        ModifierPoints = 5u
        ValuePoints = 10u
        BonusPoints = 15u
        Multiplier = 4u
    }

    let b = {
        ModifierPoints = 2u
        ValuePoints = 3u
        BonusPoints = 5u
        Multiplier = 2u
    }

    let result = a - b
    Assert.Equal(3u, result.ModifierPoints)
    Assert.Equal(7u, result.ValuePoints)
    Assert.Equal(10u, result.BonusPoints)
    Assert.Equal(2u, result.Multiplier)

[<Fact>]
let ``Equal score buckets are equal`` () =
    let a = {
        ModifierPoints = 2u
        ValuePoints = 3u
        BonusPoints = 0u
        Multiplier = 1u
    }

    let b = {
        ModifierPoints = 2u
        ValuePoints = 3u
        BonusPoints = 0u
        Multiplier = 1u
    }

    Assert.Equal(a, b)

[<Fact>]
let ``Score buckets with different fields are not equal`` () =
    let a = { ScoreBuckets.Zero with ValuePoints = 1u }
    let b = { ScoreBuckets.Zero with ValuePoints = 2u }
    Assert.NotEqual(a, b)

[<Fact>]
let ``Lower total compares less than higher total`` () =
    let low = { ScoreBuckets.Zero with ValuePoints = 1u }
    let high = { ScoreBuckets.Zero with ValuePoints = 2u }
    Assert.True(low < high)

[<Fact>]
let ``Higher total compares greater than lower total`` () =
    let low = { ScoreBuckets.Zero with ValuePoints = 1u }
    let high = { ScoreBuckets.Zero with ValuePoints = 2u }
    Assert.True(high > low)

[<Fact>]
let ``Equal totals compare as zero`` () =
    let a = { ScoreBuckets.Zero with ValuePoints = 3u }
    let b = { ScoreBuckets.Zero with ModifierPoints = 3u }
    Assert.True(a <= b && a >= b)
