module ScoreBucketsTests

open Xunit
open Flip7

// xunit's Assert.Equal compares buckets through IComparable, that is by their
// Total alone, so these tests assert bucket equality with F# =, which calls
// the structural Equals.

let private buckets value modifier bonus multiplier = {
    ValuePoints = value
    ModifierPoints = modifier
    BonusPoints = bonus
    Multiplier = multiplier
}

// ---- total ----

[<Fact>]
let ``the multiplier doubles only the value points, then modifiers and bonus are added`` () =
    // Doubling the whole hand would give (5 + 3 + 2) * 2 instead. The official
    // scoring doubles the value cards alone
    Assert.Equal(5u * 2u + 3u + 2u, ScoreBuckets.Total(buckets 5u 3u 2u 2u))

// ---- addition and subtraction ----

[<Fact>]
let ``Zero is the identity of addition`` () =
    // Every bucket is nonzero and the multiplier is 2, so a Zero holding any
    // points, or a multiplier of 0 that would wipe out every value card, fails
    let a = buckets 5u 3u 15u 2u
    Assert.True((a + ScoreBuckets.Zero = a), $"{a} + Zero gave {a + ScoreBuckets.Zero}")
    Assert.True((ScoreBuckets.Zero + a = a), $"Zero + {a} gave {ScoreBuckets.Zero + a}")

// Hand.Score adds up its cards' buckets with (+)
[<Fact>]
let ``addition adds the points bucket by bucket and multiplies the multipliers`` () =
    let a = buckets 3u 2u 0u 2u
    let b = buckets 4u 1u 5u 3u
    let expected = buckets (3u + 4u) (2u + 1u) (0u + 5u) (2u * 3u)
    Assert.True((a + b = expected), $"{a} + {b} gave {a + b}, expected {expected}")

[<Fact>]
let ``subtracting what was added gives back the original`` () =
    // The multipliers tell dividing from subtracting: 6 / 3 is 2, but 6 - 3 is 3
    let a = buckets 10u 5u 15u 2u
    let b = buckets 3u 2u 5u 3u
    Assert.True(((a + b) - b = a), $"({a} + {b}) - {b} gave {(a + b) - b}")
    // 1 / 2 floors to 0, and a zero multiplier would wipe out every value card
    Assert.Equal(1u, (ScoreBuckets.Zero - { ScoreBuckets.Zero with Multiplier = 2u }).Multiplier)

// ---- equality and comparison ----

[<Fact>]
let ``buckets are equal exactly when every bucket matches`` () =
    let original = buckets 5u 3u 15u 2u
    let copy = buckets 5u 3u 15u 2u
    Assert.True((original = copy), "a copy with every bucket the same")
    Assert.Equal(hash original, hash copy)
    Assert.False((original = { original with ValuePoints = 6u }), "ValuePoints")
    Assert.False((original = { original with ModifierPoints = 4u }), "ModifierPoints")
    Assert.False((original = { original with BonusPoints = 0u }), "BonusPoints")
    Assert.False((original = { original with Multiplier = 1u }), "Multiplier")

[<Fact>]
let ``buckets compare by total, so different mixes with the same total compare equal without being equal`` () =
    let threeInValue = buckets 3u 0u 0u 1u
    let threeInModifiers = buckets 0u 3u 0u 1u
    Assert.Equal(0, compare threeInValue threeInModifiers)
    Assert.False((threeInValue = threeInModifiers), "same total, different mix")
    // Ten modifier points against six value points doubled: the lower total
    // holds more points before the multiplier, so a comparison that skipped
    // the multiplier would order the two the other way
    let low = buckets 0u 10u 0u 1u
    let high = buckets 6u 0u 0u 2u
    Assert.True(compare low high < 0, "a lower total compares less")
    Assert.True(compare high low > 0, "a higher total compares greater")
