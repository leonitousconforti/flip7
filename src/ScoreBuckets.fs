namespace Flip7

/// <summary>
/// The four buckets of points that contribute to a hand's total score:
///
/// <list type="bullet">
/// <item>Value points: the sum of the value cards in the hand.</item>
/// <item>Modifier points: the sum of the modifier cards in the hand.</item>
/// <item>Bonus points: the 15pts for getting to 7 cards before busting.</item>
/// <item>Multiplier: multiplies the sum of the other points.</item>
/// </list>
/// </summary>
[<CustomEquality>]
[<CustomComparison>]
type public ScoreBuckets = {
    ModifierPoints: uint
    ValuePoints: uint
    BonusPoints: uint
    Multiplier: uint
} with

    /// <summary>
    /// Calculates the total score from the buckets by adding all the value
    /// points, modifier points, and bonus points together before multiplying by
    /// the multiplier.
    /// </summary>
    static member public Total(scoreBuckets: ScoreBuckets) : uint =
        (scoreBuckets.ValuePoints
         + scoreBuckets.ModifierPoints
         + scoreBuckets.BonusPoints)
        * scoreBuckets.Multiplier

    /// <summary>
    /// The zero score, which has no points and a multiplier of 1. This is the
    /// starting point for all hands and is the identity element for addition.
    /// </summary>
    static member public Zero: ScoreBuckets = {
        ModifierPoints = 0u
        ValuePoints = 0u
        BonusPoints = 0u
        Multiplier = 1u
    }

    /// <summary>
    /// The maximum possible score, which is the sum of all the points from the
    /// top seven cards, plus one of all the modifier cards, plus the 15pts for
    /// getting the flip7 bonus, times two for the double multiplier.
    /// </summary>
    static member public Max: ScoreBuckets = {
        ModifierPoints = 12 + 11 + 10 + 9 + 8 + 7 + 6 |> uint
        ValuePoints = 10 + 8 + 6 + 4 + 2 |> uint
        BonusPoints = 15 |> uint
        Multiplier = 2 |> uint
    }

    static member public (+)(a: ScoreBuckets, b: ScoreBuckets) : ScoreBuckets = {
        ModifierPoints = a.ModifierPoints + b.ModifierPoints
        ValuePoints = a.ValuePoints + b.ValuePoints
        BonusPoints = a.BonusPoints + b.BonusPoints
        Multiplier = a.Multiplier * b.Multiplier
    }

    static member public (-)(a: ScoreBuckets, b: ScoreBuckets) : ScoreBuckets = {
        ModifierPoints = a.ModifierPoints - b.ModifierPoints
        ValuePoints = a.ValuePoints - b.ValuePoints
        BonusPoints = a.BonusPoints - b.BonusPoints
        Multiplier = a.Multiplier / b.Multiplier
    }

    override self.GetHashCode() : int =
        hash (self.ModifierPoints, self.ValuePoints, self.BonusPoints, self.Multiplier)

    override self.Equals(obj: obj) : bool =
        match obj with
        | :? ScoreBuckets as other ->
            self.ModifierPoints = other.ModifierPoints
            && self.ValuePoints = other.ValuePoints
            && self.BonusPoints = other.BonusPoints
            && self.Multiplier = other.Multiplier
        | _ -> false

    override self.ToString() : string =
        sprintf
            "ValuePoints: %u, ModifierPoints: %u, BonusPoints: %u, Multiplier: %u"
            self.ValuePoints
            self.ModifierPoints
            self.BonusPoints
            self.Multiplier

    interface System.IComparable with
        member self.CompareTo(obj: obj) : int =
            match obj with
            | :? ScoreBuckets as other ->
                let selfTotal = ScoreBuckets.Total self
                let otherTotal = ScoreBuckets.Total other
                selfTotal.CompareTo otherTotal
            | _ -> invalidArg "obj" "Cannot compare with non-ScoreBuckets type"
