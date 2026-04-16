namespace Flip7

[<CustomEquality>]
[<CustomComparison>]
type public ScoreBuckets = {
    ModifierPoints: uint
    ValuePoints: uint
    BonusPoints: uint
    Multiplier: uint
} with

    static member Total(scoreBuckets: ScoreBuckets) : uint =
        (scoreBuckets.ValuePoints
         + scoreBuckets.ModifierPoints
         + scoreBuckets.BonusPoints)
        * scoreBuckets.Multiplier

    static member Zero: ScoreBuckets = {
        ModifierPoints = 0u
        ValuePoints = 0u
        BonusPoints = 0u
        Multiplier = 1u
    }

    static member Max: ScoreBuckets = {
        ModifierPoints = 12 + 11 + 10 + 9 + 8 + 7 + 6 |> uint
        ValuePoints = 10 + 8 + 6 + 4 + 2 |> uint
        BonusPoints = 15 |> uint
        Multiplier = 2 |> uint
    }

    static member (+)(a: ScoreBuckets, b: ScoreBuckets) : ScoreBuckets = {
        ModifierPoints = a.ModifierPoints + b.ModifierPoints
        ValuePoints = a.ValuePoints + b.ValuePoints
        BonusPoints = a.BonusPoints + b.BonusPoints
        Multiplier = a.Multiplier * b.Multiplier
    }

    static member (-)(a: ScoreBuckets, b: ScoreBuckets) : ScoreBuckets = {
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
