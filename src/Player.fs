namespace Flip7

/// <summary>
/// A strategy decides whether a player hits or stands. Strategies are
/// represented as data rather than functions so that they can be serialized and
/// deserialized; use Strategy.DecideWith to evaluate one.
/// </summary>
[<RequireQualifiedAccess>]
type public Strategy =
    | AlwaysHits
    | AlwaysStands
    | RandomWithProbability of float
    | HitUntilScore of uint
    | HitUntilNumCards of uint
    | HitUntilBustProbability of float
    | HitUntilNaiveBustProbability of float
    | SoftHitUntilScore of Threshold: uint * Temperature: float
    | HitUntilTotal of uint
    | HitUntilUniqueValues of uint
    | ChasesFlip7 of Score: uint * Uniques: uint
    | EmboldenedBySecondChance of uint
    | HitWhileBehindLeader of uint
    | StandsAfterTurn of uint
    | MaximizesExpectedValue
    | Custom of string

    override self.ToString() : string =
        let writeFloat (value: float) =
            value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        match self with
        | AlwaysHits -> "AlwaysHits"
        | AlwaysStands -> "AlwaysStands"
        | RandomWithProbability probability -> $"RandomWithProbability {writeFloat probability}"
        | HitUntilScore threshold -> $"HitUntilScore {threshold}"
        | HitUntilNumCards threshold -> $"HitUntilNumCards {threshold}"
        | HitUntilBustProbability threshold -> $"HitUntilBustProbability {writeFloat threshold}"
        | HitUntilNaiveBustProbability threshold -> $"HitUntilNaiveBustProbability {writeFloat threshold}"
        | SoftHitUntilScore(threshold, temperature) -> $"SoftHitUntilScore {threshold} {writeFloat temperature}"
        | HitUntilTotal target -> $"HitUntilTotal {target}"
        | HitUntilUniqueValues threshold -> $"HitUntilUniqueValues {threshold}"
        | ChasesFlip7(score, uniques) -> $"ChasesFlip7 {score} {uniques}"
        | EmboldenedBySecondChance threshold -> $"EmboldenedBySecondChance {threshold}"
        | HitWhileBehindLeader margin -> $"HitWhileBehindLeader {margin}"
        | StandsAfterTurn turns -> $"StandsAfterTurn {turns}"
        | MaximizesExpectedValue -> "MaximizesExpectedValue"
        | Custom name -> $"Custom {name}"

    static member public Parse(string: string) : Strategy =
        let readFloat (value: string) =
            System.Double.Parse(value, System.Globalization.CultureInfo.InvariantCulture)

        let readUint (value: string) =
            System.UInt32.Parse(value, System.Globalization.CultureInfo.InvariantCulture)

        if string.StartsWith("Custom ", System.StringComparison.Ordinal) then
            Custom(string.Substring("Custom ".Length))
        else
            match string.Split ' ' with
            | [| "AlwaysHits" |] -> AlwaysHits
            | [| "AlwaysStands" |] -> AlwaysStands
            | [| "RandomWithProbability"; probability |] -> RandomWithProbability(readFloat probability)
            | [| "HitUntilScore"; threshold |] -> HitUntilScore(readUint threshold)
            | [| "HitUntilNumCards"; threshold |] -> HitUntilNumCards(readUint threshold)
            | [| "HitUntilBustProbability"; threshold |] -> HitUntilBustProbability(readFloat threshold)
            | [| "HitUntilNaiveBustProbability"; threshold |] -> HitUntilNaiveBustProbability(readFloat threshold)
            | [| "SoftHitUntilScore"; threshold; temp |] -> SoftHitUntilScore(readUint threshold, readFloat temp)
            | [| "HitUntilTotal"; target |] -> HitUntilTotal(readUint target)
            | [| "HitUntilUniqueValues"; threshold |] -> HitUntilUniqueValues(readUint threshold)
            | [| "ChasesFlip7"; score; uniques |] -> ChasesFlip7(readUint score, readUint uniques)
            | [| "EmboldenedBySecondChance"; threshold |] -> EmboldenedBySecondChance(readUint threshold)
            | [| "HitWhileBehindLeader"; margin |] -> HitWhileBehindLeader(readUint margin)
            | [| "StandsAfterTurn"; turns |] -> StandsAfterTurn(readUint turns)
            | [| "MaximizesExpectedValue" |] -> MaximizesExpectedValue
            | _ -> raise (System.FormatException $"Invalid strategy string: {string}")

    static member TryParse(string: string) : Strategy option =
        try
            string |> Strategy.Parse |> Some
        with :? System.FormatException ->
            None

/// <summary>
/// A targeting policy decides who an action card is given to: who gets frozen,
/// who is dealt three cards, and who is passed a second chance its holder
/// cannot keep. Like a strategy it is data rather than a function so that it
/// can be serialized and deserialized; use Strategy.DecideTargetWith to
/// evaluate one. The escape hatch is called ChoosesExternally rather than
/// Custom because Strategy.Custom is matched unqualified in several places and
/// a second case of the same name in this namespace would rebind those matches.
/// </summary>
[<RequireQualifiedAccess>]
type public Targeting =
    | ChoosesRandomly
    | PlaysSpitefully
    | PlaysGreedily of BanksAbove: float * Deal3sBelow: float
    | ChoosesExternally of string

    override self.ToString() : string =
        let writeFloat (value: float) =
            value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        match self with
        | ChoosesRandomly -> "ChoosesRandomly"
        | PlaysSpitefully -> "PlaysSpitefully"
        | PlaysGreedily(banksAbove, deal3sBelow) -> $"PlaysGreedily {writeFloat banksAbove} {writeFloat deal3sBelow}"
        | ChoosesExternally name -> $"ChoosesExternally {name}"

    static member public Parse(string: string) : Targeting =
        let readFloat (value: string) =
            System.Double.Parse(value, System.Globalization.CultureInfo.InvariantCulture)

        if string.StartsWith("ChoosesExternally ", System.StringComparison.Ordinal) then
            ChoosesExternally(string.Substring("ChoosesExternally ".Length))
        else
            match string.Split ' ' with
            | [| "ChoosesRandomly" |] -> ChoosesRandomly
            | [| "PlaysSpitefully" |] -> PlaysSpitefully
            | [| "PlaysGreedily"; banksAbove; deal3sBelow |] ->
                PlaysGreedily(readFloat banksAbove, readFloat deal3sBelow)
            | _ -> raise (System.FormatException $"Invalid targeting string: {string}")

    static member TryParse(string: string) : Targeting option =
        try
            string |> Targeting.Parse |> Some
        with :? System.FormatException ->
            None

type public Player = {
    Name: string
    Strategy: Strategy
    Targeting: Targeting
    FirmScore: uint
    Hand: Hand
} with

    static member Make
        (name: string, strategy: Strategy, ?firmScore: uint, ?hand: Hand, ?targeting: Targeting)
        : Player
        =
        let firmScore = defaultArg firmScore 0u
        let hand = defaultArg hand []
        let targeting = defaultArg targeting Targeting.ChoosesRandomly

        {
            Name = name
            Strategy = strategy
            Targeting = targeting
            FirmScore = firmScore
            Hand = hand
        }
