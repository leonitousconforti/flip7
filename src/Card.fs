namespace Flip7

module public Card =
    /// <summary>
    /// Doesn't contribute points but can affect the hand in other ways, e.g.
    /// causing a bust to be ignored or forcing another player to draw cards.
    /// </summary>
    type public ActionCard =
        | Deal3
        | Freeze
        | SecondChance

    /// <summary>
    /// Contributes modifier points and/or a score multiplier to the hand.
    /// </summary>
    type public ModifierCard =
        | Plus2
        | Plus4
        | Plus6
        | Plus8
        | Plus10
        | Double

    /// <summary>
    /// Contributes a certain number of points to the hand.
    /// </summary>
    type public ValueCard =
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Eleven
        | Twelve

/// <summary>
/// A card in flip7 can be one of three types: a value card, which contributes a
/// certain number of points to the hand; a modifier card, which contributes
/// modifier points and/or multipliers to the hand; or an action card, which
/// doesn't contribute any points but can affect the hand in other ways (e.g.
/// causing a bust to be ignored or forcing another player to draw cards).
/// </summary>
type public Card =
    | ActionCard of Card.ActionCard
    | ModifierCard of Card.ModifierCard
    | ValueCard of Card.ValueCard

    static member public Parse(string: string) : Card =
        match string with
        | "Deal3" -> ActionCard Card.Deal3
        | "Freeze" -> ActionCard Card.Freeze
        | "SecondChance" -> ActionCard Card.SecondChance
        | "+2" -> ModifierCard Card.Plus2
        | "+4" -> ModifierCard Card.Plus4
        | "+6" -> ModifierCard Card.Plus6
        | "+8" -> ModifierCard Card.Plus8
        | "+10" -> ModifierCard Card.Plus10
        | "x2" -> ModifierCard Card.Double
        | "0" -> ValueCard Card.Zero
        | "1" -> ValueCard Card.One
        | "2" -> ValueCard Card.Two
        | "3" -> ValueCard Card.Three
        | "4" -> ValueCard Card.Four
        | "5" -> ValueCard Card.Five
        | "6" -> ValueCard Card.Six
        | "7" -> ValueCard Card.Seven
        | "8" -> ValueCard Card.Eight
        | "9" -> ValueCard Card.Nine
        | "10" -> ValueCard Card.Ten
        | "11" -> ValueCard Card.Eleven
        | "12" -> ValueCard Card.Twelve
        | _ -> raise (System.ArgumentException $"Invalid card string: {string}")

    static member TryParse(string: string) : Card option =
        try
            string |> Card.Parse |> Some
        with :? System.ArgumentException ->
            None

    member public self.Value: ScoreBuckets =
        match self with
        | ActionCard _ -> ScoreBuckets.Zero
        | ModifierCard Card.Plus2 -> { ScoreBuckets.Zero with ModifierPoints = 2u }
        | ModifierCard Card.Plus4 -> { ScoreBuckets.Zero with ModifierPoints = 4u }
        | ModifierCard Card.Plus6 -> { ScoreBuckets.Zero with ModifierPoints = 6u }
        | ModifierCard Card.Plus8 -> { ScoreBuckets.Zero with ModifierPoints = 8u }
        | ModifierCard Card.Plus10 -> { ScoreBuckets.Zero with ModifierPoints = 10u }
        | ModifierCard Card.Double -> { ScoreBuckets.Zero with Multiplier = 2u }
        | ValueCard Card.Zero -> { ScoreBuckets.Zero with ValuePoints = 0u }
        | ValueCard Card.One -> { ScoreBuckets.Zero with ValuePoints = 1u }
        | ValueCard Card.Two -> { ScoreBuckets.Zero with ValuePoints = 2u }
        | ValueCard Card.Three -> { ScoreBuckets.Zero with ValuePoints = 3u }
        | ValueCard Card.Four -> { ScoreBuckets.Zero with ValuePoints = 4u }
        | ValueCard Card.Five -> { ScoreBuckets.Zero with ValuePoints = 5u }
        | ValueCard Card.Six -> { ScoreBuckets.Zero with ValuePoints = 6u }
        | ValueCard Card.Seven -> { ScoreBuckets.Zero with ValuePoints = 7u }
        | ValueCard Card.Eight -> { ScoreBuckets.Zero with ValuePoints = 8u }
        | ValueCard Card.Nine -> { ScoreBuckets.Zero with ValuePoints = 9u }
        | ValueCard Card.Ten -> { ScoreBuckets.Zero with ValuePoints = 10u }
        | ValueCard Card.Eleven -> { ScoreBuckets.Zero with ValuePoints = 11u }
        | ValueCard Card.Twelve -> { ScoreBuckets.Zero with ValuePoints = 12u }

    override self.ToString() : string =
        match self with
        | ActionCard Card.Deal3 -> "Deal3"
        | ActionCard Card.Freeze -> "Freeze"
        | ActionCard Card.SecondChance -> "SecondChance"
        | ModifierCard Card.Plus2 -> "+2"
        | ModifierCard Card.Plus4 -> "+4"
        | ModifierCard Card.Plus6 -> "+6"
        | ModifierCard Card.Plus8 -> "+8"
        | ModifierCard Card.Plus10 -> "+10"
        | ModifierCard Card.Double -> "x2"
        | ValueCard Card.Zero -> "0"
        | ValueCard Card.One -> "1"
        | ValueCard Card.Two -> "2"
        | ValueCard Card.Three -> "3"
        | ValueCard Card.Four -> "4"
        | ValueCard Card.Five -> "5"
        | ValueCard Card.Six -> "6"
        | ValueCard Card.Seven -> "7"
        | ValueCard Card.Eight -> "8"
        | ValueCard Card.Nine -> "9"
        | ValueCard Card.Ten -> "10"
        | ValueCard Card.Eleven -> "11"
        | ValueCard Card.Twelve -> "12"
