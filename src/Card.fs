namespace Flip7

type public ActionCard =
    | Deal3
    | Freeze
    | SecondChance

type public ModifierCard =
    | Plus2
    | Plus4
    | Plus6
    | Plus8
    | Plus10
    | Double

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

type public Card =
    | ActionCard of ActionCard
    | ModifierCard of ModifierCard
    | ValueCard of ValueCard

    member public self.Value: ScoreBuckets =
        match self with
        | ValueCard Zero -> { ScoreBuckets.Zero with ValuePoints = 0u }
        | ValueCard One -> { ScoreBuckets.Zero with ValuePoints = 1u }
        | ValueCard Two -> { ScoreBuckets.Zero with ValuePoints = 2u }
        | ValueCard Three -> { ScoreBuckets.Zero with ValuePoints = 3u }
        | ValueCard Four -> { ScoreBuckets.Zero with ValuePoints = 4u }
        | ValueCard Five -> { ScoreBuckets.Zero with ValuePoints = 5u }
        | ValueCard Six -> { ScoreBuckets.Zero with ValuePoints = 6u }
        | ValueCard Seven -> { ScoreBuckets.Zero with ValuePoints = 7u }
        | ValueCard Eight -> { ScoreBuckets.Zero with ValuePoints = 8u }
        | ValueCard Nine -> { ScoreBuckets.Zero with ValuePoints = 9u }
        | ValueCard Ten -> { ScoreBuckets.Zero with ValuePoints = 10u }
        | ValueCard Eleven -> { ScoreBuckets.Zero with ValuePoints = 11u }
        | ValueCard Twelve -> { ScoreBuckets.Zero with ValuePoints = 12u }
        | ModifierCard Plus2 -> { ScoreBuckets.Zero with ModifierPoints = 2u }
        | ModifierCard Plus4 -> { ScoreBuckets.Zero with ModifierPoints = 4u }
        | ModifierCard Plus6 -> { ScoreBuckets.Zero with ModifierPoints = 6u }
        | ModifierCard Plus8 -> { ScoreBuckets.Zero with ModifierPoints = 8u }
        | ModifierCard Plus10 -> { ScoreBuckets.Zero with ModifierPoints = 10u }
        | ModifierCard Double -> { ScoreBuckets.Zero with Multiplier = 2u }
        | _ -> ScoreBuckets.Zero

    override self.ToString() : string =
        match self with
        | ValueCard Zero -> "0"
        | ValueCard One -> "1"
        | ValueCard Two -> "2"
        | ValueCard Three -> "3"
        | ValueCard Four -> "4"
        | ValueCard Five -> "5"
        | ValueCard Six -> "6"
        | ValueCard Seven -> "7"
        | ValueCard Eight -> "8"
        | ValueCard Nine -> "9"
        | ValueCard Ten -> "10"
        | ValueCard Eleven -> "11"
        | ValueCard Twelve -> "12"
        | ModifierCard Plus2 -> "Plus2"
        | ModifierCard Plus4 -> "Plus4"
        | ModifierCard Plus6 -> "Plus6"
        | ModifierCard Plus8 -> "Plus8"
        | ModifierCard Plus10 -> "Plus10"
        | ModifierCard Double -> "Double"
        | ActionCard Deal3 -> "Deal3"
        | ActionCard Freeze -> "Freeze"
        | ActionCard SecondChance -> "SecondChance"
