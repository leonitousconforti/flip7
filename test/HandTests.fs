module HandTests

open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// Hands are newest first: the head is the card drawn last.

let private oneToSeven: Hand =
    values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]

[<Fact>]
let ``an empty hand scores nothing, has no bonus and has not busted`` () =
    Assert.Equal(0u, Hand.Score [])
    Assert.False(Hand.HasFlip7Bonus [])
    Assert.False(Hand.IsBust [])

// ---- bonus ----

[<Fact>]
let ``seven distinct values earn the 15-point flip7 bonus`` () =
    Assert.True(Hand.HasFlip7Bonus oneToSeven)
    Assert.Equal(1u + 2u + 3u + 4u + 5u + 6u + 7u + 15u, Hand.Score oneToSeven)

    // The bonus is for seven or more, so an eighth value keeps it.
    Assert.True(Hand.HasFlip7Bonus(ValueCard Card.Eight :: oneToSeven))

[<Fact>]
let ``the bonus counts distinct value cards, not actions, modifiers or repeats`` () =
    let oneToSix =
        values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]

    Assert.False(Hand.HasFlip7Bonus(ActionCard Card.Deal3 :: ModifierCard Card.Double :: oneToSix))
    Assert.False(Hand.HasFlip7Bonus(ValueCard Card.Six :: oneToSix))

// ---- Score ----

[<Fact>]
let ``Score sums value points without bonus for fewer than 7 value cards`` () =
    Assert.Equal(3u + 4u, Hand.Score(values [ Card.Three; Card.Four ]))

[<Fact>]
let ``a Double doubles the value cards but not the modifiers or the bonus`` () =
    let fivePlusFourDoubled = [ ValueCard Card.Five; ModifierCard Card.Plus4; ModifierCard Card.Double ]

    Assert.Equal(5u * 2u + 4u, Hand.Score fivePlusFourDoubled)
    Assert.Equal((1u + 2u + 3u + 4u + 5u + 6u + 7u) * 2u + 15u, Hand.Score(ModifierCard Card.Double :: oneToSeven))

// ---- IsBust ----

[<Fact>]
let ``IsBust is true when a value card appears twice`` () =
    Assert.True(Hand.IsBust(values [ Card.Five; Card.Five ]))

[<Fact>]
let ``only value cards can bust a hand`` () =
    // A deal3 parks the Freezes it flips in the target's hand, so two Freezes
    // are a repeat play can build. Two x2s are not: the deck holds one Double.
    let hand = [
        ActionCard Card.Freeze
        ActionCard Card.Freeze
        ModifierCard Card.Double
        ValueCard Card.One
        ValueCard Card.Two
    ]

    Assert.False(Hand.IsBust hand)

[<Fact>]
let ``a SecondChance cancels a duplicate wherever it sits in the hand`` () =
    // Hands are newest first, so a card's place in the list is not its place
    // in the draw. IsBust counts duplicates against second chances over the
    // whole hand, so either order cancels.
    Assert.False(Hand.IsBust [ ActionCard Card.SecondChance; ValueCard Card.Five; ValueCard Card.Five ])
    Assert.False(Hand.IsBust [ ValueCard Card.Five; ValueCard Card.Five; ActionCard Card.SecondChance ])

[<Fact>]
let ``each SecondChance cancels exactly one duplicate`` () =
    // Play never builds these hands: a hand holds one SecondChance at most, and
    // Reduce spends it on the first duplicate. They pin only the counting rule.
    let twoPairsOneSecondChance = [
        ValueCard Card.Five
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Six
        ActionCard Card.SecondChance
    ]

    let twoPairsTwoSecondChances = [
        ValueCard Card.Five
        ActionCard Card.SecondChance
        ValueCard Card.Five
        ValueCard Card.Six
        ValueCard Card.Six
        ActionCard Card.SecondChance
    ]

    Assert.True(Hand.IsBust twoPairsOneSecondChance)
    Assert.False(Hand.IsBust twoPairsTwoSecondChances)

[<Fact>]
let ``a SecondChance with nothing to cancel does not count against the hand`` () =
    // This is the common insured hand. Its spare second chance leaves more
    // second chances than duplicates, which is not a bust.
    Assert.False(Hand.IsBust [ ActionCard Card.SecondChance; ValueCard Card.Six; ValueCard Card.Five ])

// ---- Reduce ----

[<Fact>]
let ``a SecondChance cancels the duplicate just drawn and nothing else`` () =
    // The head Five was just drawn onto a hand already holding a Five and a
    // SecondChance.
    let isBust, reduced, removed =
        Hand.Reduce [
            ValueCard Card.Five
            ModifierCard Card.Plus4
            ActionCard Card.SecondChance
            ValueCard Card.Five
            ValueCard Card.Two
        ]

    Assert.False isBust
    Assert.Equal<Hand>([ ModifierCard Card.Plus4; ValueCard Card.Five; ValueCard Card.Two ], reduced)

    // The removed cards go to the discards, which keep no order.
    Assert.Equal<Hand>(List.sort [ ValueCard Card.Five; ActionCard Card.SecondChance ], List.sort removed)

[<Fact>]
let ``a duplicate nothing cancels busts the hand as it stands`` () =
    // Round scoring and the table tell a bust by Hand.IsBust on the hand Reduce
    // leaves, so a busted hand keeps its duplicate.
    let hand = values [ Card.Five; Card.Seven; Card.Five ]
    Assert.Equal((true, hand, []), Hand.Reduce hand)

// Reducing a hand takes microseconds, so the properties below run FsCheck's
// default number of cases.

[<Fact>]
let ``reducing a hand keeps every card, in the hand or among those it removes`` () =
    Prop.forAll
        (Arb.fromGen genHand)
        (fun hand ->
            let _, reduced, removed = Hand.Reduce hand
            List.sort (reduced @ removed) = List.sort hand
        )
    |> Check.QuickThrowOnFailure

// genHand runs to long hands with several duplicates and second chances, which
// is where a miscount shows, so it stays at its default size.
[<Fact>]
let ``Reduce agrees with IsBust`` () =
    Prop.forAll
        (Arb.fromGen genHand)
        (fun hand ->
            let isBust, _, _ = Hand.Reduce hand
            isBust = Hand.IsBust hand
        )
    |> Check.QuickThrowOnFailure

// Long hands nearly always hold more duplicates than second chances. So half
// the cases are hands of at most twelve cards with one card in four a
// SecondChance, where second chances often outnumber duplicates.
let private genInsuredHand: Gen<Hand> =
    Gen.frequency [ 3, genCard; 1, Gen.constant (ActionCard Card.SecondChance) ]
    |> Gen.listOf
    |> Gen.resize 12

[<Fact>]
let ``Reduce spends one second chance per duplicate and nothing else`` () =
    // A second chance with no duplicate to cancel stays in the hand.
    let insured = [ ActionCard Card.SecondChance; ValueCard Card.Five ]
    Assert.Equal((false, insured, []), Hand.Reduce insured)

    let valuesIn (hand: Hand) =
        hand |> List.filter (fun card -> card.IsValueCard)

    let secondChancesIn (hand: Hand) =
        hand |> List.filter ((=) (ActionCard Card.SecondChance))

    Prop.forAll
        (Arb.fromGen (Gen.frequency [ 1, genInsuredHand; 1, genHand ]))
        (fun hand ->
            let isBust, reduced, removed = Hand.Reduce hand
            let duplicates =
                List.length (valuesIn hand) - List.length (List.distinct (valuesIn hand))
            let spent = min duplicates (List.length (secondChancesIn hand))

            [
                Prop.label
                    "Reduce removes one SecondChance per duplicate it cancels"
                    (List.length (secondChancesIn removed) = spent)
                Prop.label
                    "Reduce removes one value card per duplicate it cancels"
                    (List.length (valuesIn removed) = spent)
                Prop.label
                    "Reduce removes nothing but second chances and value cards"
                    (List.length removed = 2 * spent)
                Prop.label
                    "every value card removed leaves a copy in the hand"
                    (valuesIn removed |> List.forall (fun card -> List.contains card reduced))
                Prop.label "the reduced hand is bust exactly when Reduce says so" (Hand.IsBust reduced = isBust)
                Prop.label
                    "a hand that survives holds no duplicate value"
                    (isBust || List.distinct (valuesIn reduced) = valuesIn reduced)
                Prop.label
                    "a hand with no duplicate is left as it is"
                    (duplicates > 0 || (reduced = hand && List.isEmpty removed))
            ]
            |> List.reduce (.&.)
        )
    |> Check.QuickThrowOnFailure
