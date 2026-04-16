namespace Flip7

type public Hand = Card list

module public Hand =
    let public IsBust (hand: Hand) : bool =
        let rec isBust (hand: Hand) (seen: Set<Card.ValueCard>) : bool =
            match hand with
            | [] -> false
            | ActionCard(Card.SecondChance) :: tail -> false
            | ActionCard(_) :: tail -> isBust tail seen
            | ModifierCard(_) :: tail -> isBust tail seen
            | ValueCard(vc) :: tail when Set.contains vc seen -> true
            | ValueCard(vc) :: tail -> isBust tail (Set.add vc seen)

        in
        isBust hand Set.empty

    let public HasFlip7Bonus: Hand -> bool =
        List.filter (fun card -> card.IsValueCard) >> List.length >> (>=) 7

    let public Score (hand: Hand) : uint =
        let maybeBonusPoints =
            if HasFlip7Bonus hand then
                { ScoreBuckets.Zero with BonusPoints = 15u }
            else
                ScoreBuckets.Zero

        hand
        |> List.map (fun card -> card.Value)
        |> List.fold (+) ScoreBuckets.Zero
        |> (+) maybeBonusPoints
        |> ScoreBuckets.Total
