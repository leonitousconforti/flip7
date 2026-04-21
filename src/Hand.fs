namespace Flip7

type public Hand = Card list

module public Hand =
    let public HasFlip7Bonus: Hand -> bool =
        List.filter (fun card -> card.IsValueCard) >> List.length >> (>=) 7

    let public Score (hand: Hand) : uint =
        let maybeBonusPoints = {
            ScoreBuckets.Zero with
                BonusPoints = if HasFlip7Bonus hand then 15u else 0u
        }

        hand
        |> List.map (fun card -> card.Value)
        |> List.fold (+) ScoreBuckets.Zero
        |> (+) maybeBonusPoints
        |> ScoreBuckets.Total

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
