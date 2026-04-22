namespace Flip7

/// <summary>
/// A hand in flip7 is merely a list of cards.
/// </summary>
type public Hand = Card list

module public Hand =
    /// <summary>
    /// You are awarded the flip7 bonus if you have 7 or more distinct value
    /// cards in your hand.
    /// </summary>
    let public HasFlip7Bonus: Hand -> bool =
        List.filter (fun card -> card.IsValueCard)
        >> List.distinct
        >> List.length
        >> (<=) 7

    /// <summary>
    /// The score of a hand can be calculated using the ScoreBuckets of each
    /// card in the hand. Since addition and subtraction are defined for
    /// ScoreBuckets, it is a simple sum.
    /// </summary>
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

    /// <summary>
    /// A hand is a bust if it contains any duplicate value cards and doesn't
    /// have the SecondChance action card.
    /// </summary>
    let public IsBust (hand: Hand) : bool =
        let rec isBust (hand: Hand) (seen: Set<Card.ValueCard>) (busted: bool) : bool =
            match hand with
            | [] -> busted
            | ActionCard(Card.SecondChance) :: tail -> false
            | ActionCard(_) :: tail -> isBust tail seen busted
            | ModifierCard(_) :: tail -> isBust tail seen busted
            | ValueCard(vc) :: tail ->
                let newSeen = Set.add vc seen
                let newBusted = busted || Set.contains vc seen
                isBust tail newSeen newBusted

        in
        isBust hand Set.empty false
