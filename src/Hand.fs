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
        let rec IsBust' (hand: Hand) (seen: Set<Card.ValueCard>) (busts: int) : bool =
            match hand with
            | [] -> busts > 0
            | ActionCard(Card.SecondChance) :: tail -> IsBust' tail seen (busts - 1)
            | ActionCard(_) :: tail -> IsBust' tail seen busts
            | ModifierCard(_) :: tail -> IsBust' tail seen busts
            | ValueCard(vc) :: tail ->
                let newSeen = Set.add vc seen
                let newBusts = busts + if Set.contains vc seen then 1 else 0
                IsBust' tail newSeen newBusts

        in
        IsBust' hand Set.empty 0

    let public Reduce (hand: Hand) : bool * Hand =

        hand |> List.back

        let rec Reduce
            (hand: Hand)
            (newHand: Hand)
            (seen: Set<Card.ValueCard>)
            (secondChances: int)
            (busts: int)
            : bool * Hand =
            match hand with
            | [] -> busts - secondChances > 0, newHand
            | ActionCard(Card.SecondChance) :: tail -> false
            | ActionCard(_) as c :: tail -> Reduce tail (c :: newHand) seen busts
            | ModifierCard(_) as c :: tail -> Reduce tail (c :: newHand) seen busts
            | ValueCard(vc) as c :: tail ->
                let newSeen = Set.add vc seen
                let newBusts = busts + if Set.contains vc seen then 1 else 0
                Reduce tail (c :: newHand) newSeen newBusts

        in
        Reduce hand List.empty Set.empty 0
