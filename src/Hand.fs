namespace Flip7

/// <summary>
/// A hand in flip7 is merely a list of cards.
/// </summary>
type public Hand = Card list

module public Hand =
    /// <summary>
    /// The number of distinct value cards in a hand; modifier and action
    /// cards do not count.
    /// </summary>
    let public UniqueValueCards: Hand -> int =
        List.filter (fun card -> card.IsValueCard) >> List.distinct >> List.length

    /// <summary>
    /// You are awarded the flip7 bonus if you have 7 or more distinct value
    /// cards in your hand.
    /// </summary>
    let public HasFlip7Bonus: Hand -> bool = UniqueValueCards >> (<=) 7

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
    /// A hand is a bust if it contains duplicate value cards that are not
    /// canceled out by SecondChance cards. The Reduce function can be used to
    /// determine if a hand is a bust and to produce a reduced hand with the
    /// duplicates and SecondChance cards removed.
    /// </summary>
    let public IsBust (hand: Hand) : bool =
        let rec IsBust' (hand: Hand) (seen: Set<Card.ValueCard>) (busts: int) : bool =
            match hand with
            | [] -> busts > 0
            | ActionCard(Card.SecondChance) :: tail -> IsBust' tail seen (busts - 1)
            | ActionCard(_) :: tail -> IsBust' tail seen busts
            | ModifierCard(_) :: tail -> IsBust' tail seen busts
            | ValueCard(vc) :: tail ->
                let seen' = Set.add vc seen
                let busts' = busts + if Set.contains vc seen then 1 else 0
                IsBust' tail seen' busts'

        in
        IsBust' hand Set.empty 0

    /// <summary>
    /// The Reduce function takes a hand and produces a reduced hand with
    /// duplicate value cards and SecondChance cards removed. It also returns a
    /// boolean indicating whether the reduction still results in a bust (i.e.,
    /// there were more duplicate value cards than SecondChance cards).
    /// </summary>
    let public Reduce (hand: Hand) : bool * Hand =
        // The first fold counts the number of duplicate value cards and the
        // number of SecondChance cards in the hand.
        let folder =
            fun (seen, dups, scs) card ->
                match card with
                | ActionCard(Card.SecondChance) -> seen, dups, scs + 1
                | ValueCard(vc: Card.ValueCard) when Set.contains vc seen -> seen, dups + 1, scs
                | ValueCard(vc: Card.ValueCard) -> Set.add vc seen, dups, scs
                | _ -> seen, dups, scs

        // The second fold builds the reduced hand by skipping over the
        // appropriate number of duplicate value cards and SecondChance cards.
        let backFolder =
            fun card (seen, dupsToDrop, scsToDrop, acc) ->
                match card with
                | ActionCard(Card.SecondChance) when scsToDrop > 0 -> seen, dupsToDrop, scsToDrop - 1, acc
                | ValueCard(vc: Card.ValueCard) when Set.contains vc seen && dupsToDrop > 0 -> seen, dupsToDrop - 1, scsToDrop, acc
                | ValueCard(vc: Card.ValueCard) -> Set.add vc seen, dupsToDrop, scsToDrop, card :: acc
                | _ -> seen, dupsToDrop, scsToDrop, card :: acc

        let numDups, numSCs =
            hand
            |> List.fold folder (Set.empty, 0, 0)
            |> fun (_seen, dups, scs) -> dups, scs

        let isBust = numDups > numSCs
        let numCancel = min numDups numSCs

        let reducedHand =
            (Set.empty, numCancel, numCancel, List.empty)
            |> List.foldBack backFolder hand
            |> fun (_seen, _dupsToDrop, _scsToDrop, reducedHand) -> reducedHand

        isBust, reducedHand

    /// <summary>
    /// Parses a hand from a sequence of lines, where each line represents a
    /// card.
    /// </summary>
    let public Deserialize: (string seq) -> Hand = Seq.map Card.Parse >> Seq.toList

    /// <summary>
    /// Tries to parse a hand from a sequence of lines, where each line
    /// represents a card.
    /// </summary>
    let public TryDeserialize (lines: string seq) : Hand option =
        try
            Some(Deserialize lines)
        with :? System.FormatException ->
            None

    /// <summary>
    /// Converts a hand to an array of lines, where each line represents a card.
    /// </summary>
    let public Serialize: (Hand) -> string array =
        List.toArray >> Array.map (fun card -> $"{card}")
