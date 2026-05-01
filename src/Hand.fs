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
                let newSeen = Set.add vc seen
                let newBusts = busts + if Set.contains vc seen then 1 else 0
                IsBust' tail newSeen newBusts

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
                | ValueCard(vc) when Set.contains vc seen -> seen, dups + 1, scs
                | ValueCard(vc) -> Set.add vc seen, dups, scs
                | _ -> seen, dups, scs

        // The second fold builds the reduced hand by skipping over the
        // appropriate number of duplicate value cards and SecondChance cards.
        let backFolder =
            fun card (seen, dupsToDrop, scsToDrop, acc) ->
                match card with
                | ActionCard(Card.SecondChance) when scsToDrop > 0 -> seen, dupsToDrop, scsToDrop - 1, acc
                | ValueCard(vc) when Set.contains vc seen && dupsToDrop > 0 -> seen, dupsToDrop - 1, scsToDrop, acc
                | ValueCard(vc) -> Set.add vc seen, dupsToDrop, scsToDrop, card :: acc
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
    /// Writes a hand to a file with the given name in the current directory.
    /// The file will contain one line per card.
    /// </summary>
    let public Write (name: string) : Hand -> unit =
        let currentDirectory = System.IO.Directory.GetCurrentDirectory()
        let path = System.IO.Path.Combine(currentDirectory, name)
        let write = fun lines -> System.IO.File.WriteAllLines(path, lines)
        List.toArray >> Array.map (fun card -> $"{card}") >> write

    /// <summary>
    /// Reads a hand from a file with the given name in the current directory.
    /// The file should contain one line per card.
    /// </summary>
    let public Read (name: string) : Hand =
        let currentDirectory = System.IO.Directory.GetCurrentDirectory()
        let path = System.IO.Path.Combine(currentDirectory, name)
        let lines = System.IO.File.ReadLines path
        lines |> Seq.map Card.Parse |> Seq.toList
