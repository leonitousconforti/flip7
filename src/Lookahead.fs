namespace Flip7

/// <summary>
/// What a hand is worth when it is played on as well as it can be, computed
/// rather than sampled. The deck is a known multiset, so the chance of every
/// card that could come next is known exactly and the average over them can be
/// summed instead of estimated: there is no sampling error in any of this, and
/// nothing here needs a threshold to decide whether it has seen enough.
///
/// Looking one card ahead is what MaximizesExpectedValue already does. Looking
/// further is the same question asked of the hand that card would leave, which
/// is what makes a hand worth holding onto: a fifteen worth keeping is one you
/// can still safely improve, and one card of sight cannot tell the difference.
/// </summary>
module public Lookahead =
    // Standing banks what the hand is worth, and a bust hand banks nothing
    let private Banked (hand: Hand) : float =
        if Hand.IsBust hand then
            0.0
        else
            hand |> Hand.Score |> float

    // The cards the deck could turn up, with the chance of each
    let private Chances (deck: Deck) : (Card * float) list =
        let drawable = deck |> Map.toList |> List.filter (fun (_, count) -> count > 0u)
        let total = drawable |> List.sumBy (snd >> float)

        if total = 0.0 then
            []
        else
            drawable |> List.map (fun (card, count) -> card, float count / total)

    // The deck packed into two words, four bits per card: the fullest card has
    // twelve copies, and the deck always carries a count for all twenty-one
    let private Key (depth: int) (deck: Deck) : struct (int * uint64 * uint64) =
        let folder (struct (index, low, high)) _ (count: uint) =
            if index < 16 then
                struct (index + 1, low ||| (uint64 count <<< (4 * index)), high)
            else
                struct (index + 1, low, high ||| (uint64 count <<< (4 * (index - 16))))

        let struct (_, low, high) = Map.fold folder (struct (0, 0UL, 0UL)) deck
        struct (depth, low, high)

    /// <summary>
    /// A memory of worths already computed, so that a state is only ever priced
    /// once. Draw order never matters: every way of reaching a state drew the
    /// same multiset of cards, and the deck records that multiset exactly, so
    /// the deck and the sight left to spend name the state completely.
    ///
    /// A memory may be shared between searches, including from different
    /// threads, but only when they price hands against decks missing exactly
    /// those hands from the same full deck: then the missing cards name the
    /// hand, and worths carry across. Searches rooted in different universes
    /// (say, mid-game decks with discards) must not share one, because equal
    /// decks would no longer mean equal hands.
    /// </summary>
    type public Memory = System.Collections.Concurrent.ConcurrentDictionary<struct (int * uint64 * uint64), float>

    /// <summary>
    /// A fresh, empty memory. Create one at the edge of the program and pass it
    /// to the With family of searches to share what they learn.
    /// </summary>
    let public NewMemory () : Memory = Memory()

    /// <summary>
    /// What playing on is worth: the average over every card that could come,
    /// each one played out as well as it can be from there. A bust banks
    /// nothing, and flipping seven ends the round with the bonus already in the
    /// score.
    /// </summary>
    let rec public AfterHittingWith (memory: Memory) (depth: int) (deck: Deck) (hand: Hand) : float =
        match Chances deck with
        | [] -> Banked hand
        | chances ->
            chances
            |> List.sumBy (fun (card, chance) ->
                let isBust, reduced, _ = Hand.Reduce(card :: hand)

                let worth =
                    if isBust then
                        0.0
                    else
                        WorthWith memory (depth - 1) (Deck.Decrement deck card) reduced

                chance * worth
            )

    /// <summary>
    /// What a hand is worth held: the better of banking it now and playing on,
    /// as far ahead as the depth allows. A hand that has flipped seven is worth
    /// banking whatever the depth, because the round ends there.
    /// </summary>
    and public WorthWith (memory: Memory) (depth: int) (deck: Deck) (hand: Hand) : float =
        let banked = Banked hand

        if depth <= 0 || Hand.HasFlip7Bonus hand then
            banked
        else
            let key = Key depth deck

            match memory.TryGetValue key with
            | true, worth -> worth
            | false, _ ->
                let worth = max banked (AfterHittingWith memory depth deck hand)
                memory[key] <- worth
                worth

    /// <summary>
    /// What hitting is worth over standing, in points of expected round score.
    /// Positive says hit. Looking one card ahead this is exactly the question
    /// MaximizesExpectedValue asks, so any greater depth knows strictly more
    /// than it does.
    /// </summary>
    let public GainFromHittingWith (memory: Memory) (depth: int) (deck: Deck) (hand: Hand) : float =
        if Hand.HasFlip7Bonus hand then
            0.0
        else
            AfterHittingWith memory depth deck hand - Banked hand

    /// <summary>
    /// AfterHittingWith against a memory of its own, forgotten when it returns.
    /// </summary>
    let public AfterHitting (depth: int) (deck: Deck) (hand: Hand) : float =
        AfterHittingWith (NewMemory()) depth deck hand

    /// <summary>
    /// WorthWith against a memory of its own, forgotten when it returns.
    /// </summary>
    let public Worth (depth: int) (deck: Deck) (hand: Hand) : float = WorthWith (NewMemory()) depth deck hand

    /// <summary>
    /// GainFromHittingWith against a memory of its own, forgotten when it
    /// returns.
    /// </summary>
    let public GainFromHitting (depth: int) (deck: Deck) (hand: Hand) : float =
        GainFromHittingWith (NewMemory()) depth deck hand
