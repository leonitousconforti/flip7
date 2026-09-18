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
        if Hand.IsBust hand then 0.0 else float (Hand.Score hand)

    // The cards the deck could turn up, with the chance of each
    let private Chances (deck: Deck) : (Card * float) list =
        let drawable = deck |> Map.toList |> List.filter (fun (_, count) -> count > 0u)
        let total = drawable |> List.sumBy (snd >> float)

        if total = 0.0 then
            []
        else
            drawable |> List.map (fun (card, count) -> card, float count / total)

    /// <summary>
    /// What playing on is worth: the average over every card that could come,
    /// each one played out as well as it can be from there. A bust banks
    /// nothing, and flipping seven ends the round with the bonus already in
    /// the score.
    /// </summary>
    let rec public AfterHitting (depth: int) (deck: Deck) (hand: Hand) : float =
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
                        Worth (depth - 1) (Deck.Decrement deck card) reduced

                chance * worth
            )

    /// <summary>
    /// What a hand is worth held: the better of banking it now and playing on,
    /// as far ahead as the depth allows. A hand that has flipped seven is worth
    /// banking whatever the depth, because the round ends there.
    /// </summary>
    and public Worth (depth: int) (deck: Deck) (hand: Hand) : float =
        let banked = Banked hand

        if depth <= 0 || Hand.HasFlip7Bonus hand then
            banked
        else
            max banked (AfterHitting depth deck hand)

    /// <summary>
    /// What hitting is worth over standing, in points of expected round score.
    /// Positive says hit. Looking one card ahead this is exactly the question
    /// MaximizesExpectedValue asks, so any greater depth knows strictly more
    /// than it does.
    /// </summary>
    let public GainFromHitting (depth: int) (deck: Deck) (hand: Hand) : float =
        // Seven distinct cards ends the round where it stands, so there is no
        // draw left to price
        if Hand.HasFlip7Bonus hand then
            0.0
        else
            AfterHitting depth deck hand - Banked hand
