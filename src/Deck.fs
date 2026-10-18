namespace Flip7

/// <summary>
/// A deck in flip7 is a mapping from each card to the number of copies of that
/// card remaining in the deck.
/// </summary>
type public Deck = Map<Card, uint>

module public Deck =
    /// <summary>
    /// The empty deck.
    /// </summary>
    let public Empty: Deck =
        Map.ofList [
            ValueCard Card.Zero, 0u
            ValueCard Card.One, 0u
            ValueCard Card.Two, 0u
            ValueCard Card.Three, 0u
            ValueCard Card.Four, 0u
            ValueCard Card.Five, 0u
            ValueCard Card.Six, 0u
            ValueCard Card.Seven, 0u
            ValueCard Card.Eight, 0u
            ValueCard Card.Nine, 0u
            ValueCard Card.Ten, 0u
            ValueCard Card.Eleven, 0u
            ValueCard Card.Twelve, 0u
            ModifierCard Card.Plus2, 0u
            ModifierCard Card.Plus4, 0u
            ModifierCard Card.Plus6, 0u
            ModifierCard Card.Plus8, 0u
            ModifierCard Card.Plus10, 0u
            ModifierCard Card.Double, 0u
            ActionCard Card.Deal3, 0u
            ActionCard Card.Freeze, 0u
            ActionCard Card.SecondChance, 0u
        ]

    /// <summary>
    /// The full deck.
    /// </summary>
    let public Full: Deck =
        Map.ofList [
            ValueCard Card.Zero, 1u
            ValueCard Card.One, 1u
            ValueCard Card.Two, 2u
            ValueCard Card.Three, 3u
            ValueCard Card.Four, 4u
            ValueCard Card.Five, 5u
            ValueCard Card.Six, 6u
            ValueCard Card.Seven, 7u
            ValueCard Card.Eight, 8u
            ValueCard Card.Nine, 9u
            ValueCard Card.Ten, 10u
            ValueCard Card.Eleven, 11u
            ValueCard Card.Twelve, 12u
            ModifierCard Card.Plus2, 1u
            ModifierCard Card.Plus4, 1u
            ModifierCard Card.Plus6, 1u
            ModifierCard Card.Plus8, 1u
            ModifierCard Card.Plus10, 1u
            ModifierCard Card.Double, 1u
            ActionCard Card.Deal3, 3u
            ActionCard Card.Freeze, 3u
            ActionCard Card.SecondChance, 3u
        ]

    /// <summary>
    /// A random deck with the corresponding discards.
    /// </summary>
    let public RandomWith (random: System.Random) : Deck * Deck =
        Map.fold
            (fun (deck, discards) card maxCount ->
                let count = uint (random.Next(0, int maxCount + 1))
                Map.add card count deck, Map.add card (maxCount - count) discards
            )
            (Empty, Empty)
            Full

#if !FABLE_COMPILER
    /// <summary>
    /// A random deck with the corresponding discards.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Deck.RandomWith instead")>]
    let public Random: Deck * Deck = RandomWith System.Random.Shared
#endif

    /// <summary>
    /// Parses a deck from the lines Serialize writes: one "Card: Count" line
    /// per card, every card once. Raises a FormatException naming the problem
    /// for anything else: the wrong number of lines, a null line, a line of
    /// another form, an unknown card, a count that is not a uint in plain
    /// digits, a card listed twice, or more copies of a card than the full deck
    /// holds.
    /// </summary>
    let public Deserialize (lines: string seq) : Deck =
        let lines = Seq.toArray lines

        if lines.Length <> Map.count Full then
            raise (
                System.FormatException
                    $"Invalid deck: expected {Map.count Full} lines, one per card, but got {lines.Length}"
            )

        // With one line per card, a card listed twice leaves another one
        // missing, so no card can go uncounted once every line has been read
        lines
        |> Array.fold
            (fun (deck: Deck) line ->
                if isNull line then
                    raise (System.FormatException "Invalid deck: a line is null")

                let card, count =
                    match line.Split ": " with
                    | [| cardStr; countStr |] ->
                        match Card.TryParse cardStr, System.UInt32.TryParse countStr with
                        | None, _ ->
                            raise (System.FormatException $"Invalid deck line '{line}': unknown card '{cardStr}'")
                        // Comparing with the count written back rejects the
                        // signs, spaces and leading zeros UInt32 also reads
                        | Some card, (true, count) when string count = countStr -> card, count
                        | Some _, _ ->
                            raise (
                                System.FormatException
                                    $"Invalid deck line '{line}': the count '{countStr}' is not a uint in plain digits"
                            )
                    | _ -> raise (System.FormatException $"Invalid deck line '{line}': expected 'Card: Count'")

                let copies = Map.find card Full

                if Map.containsKey card deck then
                    raise (System.FormatException $"Invalid deck: {card} is listed twice, so another card is missing")
                elif count > copies then
                    raise (
                        System.FormatException
                            $"Invalid deck line '{line}': {count} copies of {card} is more than the {copies} in a full deck"
                    )
                else
                    Map.add card count deck
            )
            Map.empty

    /// <summary>
    /// Tries to parse a deck from the lines Serialize writes: one "Card: Count"
    /// line per card, every card once. Returns None for anything Deserialize
    /// rejects rather than raising.
    /// </summary>
    let public TryDeserialize (lines: string seq) : Deck option =
        try
            Some(Deserialize lines)
        with :? System.FormatException ->
            None

    /// <summary>
    /// Converts a deck to an array of lines, where each line is in the format
    /// "Card: Count".
    /// </summary>
    let public Serialize: Deck -> string array =
        Map.toArray >> Array.map (fun (card, count) -> $"{card}: {count}")

    /// <summary>
    /// Increments the count of the given card in the deck by 1.
    /// </summary>
    let public Increment (deck: Deck) (card: Card) : Deck =
        Map.change card (Option.map (fun count -> count + 1u)) deck

    /// <summary>
    /// Decrements the count of the given card in the deck by 1.
    /// </summary>
    let public Decrement (deck: Deck) (card: Card) : Deck =
        Map.change card (Option.map (fun count -> count - 1u)) deck

    /// <summary>
    /// A deck is empty if it contains zero copies of every card.
    /// </summary>
    let public IsEmpty: Deck -> bool = Map.forall (fun _ count -> count = 0u)

    /// <summary>
    /// Counts the total number of cards in the deck.
    /// </summary>
    let public Count: Deck -> bigint =
        Map.fold (fun acc _ count -> acc + bigint count) 0I

    /// <summary>
    /// Draws a card from the deck using the given source of randomness,
    /// returning the new deck, the new discards, and the drawn card. If the
    /// deck is empty, the discards are shuffled and become the new deck, and
    /// the discards become empty. Raises an InvalidOperationException if the
    /// deck and the discards run out before every card is drawn.
    /// </summary>
    let rec internal DrawWith (random: System.Random) (decks: Deck * Deck) (count: uint) : (Deck * Deck) * Card list =
        let deck, discards = decks

        // Two empty piles are refused rather than shuffled into each other
        // forever. A draw of more cards than the piles hold ends up here too
        if IsEmpty deck && IsEmpty discards then
            raise (System.InvalidOperationException "Cannot draw a card: the deck and the discards are both empty")

        if IsEmpty deck then
            DrawWith random (discards, Empty) count
        else

        assert (count > 0u)
        assert (IsEmpty deck |> not)

        let drawnCard =
            deck
            |> Map.toArray
            |> Array.collect (fun (card, count) -> Array.replicate (int count) card)
            |> Array.randomShuffleWith random
            |> Array.head

        let deck' =
            Map.change
                drawnCard
                (fun count ->
                    assert (Option.isSome count)
                    assert (Option.get count > 0u)
                    Some(count.Value - 1u)
                )
                deck

        match count with
        | 1u -> (deck', discards), [ drawnCard ]
        | _ ->
            let lastDecks, lastDrawnCards = DrawWith random (deck', discards) (count - 1u)
            lastDecks, drawnCard :: lastDrawnCards

#if !FABLE_COMPILER
    /// <summary>
    /// Draws a card from the deck, returning the new deck, the new discards,
    /// and the drawn card. If the deck is empty, the discards are shuffled and
    /// become the new deck, and the discards become empty.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Deck.DrawWith instead")>]
    let internal Draw (decks: Deck * Deck) (count: uint) : (Deck * Deck) * Card list =
        DrawWith System.Random.Shared decks count
#endif

    /// <summary>
    /// Draws one card from the deck using the given source of randomness,
    /// returning the new deck, the new discards, and the drawn card. If the
    /// deck is empty, the discards are shuffled and become the new deck, and
    /// the discards become empty. Raises an InvalidOperationException if the
    /// deck and the discards are both empty.
    /// </summary>
    let public Draw1With (random: System.Random) (decks: Deck * Deck) =
        DrawWith random decks 1u |> fun (decks, cards) -> decks, cards.Head

#if !FABLE_COMPILER
    /// <summary>
    /// Draws one card from the deck, returning the new deck, the new discards,
    /// and the drawn card. If the deck is empty, the discards are shuffled and
    /// become the new deck, and the discards become empty.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Deck.Draw1With instead")>]
    let public Draw1 (decks: Deck * Deck) = Draw1With System.Random.Shared decks
#endif

    /// <summary>
    /// Draws three cards from the deck using the given source of randomness,
    /// returning the new deck, the new discards and the drawn cards. If the
    /// deck is empty, the discards are shuffled and become the new deck, and
    /// the discards become empty. Raises an InvalidOperationException if the
    /// deck and the discards run out before every card is drawn.
    /// </summary>
    let public Draw3With (random: System.Random) (decks: Deck * Deck) = DrawWith random decks 3u

#if !FABLE_COMPILER
    /// <summary>
    /// Draws three cards from the deck, returning the new deck, the new
    /// discards and the drawn cards. If the deck is empty, the discards are
    /// shuffled and become the new deck, and the discards become empty.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Deck.Draw3With instead")>]
    let public Draw3 (decks: Deck * Deck) = Draw3With System.Random.Shared decks
#endif

    /// <summary>
    /// The probability distribution function of the deck, which maps each card
    /// to the probability of drawing that card from the deck.
    /// </summary>
    let public pdf (deck: Deck) : Map<Card, float> =
        let totalCards = if IsEmpty deck then 1.0 else float (Count deck)
        Map.map (fun _ count -> float count / totalCards) deck

    /// <summary>
    /// The cumulative distribution function of the deck, which maps each card
    /// to the probability of drawing that card or one ordered before it from
    /// the deck. The order is Card's structural order: the actions, then the
    /// modifiers, then the values.
    /// </summary>
    let public cdf (deck: Deck) : Map<Card, float> =
        deck
        |> pdf
        |> Map.toArray
        |> Array.sortBy fst
        |> Array.scan (fun acc (card, prob) -> (card, prob + snd acc)) (ValueCard Card.Zero, 0.0)
        |> Array.tail
        |> Map.ofArray

    /// <summary>
    /// The expected value of points you would get from drawing a card from the
    /// deck, if you were to draw a card at random from the deck.
    /// </summary>
    let public ev: Deck -> float =
        pdf
        >> Map.toArray
        >> Array.sumBy (fun (card, prob) ->
            let score = card.Value |> ScoreBuckets.Total
            float prob * float score
        )

    /// <summary>
    /// The card you would expect to draw from the deck, if you were to draw a
    /// card at random from the deck.
    /// </summary>
    let public ec: Deck -> Card = pdf >> Map.toArray >> Array.maxBy snd >> fst

    /// <summary>
    /// The variance of the points you would get from drawing a card from the
    /// deck, if you were to draw a card at random from the deck.
    /// </summary>
    let public var (deck: Deck) : float =
        let expectedValue = ev deck

        deck
        |> pdf
        |> Map.toArray
        |> Array.sumBy (fun (card, prob) ->
            let score = card.Value |> ScoreBuckets.Total
            prob * (float score - expectedValue) ** 2.0
        )

    /// <summary>
    /// The standard deviation of the points you would get from drawing a card
    /// from the deck, if you were to draw a card at random from the deck.
    /// </summary>
    let public std: Deck -> float = var >> sqrt
