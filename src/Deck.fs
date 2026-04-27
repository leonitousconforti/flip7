namespace Flip7

/// <summary>
/// A deck in flip7 is a mapping from each card to the number of copies of that
/// card remaining in the deck.
/// </summary>
type Deck = Map<Card, uint>

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
    /// A random deck.
    /// </summary>
    let public Random: Deck =
        let random = System.Random()
        Map.map (fun _card maxCount -> uint (random.Next(0, int maxCount + 1))) Full

    /// <summary>
    /// Increments the count of the given card in the deck by 1.
    /// </summary>
    let public increment (deck: Deck) (card: Card) : Deck =
        Map.change card (Option.map (fun count -> count + 1u)) deck

    /// <summary>
    /// Decrements the count of the given card in the deck by 1.
    /// </summary>
    let public decrement (deck: Deck) (card: Card) : Deck =
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
    /// Draws a card from the deck, returning the new deck, the new discards,
    /// and the drawn card. If the deck is empty, the discards are shuffled and
    /// become the new deck, and the discards become empty.
    /// </summary>
    let rec internal Draw (deck: Deck) (discards: Deck) (count: uint) : Deck * Deck * Card list =
        if IsEmpty deck then
            Draw discards Empty count
        else

        assert (count > 0u)
        assert (Count deck + Count discards >= bigint count)
        assert (IsEmpty deck |> not)

        let drawnCard =
            deck
            |> Map.toArray
            |> Array.collect (fun (card, count) -> Array.replicate (int count) card)
            |> Array.randomShuffle
            |> Array.head

        let newDeck =
            Map.change
                drawnCard
                (fun count ->
                    assert (Option.isSome count)
                    assert (Option.get count > 0u)
                    Some(count.Value - 1u)
                )
                deck

        match count with
        | 1u -> newDeck, discards, [ drawnCard ]
        | _ ->
            let lastDeck, lastDiscards, lastDrawnCards = Draw newDeck discards (count - 1u)
            lastDeck, lastDiscards, drawnCard :: lastDrawnCards

    /// <summary>
    /// Draws one card from the deck, returning the new deck, the new discards,
    /// and the drawn card. If the deck is empty, the discards are shuffled and
    /// become the new deck, and the discards become empty.
    /// </summary>
    let public Draw1 (deck: Deck) (discards: Deck) = Draw deck discards 1u

    /// <summary>
    /// Draws three cards from the deck, returning the new deck, the new
    /// discards and the drawn cards. If the deck is empty, the discards are
    /// shuffled and become the new deck, and the discards become empty.
    /// </summary>
    let public Draw3 (deck: Deck) (discards: Deck) = Draw deck discards 3u

    /// <summary>
    /// The probability distribution function of the deck, which maps each card
    /// to the probability of drawing that card from the deck.
    /// </summary>
    let public pdf (deck: Deck) : Map<Card, float> =
        let totalCards = if IsEmpty deck then 1.0 else float (Count deck)
        Map.map (fun _ count -> float count / totalCards) deck

    /// <summary>
    /// The cumulative distribution function of the deck, which maps each card
    /// to the probability of drawing that card or a lower card from the deck.
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
