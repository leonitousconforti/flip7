namespace Flip7

type Deck = Map<Card, uint>

module public Deck =
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

    let public IsEmpty: Deck -> bool = Map.forall (fun _ count -> count = 0u)

    let public Count: Deck -> uint = Map.fold (fun acc _ count -> acc + count) 0u

    let rec internal Draw (deck: Deck) (discards: Deck) (count: uint) : Deck * Deck * Card list =
        if IsEmpty deck then
            Draw discards Empty count
        else

        assert (count > 0u)
        assert (Count deck + Count discards >= count)
        assert (IsEmpty deck |> not)

        let drawnCard =
            deck
            |> Map.toList
            |> List.collect (fun (card, count) -> List.replicate (int count) card)
            |> List.randomShuffle
            |> List.head

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

    let public Draw1 (deck: Deck) (discards: Deck) = Draw deck discards 1u
    let public Draw3 (deck: Deck) (discards: Deck) = Draw deck discards 3u

    let public pdf (deck: Deck) : Map<Card, float> =
        let totalCards = if IsEmpty deck then 1.0 else float (Count deck)
        Map.map (fun _ count -> float count / totalCards) deck

    let public cdf (deck: Deck) : Map<Card, float> =
        deck
        |> pdf
        |> Map.toList
        |> List.sortBy fst
        |> List.scan (fun acc (card, prob) -> (card, prob + snd acc)) (ValueCard Card.Zero, 0.0)
        |> List.tail
        |> Map.ofList
