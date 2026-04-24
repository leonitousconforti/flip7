namespace Flip7

module Simulation =
    type public Player = string * Strategy * Hand

    let public assertIsValid (deck: Deck) (discards: Deck) (hands: Hand list) : unit =
        let handsToDeck =
            hands |> List.collect (fun hand -> hand) |> List.countBy id |> Map.ofList

        Deck.Full
        |> Map.iter (fun card expected ->
            let deckCount = deck |> Map.find card
            System.Diagnostics.Debug.Assert(
                deckCount >= 0u,
                $"Deck cannot have negative counts for any card, found {deckCount} of {card}"
            )
            System.Diagnostics.Debug.Assert(
                deckCount <= expected,
                $"Deck cannot have more {card} than the full count of that card, found {deckCount} of {card}"
            )

            let discardsCount = discards |> Map.find card
            System.Diagnostics.Debug.Assert(
                discardsCount >= 0u,
                $"Discards cannot have negative counts for any card, found {discardsCount} of {card}"
            )
            System.Diagnostics.Debug.Assert(
                discardsCount <= expected,
                $"Discards cannot have more {card} than the full count of that card, found {discardsCount} of {card}"
            )

            let handCount = handsToDeck |> Map.tryFind card |> Option.defaultValue 0 |> uint
            System.Diagnostics.Debug.Assert(
                handCount >= 0u,
                $"Hands cannot have negative counts for any card, found {handCount} of {card}"
            )
            System.Diagnostics.Debug.Assert(
                handCount <= expected,
                $"Hands cannot have more {card} than the full count of that card, found {handCount} of {card}"
            )

            let actual = deckCount + discardsCount + handCount
            System.Diagnostics.Debug.Assert(
                (actual = expected),
                $"Card count mismatch for {card}: expected {expected}, found {actual}"
            )
        )

    let rec public GoonSession
        (inHands: Player list)
        (doneHands: Player list)
        (deck: Deck)
        (discards: Deck)
        : Player list * Deck * Deck =
        // Base case: if everyone is done gooning (have stood or busted)
        if List.isEmpty inHands then
            doneHands, deck, discards
        else

        // Invariant: no one in inHands has busted
        assert
            inHands
            |> List.map (fun (_name, _strategy, hand) -> hand)
            |> List.exists Hand.IsBust
            |> not

        // Base case: if anyone has the Flip7 bonus, they win immediately and
        // everyone else is done gooning
        if
            inHands
            |> List.map (fun (_name, _strategy, hand) -> hand)
            |> List.exists Hand.HasFlip7Bonus
        then
            inHands @ doneHands, deck, discards
        else

        // We just care about the first player right now, recursion will handle
        // the rest
        let currentIn = List.head inHands
        let othersIn = List.tail inHands
        let name, strategy, hand = currentIn

        // Resolve the current player's turn
        match strategy 0u hand (othersIn |> List.map (fun (n, s, h) -> h)) deck with
        | Strategy.Stand -> GoonSession othersIn (currentIn :: doneHands) deck discards
        | Strategy.Hit ->
            let newDeck, newDiscards, newCard = Deck.Draw1 deck discards
            let newPlayer = (name, strategy, newCard @ hand)
            GoonSession (newPlayer :: othersIn) doneHands newDeck newDiscards
