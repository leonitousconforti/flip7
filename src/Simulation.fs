namespace Flip7

module Simulation =
    type public Player = string * Strategy * Hand

    let rec public probabilityToBust (deck: Deck) (discards: Deck) (hand: Hand) (onlyPlayer: bool) : float =
        let pdf = Deck.pdf deck
        let probabilityOfDuplicateValueCard =
            hand
            |> List.filter (fun card -> card.IsValueCard)
            |> List.map (fun card -> Map.find card pdf)
            |> List.sum

        // Simple case: there are other players whom haven't stood or busted
        // yet so we don't consider the case of needing to play action cards
        // on ourselves
        if not onlyPlayer then
            probabilityOfDuplicateValueCard
        else

        // We must play these on ourselves as we are the only player left
        let probabilityOfDeal3 = Map.find (ActionCard Card.Deal3) pdf
        let probabilityOfFreeze = Map.find (ActionCard Card.Freeze) pdf

        // Base case: the probability of drawing a deal3 card is 0%
        if probabilityOfDeal3 = 0.0 then
            probabilityOfDuplicateValueCard + probabilityOfFreeze
        else

        let deck' = Deck.decrement deck (ActionCard Card.Deal3)
        let cardsDrawnBeforeReshuffling = min (Deck.Count deck' |> uint) 3u
        let cardsDrawnAfterReshuffling = 3u - cardsDrawnBeforeReshuffling

        let probabilityToBustBeforeReshuffling =
            probabilityToBust deck' discards hand onlyPlayer
            * float cardsDrawnBeforeReshuffling

        let probabilityToBustAfterReshuffling =
            probabilityToBust discards Deck.Empty hand onlyPlayer
            * float cardsDrawnAfterReshuffling

        let probabilityToBust' =
            probabilityToBustBeforeReshuffling + probabilityToBustAfterReshuffling

        probabilityOfDeal3 * min probabilityToBust' 1.0
        + probabilityOfDuplicateValueCard
        + probabilityOfFreeze

    let public IsValid (deck: Deck) (discards: Deck) (hands: Hand seq) : string seq =
        let handsToDeck =
            hands
            |> Seq.collect id
            |> Seq.countBy id
            |> Map.ofSeq
            |> Map.map (fun _ count -> uint count)

        Deck.Full
        |> Map.toSeq
        |> Seq.collect (fun (card, expected) ->
            let deckCount = deck |> Map.find card
            let discardsCount = discards |> Map.find card
            let handCount = handsToDeck |> Map.tryFind card |> Option.defaultValue 0u
            let cannot = $"cannot have more {card} than the full count of that card"
            let actual = deckCount + discardsCount + handCount

            seq {
                if deckCount > expected then
                    yield $"Deck {cannot}, found {deckCount} of {card}"
                if discardsCount > expected then
                    yield $"Discards {cannot}, found {discardsCount} of {card}"
                if handCount > expected then
                    yield $"Hands {cannot}, found {handCount} of {card}"
                if actual <> expected then
                    yield $"Card count mismatch for {card}: expected {expected}, found {actual}"
            }
        )

    let public AssertIsValid (deck: Deck) (discards: Deck) (hands: Hand seq) : unit =
        let errors = IsValid deck discards hands |> Seq.toList
        if errors <> [] then failwith errors.Head else ()

    let rec public GoonSession
        (players: list<string * Strategy * Hand>)
        (deck: Deck)
        (discards: Deck)
        : seq<(string * Hand) * Deck * Deck> =
        // Base case: if everyone is done gooning (have stood or busted)
        if players |> List.isEmpty then
            Seq.empty
        else

        // Invariant: should not have busted yet
        assert
            players
            |> List.map (fun (_name, _strategy, hand) -> hand)
            |> List.forall (not << Hand.IsBust)

        // Base case: if anyone has the Flip7 bonus, they win immediately and
        // everyone else is done gooning
        if
            players
            |> List.map (fun (_name, _strategy, hand) -> hand)
            |> List.exists Hand.HasFlip7Bonus
        then
            Seq.empty
        else

        #nowarn "FS25"
        let current :: others = players
        let name, strategy, hand = current
        let othersHands = others |> List.map (fun (_name, _strategy, hand) -> hand)
        let newDeck, newDiscards, newCard = Deck.Draw1 deck discards
        let hitOrStand = strategy 0u hand othersHands deck
        #warnon "FS25"

        seq {
            match hitOrStand with
            | Strategy.Stand ->
                yield (name, hand), deck, discards
                yield! GoonSession others deck discards

            | Strategy.Hit ->
                match newCard with
                // Can never bust on a modifier card, so easy just add it to the
                // player's hand and keep going
                | ModifierCard _ ->
                    let newPlayer = [ (name, strategy, newCard :: hand) ]
                    yield newPlayer.Head |> fun (n, s, h) -> (n, h), newDeck, newDiscards
                    yield! GoonSession (others @ newPlayer) newDeck newDiscards

                // Can never bust on a second chance card, but you also can't hold
                // two of them at the same time even when you are the last player
                | ActionCard Card.SecondChance when
                    List.exists (fun c -> c = ActionCard Card.SecondChance) hand
                    && others |> List.isEmpty
                    ->
                    let newNewDiscards = Deck.increment newDiscards (ActionCard Card.SecondChance)
                    yield (name, hand), newDeck, newNewDiscards
                    yield! GoonSession others newDeck newNewDiscards

                // Can never bust on a second chance card, but you also can't hold
                // two of them at the same time, must give it to someone else
                | ActionCard Card.SecondChance when
                    List.exists (fun c -> c = ActionCard Card.SecondChance) hand
                    && others |> List.isEmpty |> not
                    ->
                    let index, targetPlayer = others |> List.indexed |> List.randomChoice
                    let newTargetPlayer = targetPlayer |> fun (n, s, h) -> n, s, newCard :: h
                    let newOthers = others |> List.updateAt index newTargetPlayer
                    yield newTargetPlayer |> fun (n, s, h) -> (n, h), newDeck, newDiscards
                    yield! GoonSession (newOthers @ [ current ]) newDeck newDiscards

                // Can never bust on a second chance card, so easy just just add it to the
                // player's hand and keep going
                | ActionCard Card.SecondChance ->
                    let newPlayer = [ (name, strategy, newCard :: hand) ]
                    yield newPlayer.Head |> fun (n, s, h) -> (n, h), newDeck, newDiscards
                    yield! GoonSession (others @ newPlayer) newDeck newDiscards

                // Can never bust on a freeze card, just pick someone to freeze
                // and remove them
                | ActionCard Card.Freeze ->
                    let nextPlayers = others @ [ current ]
                    let index, targetPlayer = nextPlayers |> List.indexed |> List.randomChoice
                    let newTargetPlayer = targetPlayer |> fun (n, s, h) -> n, s, newCard :: h
                    let newOthers = nextPlayers |> List.removeAt index
                    yield newTargetPlayer |> fun (n, s, h) -> (n, h), newDeck, newDiscards
                    yield! GoonSession newOthers newDeck newDiscards

                // Can bust on a value card, so we need to check if they busted or not
                // to determine if they are done
                | ValueCard _ ->
                    let newHand = newCard :: hand
                    let isBust = Hand.IsBust newHand
                    let newPlayer = [ (name, strategy, newHand) ]
                    let newOthers = if isBust then others else others @ newPlayer
                    yield newPlayer.Head |> fun (n, s, h) -> (n, h), newDeck, newDiscards
                    yield! GoonSession newOthers newDeck newDiscards

                | ActionCard Card.Deal3 ->
                    let nextPlayers = others @ [ current ]
                    let index, targetPlayer = nextPlayers |> List.indexed |> List.randomChoice
                    let newNewDeck, newNewDiscards, newCards = Deck.Draw3 newDeck newDiscards

                    yield! Seq.empty
        }
