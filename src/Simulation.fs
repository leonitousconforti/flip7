namespace Flip7

module Simulation =
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

        let deck' = Deck.Decrement deck (ActionCard Card.Deal3)
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

    /// <summary>
    /// The expected change in banked score from taking exactly one more flip
    /// and then standing. Value and modifier cards score through Hand.Reduce
    /// and Hand.Score, so busts, second chance cancellations, the x2
    /// modifier, and the flip7 bonus are all accounted for; action cards are
    /// treated as not changing the player's own banked score. When the deck
    /// is empty the discards are about to be reshuffled into the new deck, so
    /// the draw is taken from them instead.
    /// </summary>
    let public expectedValueOfHit (deck: Deck) (discards: Deck) (hand: Hand) : float =
        let effectiveDeck = if Deck.IsEmpty deck then discards else deck
        let currentScore = float (Hand.Score hand)

        Deck.pdf effectiveDeck
        |> Map.toList
        |> List.sumBy (fun (card, probability) ->
            if probability = 0.0 then
                0.0
            else
                let score' =
                    match card with
                    | ActionCard _ -> currentScore
                    | ModifierCard _
                    | ValueCard _ ->
                        let isBust, reducedHand = Hand.Reduce(card :: hand)
                        if isBust then 0.0 else float (Hand.Score reducedHand)

                probability * (score' - currentScore)
        )

    let public Issues (deck: Deck) (discards: Deck) (hands: Hand seq) : string seq =
        let handsIssues: string seq =
            hands
            |> Seq.map (fun hand -> hand |> List.filter (fun card -> card = ActionCard Card.SecondChance))
            |> Seq.where (List.length >> (<) 1)
            |> Seq.indexed
            |> Seq.map (fun (index, secondChances) ->
                $"Player {index} has {List.length secondChances} second chances in their hand, maximum is 1"
            )

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
                yield! handsIssues

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
