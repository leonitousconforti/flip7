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

    let rec private GoonSession
        (players: list<string * Strategy * Hand>)
        (deck: Deck)
        (discards: Deck)
        (session: uint)
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
        let deck', discards', card' = Deck.Draw1 deck discards
        let session' = session + 1u
        #warnon "FS25"

        let lastPlayerLeft = lazy (others |> List.isEmpty)
        let alreadyHasSecondChance =
            lazy (hand |> List.exists (fun card -> card = ActionCard Card.SecondChance))

        let hitOrStand =
            if session = 0u then
                Strategy.Hit
            else
                strategy session hand othersHands deck

        match hitOrStand with
        | Strategy.Stand -> seq {
            yield (name, hand), deck, discards
            yield! GoonSession others deck discards session'
          }

        | Strategy.Hit ->
            match card' with
            // Can never bust on a modifier card, so easy just add it to the
            // player's hand and keep going
            | ModifierCard _ -> seq {
                let player' = [ (name, strategy, card' :: hand) ]
                yield player'.Head |> fun (n, s, h) -> (n, h), deck', discards'
                yield! GoonSession (others @ player') deck' discards' session'
              }

            // Can never bust on a second chance card, but you also can't hold
            // two of them at the same time even when you are the last player
            | ActionCard Card.SecondChance when alreadyHasSecondChance.Value && lastPlayerLeft.Value -> seq {
                let discards'' = Deck.Increment discards' (ActionCard Card.SecondChance)
                yield (name, hand), deck', discards''
                yield! GoonSession others deck' discards'' session'
              }

            // Can never bust on a second chance card, but you also can't hold
            // two of them at the same time, must give it to someone else
            | ActionCard Card.SecondChance when alreadyHasSecondChance.Value && not lastPlayerLeft.Value -> seq {
                let index, targetPlayer = others |> List.indexed |> List.randomChoice
                let targetPlayer' = targetPlayer |> fun (n, s, h) -> n, s, card' :: h
                let others' = others |> List.updateAt index targetPlayer'
                yield targetPlayer' |> fun (n, s, h) -> (n, h), deck', discards'
                yield! GoonSession (others' @ [ current ]) deck' discards' session'
              }

            // Can never bust on a second chance card, so easy just just add it to the
            // player's hand and keep going
            | ActionCard Card.SecondChance -> seq {
                let player' = [ (name, strategy, card' :: hand) ]
                yield player'.Head |> fun (n, s, h) -> (n, h), deck', discards'
                yield! GoonSession (others @ player') deck' discards' session'
              }

            // Can never bust on a freeze card, just pick someone to freeze
            // and remove them
            | ActionCard Card.Freeze -> seq {
                let players' = others @ [ current ]
                let index, targetPlayer = players' |> List.indexed |> List.randomChoice
                let targetPlayer' = targetPlayer |> fun (n, s, h) -> n, s, card' :: h
                let others' = players' |> List.removeAt index
                yield targetPlayer' |> fun (n, s, h) -> (n, h), deck', discards'
                yield! GoonSession others' deck' discards' session'
              }

            // Can bust on a value card, so we need to check if they busted or not
            // to determine if they are done
            | ValueCard _ -> seq {
                let hand' = card' :: hand
                let isBust, reducedHand = Hand.Reduce hand'
                let player' = [ (name, strategy, reducedHand) ]
                let others' = if isBust then others else others @ player'
                yield player'.Head |> fun (n, s, h) -> (n, h), deck', discards'
                yield! GoonSession others' deck' discards' session'
              }

            | ActionCard Card.Deal3 ->
                seq {
                    let players' = others @ [ current ]
                    let index, targetPlayer = players' |> List.indexed |> List.randomChoice
                    let deck'', discards'', card'' = Deck.Draw3 deck' discards'
                    let targetPlayer' = targetPlayer |> fun (n, s, h) -> n, s, card'' @ h

                    if card'' |> List.exists (fun card -> card.IsActionCard) |> not then
                        let isBust, reducedHand = targetPlayer' |> fun (n, s, h) -> Hand.Reduce h
                        let others' =
                            if isBust then
                                players' |> List.removeAt index
                            else
                                players' |> List.updateAt index targetPlayer'
                        yield targetPlayer' |> fun (n, s, h) -> (n, h), deck'', discards''
                        yield! GoonSession others' deck'' discards'' session'
                    else
                        // Action cards from deal3 will be pretty hard to
                        // process
                        yield! Seq.empty
                }

    let public Simulate (players: list<string * Strategy>) : Map<string, uint> * seq<string * Hand> =
        let rec Simulate'
            (players: list<string * Strategy * uint>)
            (deck: Deck)
            (discards: Deck)
            (accumulator: seq<string * Hand>)
            : Map<string, uint> * seq<string * Hand> =
            // Make initial hands for all players
            let playersWithEmptyHands =
                players
                |> List.map (fun (name, strategy, _firmScore) ->
                    let hand = Hand.Empty
                    name, strategy, hand
                )

            // Goon session
            let seq, (points', deck', discards') =
                GoonSession playersWithEmptyHands deck discards 0u
                |> Seq.mapFold
                    (fun (acc, _, _) (data, deck', discards') ->
                        let name, hand = data
                        let score = if Hand.IsBust hand then 0u else Hand.Score hand
                        let acc' = acc |> Map.add name score
                        data, (acc', deck', discards')
                    )
                    (Map.empty, deck, discards)

            // Add the new points to the players' firm scores
            let players' =
                players
                |> List.map (fun (name, strategy, firmScore) ->
                    let maybeScore = Map.tryFind name points'
                    name, strategy, firmScore + (maybeScore |> Option.defaultValue 0u)
                )

            // Calculator the scoreboard
            let scoreboard =
                players'
                |> List.map (fun (name, _strategy, firmScore) -> name, firmScore)
                |> Map.ofList

            // Base case: if anyone has reached 200 points yet
            if scoreboard |> Map.exists (fun _ score -> score >= 200u) then
                scoreboard, accumulator
            else
                let accumulator' = Seq.append accumulator seq
                Simulate' players' deck' discards' accumulator'

        in
        Simulate' (players |> List.map (fun (name, strategy) -> name, strategy, 0u)) Deck.Full Deck.Empty Seq.empty
