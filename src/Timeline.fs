namespace Flip7

type Timeline = seq<Player * Player list * (Deck * Deck)>

module public Timeline =
    let rec private GoonSession (players: list<Player>) (decks: Deck * Deck) (session: uint) : Timeline =
        // Base case: if everyone is done gooning (have stood or busted)
        if players |> List.isEmpty then
            Seq.empty
        else

        // Invariant: should not have busted yet
        assert
            players
            |> List.map (fun player -> player.Hand)
            |> List.forall (not << Hand.IsBust)

        // Base case: if anyone has the Flip7 bonus, they win immediately and
        // everyone else is done gooning
        if
            players
            |> List.map (fun player -> player.Hand)
            |> List.exists Hand.HasFlip7Bonus
        then
            Seq.empty
        else

        #nowarn "FS25"
        let current :: others = players
        let othersHands = others |> List.map (fun player -> player.Hand)
        let (deck', discards'), card' = Deck.Draw1 decks
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

    let public Simulate
        (players: list<string * Strategy>)
        (seedHands: Map<string, Hand> option)
        (seedScores: Map<string, uint> option)
        (seedDeck: Deck option)
        (seedDiscards: Deck option)
        : Map<string, uint> * seq<string * Hand> =
        let rec Simulate'
            (players: list<string * Strategy * uint>)
            (deck: Deck)
            (discards: Deck)
            (accumulator: seq<string * Hand>)
            : Map<string, uint> * seq<string * Hand> =
            // Make initial hands for all players
            let playersWithHands =
                players
                |> List.map (fun (name, strategy, _firmScore) ->
                    let maybeHand = seedHands |> Option.bind (Map.tryFind name)
                    let hand = maybeHand |> Option.defaultValue List.empty
                    name, strategy, hand
                )

            // Goon session
            let seq, (points', deck', discards') =
                GoonSession playersWithHands deck discards 0u
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
        let startingPlayers =
            players
            |> List.map (fun (name, strategy) ->
                let maybeFirmScore = seedScores |> Option.bind (Map.tryFind name)
                name, strategy, maybeFirmScore |> Option.defaultValue 0u
            )

        Simulate'
            startingPlayers
            (seedDeck |> Option.defaultValue Deck.Full)
            (seedDiscards |> Option.defaultValue Deck.Empty)
            Seq.empty
