namespace Flip7

/// <summary>
/// What happened at a single moment of a game.
/// </summary>
type public Event =
    | Drew of Name: string * Card: Card
    | Stood of Name: string
    | Busted of Name: string * Card: Card
    | Froze of Source: string * Target: string
    | SecondChancePassed of Source: string * Target: string
    | SecondChanceDiscarded of Name: string
    | Dealt3 of Source: string * Target: string * Cards: Card list
    | Flip7Achieved of Name: string
    | RoundEnded of Scores: Map<string, uint>

/// <summary>
/// A snapshot of the game immediately after an event. Players still in the
/// round come first (the next player to act at the head), followed by players
/// who have stood, busted, or been frozen.
/// </summary>
type public Instant = {
    Event: Event
    Players: Player list
    Deck: Deck
    Discards: Deck
}

/// <summary>
/// A timeline is the full history of a game: one instant per event.
/// </summary>
type public Timeline = seq<Instant>

module public Event =
    /// <summary>
    /// Converts an event to an array of lines: the event kind followed by its
    /// fields, one per line.
    /// </summary>
    let public Serialize (event: Event) : string array =
        match event with
        | Drew(name, card) -> [| "Drew"; name; string card |]
        | Stood name -> [| "Stood"; name |]
        | Busted(name, card) -> [| "Busted"; name; string card |]
        | Froze(source, target) -> [| "Froze"; source; target |]
        | SecondChancePassed(source, target) -> [| "SecondChancePassed"; source; target |]
        | SecondChanceDiscarded name -> [| "SecondChanceDiscarded"; name |]
        | Dealt3(source, target, cards) -> [| "Dealt3"; source; target; yield! cards |> List.map string |]
        | Flip7Achieved name -> [| "Flip7Achieved"; name |]
        | RoundEnded scores -> [|
            "RoundEnded"
            yield! scores |> Map.toList |> List.map (fun (name, score) -> $"{name}: {score}")
          |]

    /// <summary>
    /// Parses an event from a sequence of lines: the event kind followed by
    /// its fields, one per line.
    /// </summary>
    let public Deserialize (lines: string seq) : Event =
        match lines |> Seq.toList with
        | [ "Drew"; name; card ] -> Drew(name, Card.Parse card)
        | [ "Stood"; name ] -> Stood name
        | [ "Busted"; name; card ] -> Busted(name, Card.Parse card)
        | [ "Froze"; source; target ] -> Froze(source, target)
        | [ "SecondChancePassed"; source; target ] -> SecondChancePassed(source, target)
        | [ "SecondChanceDiscarded"; name ] -> SecondChanceDiscarded name
        | "Dealt3" :: source :: target :: cards -> Dealt3(source, target, cards |> List.map Card.Parse)
        | [ "Flip7Achieved"; name ] -> Flip7Achieved name
        | "RoundEnded" :: scores ->
            scores
            |> List.map (fun line ->
                match line.Split ": " with
                | [| name; score |] -> name, uint score
                | _ -> raise (System.FormatException $"Invalid score line format: {line}")
            )
            |> Map.ofList
            |> RoundEnded
        | lines -> raise (System.FormatException $"Invalid event lines: %A{lines}")

module public Timeline =
    let private MakeInstant (event: Event) (active: Player list) (finished: Player list) (decks: Deck * Deck) : Instant = {
        Event = event
        Players = active @ finished
        Deck = fst decks
        Discards = snd decks
    }

    let private HasSecondChance (player: Player) : bool =
        player.Hand |> List.exists (fun card -> card = ActionCard Card.SecondChance)

    let private ToStrategyPlayer (player: Player) : Strategy.StrategyPlayer = {
        Name = player.Name
        FirmScore = player.FirmScore
        Hand = player.Hand
    }

    // Cards removed from a hand by Hand.Reduce (canceled duplicates and spent
    // second chances) must be returned to the discard pile to conserve cards.
    let private DiscardCanceled (before: Hand) (after: Hand) (discards: Deck) : Deck =
        before
        |> List.fold
            (fun (remaining: Map<Card, int>, discards) card ->
                match Map.tryFind card remaining with
                | Some count when count > 0 -> Map.add card (count - 1) remaining, discards
                | _ -> remaining, Deck.Increment discards card
            )
            (after |> List.countBy id |> Map.ofList, discards)
        |> snd

    let rec private GoonSession
        (random: System.Random)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        (session: uint)
        : Timeline =

        // Invariant: active players should not have busted yet
        assert (active |> List.forall (fun player -> not (Hand.IsBust player.Hand)))

        let flip7Winner = active |> List.tryFind (fun player -> Hand.HasFlip7Bonus player.Hand)

        match flip7Winner, active with
        // Base case: if anyone has the Flip7 bonus, the round ends immediately
        // for everyone
        | Some winner, _ -> Seq.singleton (MakeInstant (Flip7Achieved winner.Name) active finished decks)

        // Base case: everyone has stood, busted, or been frozen
        | None, [] -> Seq.empty

        | None, current :: others ->
            let session' = session + 1u

            // Everyone is dealt at least one card before they may choose
            let hitOrStand =
                if List.isEmpty current.Hand then
                    Strategy.Hit
                else
                    Strategy.DecideWith
                        random
                        current.Strategy
                        session
                        (ToStrategyPlayer current)
                        (others |> List.map ToStrategyPlayer)
                        decks

            match hitOrStand with
            | Strategy.Stand -> seq {
                let finished' = current :: finished
                yield MakeInstant (Stood current.Name) others finished' decks
                yield! GoonSession random others finished' decks session'
              }

            | Strategy.Hit ->

            let decks', card = Deck.Draw1With random decks

            match card with
            // Can never bust on a modifier card or a first second chance card,
            // so easy just add it to the player's hand and pass the turn
            | ActionCard Card.SecondChance when not (HasSecondChance current) -> seq {
                let current' = { current with Hand = card :: current.Hand }
                let active' = others @ [ current' ]
                yield MakeInstant (Drew(current.Name, card)) active' finished decks'
                yield! GoonSession random active' finished decks' session'
              }

            | ModifierCard _ -> seq {
                let current' = { current with Hand = card :: current.Hand }
                let active' = others @ [ current' ]
                yield MakeInstant (Drew(current.Name, card)) active' finished decks'
                yield! GoonSession random active' finished decks' session'
              }

            // Can never bust on a second chance card, but you also can't hold
            // two of them at the same time: give it to a random player who can
            // hold it, or discard it if no one can
            | ActionCard Card.SecondChance ->
                let candidates =
                    others
                    |> List.indexed
                    |> List.filter (fun (_index, player) -> not (HasSecondChance player))

                match candidates with
                | [] -> seq {
                    let deck', discards' = decks'
                    let decks'' = deck', Deck.Increment discards' card
                    let active' = others @ [ current ]
                    yield MakeInstant (SecondChanceDiscarded current.Name) active' finished decks''
                    yield! GoonSession random active' finished decks'' session'
                  }
                | _ -> seq {
                    let index, target = candidates |> List.randomChoiceWith random
                    let target' = { target with Hand = card :: target.Hand }
                    let active' = (others |> List.updateAt index target') @ [ current ]
                    yield MakeInstant (SecondChancePassed(current.Name, target.Name)) active' finished decks'
                    yield! GoonSession random active' finished decks' session'
                  }

            // Can never bust on a freeze card, just pick someone to freeze
            // (possibly yourself); they bank their points and are done for the
            // round
            | ActionCard Card.Freeze -> seq {
                let rotated = others @ [ current ]
                let index, target = rotated |> List.indexed |> List.randomChoiceWith random
                let target' = { target with Hand = card :: target.Hand }
                let active' = rotated |> List.removeAt index
                let finished' = target' :: finished
                yield MakeInstant (Froze(current.Name, target.Name)) active' finished' decks'
                yield! GoonSession random active' finished' decks' session'
              }

            // Can bust on a value card, so we need to check if they busted or
            // not to determine if they are done
            | ValueCard _ -> seq {
                let deck', discards' = decks'
                let hand' = card :: current.Hand
                let isBust, reducedHand = Hand.Reduce hand'
                let decks'' = deck', DiscardCanceled hand' reducedHand discards'
                let current' = { current with Hand = reducedHand }

                if isBust then
                    let finished' = current' :: finished
                    yield MakeInstant (Busted(current.Name, card)) others finished' decks''
                    yield! GoonSession random others finished' decks'' session'
                else
                    let active' = others @ [ current' ]
                    yield MakeInstant (Drew(current.Name, card)) active' finished decks''
                    yield! GoonSession random active' finished decks'' session'
              }

            | ActionCard Card.Deal3 -> seq {
                let deck', discards' = decks'

                // The deal3 card itself is used up immediately
                let discards' = Deck.Increment discards' card

                let rotated = others @ [ current ]
                let index, target = rotated |> List.indexed |> List.randomChoiceWith random
                let (deck', discards'), drawn = Deck.Draw3With random (deck', discards')

                // Freeze and deal3 cards flipped during a deal3 are really
                // hard to resolve with many edge cases, so simplify by
                // discarding them instead of resolving them
                let kept, dropped =
                    drawn
                    |> List.partition (fun card ->
                        match card with
                        | ActionCard Card.Freeze
                        | ActionCard Card.Deal3 -> false
                        | _ -> true
                    )

                let discards' = dropped |> List.fold Deck.Increment discards'
                let hand' = kept @ target.Hand
                let isBust, reducedHand = Hand.Reduce hand'
                let decks'' = deck', DiscardCanceled hand' reducedHand discards'
                let target' = { target with Hand = reducedHand }
                let event = Dealt3(current.Name, target.Name, drawn)

                if isBust then
                    let active' = rotated |> List.removeAt index
                    let finished' = target' :: finished
                    yield MakeInstant event active' finished' decks''
                    yield! GoonSession random active' finished' decks'' session'
                else
                    let active' = rotated |> List.updateAt index target'
                    yield MakeInstant event active' finished decks''
                    yield! GoonSession random active' finished decks'' session'
              }

    /// <summary>
    /// Simulates a full game using the given source of randomness and returns
    /// its timeline lazily: one instant per event, with a RoundEnded instant
    /// closing out every round. The last instant of the timeline is the
    /// RoundEnded where a player has reached 200 points. Seed values, when
    /// provided, apply to the first round only. A seeded random makes the
    /// timeline reproducible, but note that the timeline is lazy: enumerating
    /// it advances the random, so enumerate it once (or cache it) when
    /// reproducibility matters.
    /// </summary>
    let public SimulateWith
        (random: System.Random)
        (players: list<string * Strategy>)
        (seedHands: Map<string, Hand> option)
        (seedScores: Map<string, uint> option)
        (seedDeck: Deck option)
        (seedDiscards: Deck option)
        : Timeline =
        let rec Rounds (players: Player list) (decks: Deck * Deck) : Timeline = seq {
            let roundInstants = GoonSession random players [] decks 0u |> Seq.toList
            yield! roundInstants

            let finalPlayers, (deck, discards) =
                roundInstants
                |> List.tryLast
                |> Option.map (fun instant -> instant.Players, (instant.Deck, instant.Discards))
                |> Option.defaultValue (players, decks)

            // Bank each player's round score and discard their hands
            let scores =
                finalPlayers
                |> List.map (fun player ->
                    let score = if Hand.IsBust player.Hand then 0u else Hand.Score player.Hand
                    player.Name, score
                )
                |> Map.ofList

            let discards' =
                finalPlayers
                |> List.collect (fun player -> player.Hand)
                |> List.fold Deck.Increment discards

            let players' =
                finalPlayers
                |> List.map (fun player -> {
                    player with
                        FirmScore = player.FirmScore + Map.find player.Name scores
                        Hand = List.empty
                })

            yield {
                Event = RoundEnded scores
                Players = players'
                Deck = deck
                Discards = discards'
            }

            // Base case: the game ends when anyone has reached 200 points
            if players' |> List.forall (fun player -> player.FirmScore < 200u) then
                yield! Rounds players' (deck, discards')
        }

        let startingPlayers =
            players
            |> List.map (fun (name, strategy) ->
                Player.Make(
                    name,
                    strategy,
                    ?firmScore = (seedScores |> Option.bind (Map.tryFind name)),
                    ?hand = (seedHands |> Option.bind (Map.tryFind name))
                )
            )

        let startingDecks =
            seedDeck |> Option.defaultValue Deck.Full, seedDiscards |> Option.defaultValue Deck.Empty

        if List.isEmpty startingPlayers then
            Seq.empty
        else
            Rounds startingPlayers startingDecks

    /// <summary>
    /// Simulates a full game and returns its timeline lazily: one instant per
    /// event, with a RoundEnded instant closing out every round. The last
    /// instant of the timeline is the RoundEnded where a player has reached
    /// 200 points. Seed values, when provided, apply to the first round only.
    /// </summary>
    let public Simulate
        (players: list<string * Strategy>)
        (seedHands: Map<string, Hand> option)
        (seedScores: Map<string, uint> option)
        (seedDeck: Deck option)
        (seedDiscards: Deck option)
        : Timeline =
        SimulateWith System.Random.Shared players seedHands seedScores seedDeck seedDiscards

    /// <summary>
    /// The final scoreboard of a timeline: each player's firm score at the
    /// last instant. Enumerates the entire timeline.
    /// </summary>
    let public Scoreboard (timeline: Timeline) : Map<string, uint> =
        match timeline |> Seq.tryLast with
        | None -> Map.empty
        | Some instant ->
            instant.Players
            |> List.map (fun player -> player.Name, player.FirmScore)
            |> Map.ofList
