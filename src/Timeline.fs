namespace Flip7

open FSharp.Control

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

    override self.ToString() : string =
        match self with
        | Drew(name, card) -> $"{name} drew {card}"
        | Stood name -> $"{name} stood"
        | Busted(name, card) -> $"{name} drew {card} and busted"
        | Froze(source, target) when source = target -> $"{source} drew Freeze and froze themselves"
        | Froze(source, target) -> $"{source} drew Freeze and froze {target}"
        | SecondChancePassed(source, target) -> $"{source} passed a SecondChance to {target}"
        | SecondChanceDiscarded name -> $"{name} discarded a duplicate SecondChance"
        | Dealt3(source, target, cards) ->
            let cards = cards |> List.map string |> String.concat ", "
            $"{source} drew Deal3, dealing {target}: {cards}"
        | Flip7Achieved name -> $"{name} flipped 7 and ended the round!"
        | RoundEnded scores ->
            let scores =
                scores
                |> Map.toList
                |> List.map (fun (name, score) -> $"{name} +{score}")
                |> String.concat "  "

            $"round over: {scores}"

    member public self.Actor() : string option =
        match self with
        | Drew(name, _) -> Some name
        | Stood name -> Some name
        | Busted(name, _) -> Some name
        | SecondChanceDiscarded name -> Some name
        | Flip7Achieved name -> Some name
        | Froze(_, target) -> Some target
        | SecondChancePassed(_, target) -> Some target
        | Dealt3(_, target, _) -> Some target
        | RoundEnded _ -> None

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
/// A timeline is the full history of a game: one instant per event, produced
/// asynchronously so consumers can render or persist instants as they arrive
/// rather than waiting for the whole game to finish.
/// </summary>
type public Timeline = AsyncSeq<Instant>

/// <summary>
/// Decides hit-or-stand for one player: given the player's declared strategy,
/// the round, the turn, the player, the other active players, and the decks.
/// Injecting one into Timeline.SimulateWithDecider lets Prompt and Adaptive
/// strategies be decided by a human at the terminal or by a model, while
/// everything else defers to Strategy.DecideWith.
/// </summary>
type public Decider =
    Strategy
        -> uint
        -> uint
        -> Strategy.StrategyPlayer
        -> Strategy.StrategyPlayer list
        -> (Deck * Deck)
        -> Strategy.HitOrStand

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
    let private MakeInstant
        (event: Event)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Instant = {
        Event = event
        Players = active @ finished
        Deck = fst decks
        Discards = snd decks
    }

    let private step (event: Event) (active: Player list) (finished: Player list) (decks: Deck * Deck) =
        [ MakeInstant event active finished decks ], active, finished, decks

    let private HasSecondChance (player: Player) : bool =
        player.Hand |> List.exists (fun card -> card = ActionCard Card.SecondChance)

    let private WithCard (card: Card) (player: Player) : Player = { player with Hand = card :: player.Hand }

    let private ChooseAny (random: System.Random) (players: Player list) : int * Player =
        players |> List.indexed |> List.randomChoiceWith random

    let private FreezePlayer
        (random: System.Random)
        (source: string)
        (card: Card)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Instant list * Player list * Player list * (Deck * Deck) =
        let index, target = ChooseAny random active
        step (Froze(source, target.Name)) (List.removeAt index active) (WithCard card target :: finished) decks

    let private GiveAwaySecondChance
        (random: System.Random)
        (card: Card)
        (holder: string)
        (active: Player list)
        (decks: Deck * Deck)
        : string option * Player list * (Deck * Deck) =
        let candidates =
            active
            |> List.indexed
            |> List.where (fun (_, player) -> player.Name <> holder && not (HasSecondChance player))

        match candidates with
        | [] ->
            let deck, discards = decks
            None, active, (deck, Deck.Increment discards card)
        | _ ->
            let index, recipient = candidates |> List.randomChoiceWith random
            Some recipient.Name, List.updateAt index (WithCard card recipient) active, decks

    let private RemoveFirst (card: Card) (hand: Hand) : Hand =
        match hand |> List.tryFindIndex ((=) card) with
        | Some index -> List.removeAt index hand
        | None -> hand

    // Resolves a deal3 given by source to the player at targetIndex in the
    // active rotation. Cards are flipped one at a time and every card counts
    // toward the three: number and modifier cards join the target's hand
    // (stopping early on a bust or a flip7, so the remaining cards are never
    // drawn), second chances are kept by the target or passed along per the
    // usual rules (and can save a later bust within the same deal3), and freeze
    // and deal3 cards are set aside until the flips finish. Set-asides are
    // parked in the target's hand while pending: that keeps every card
    // accounted for at every instant and, unlike parking them in the discards,
    // keeps them from being reshuffled back into the deck mid-deal3. Once the
    // flips finish they are resolved in flip order - unless the target busted
    // or the round ended, in which case they stay in the (soon discarded) hand
    // - with a nested deal3 resolving recursively.
    let rec private ResolveDeal3
        (random: System.Random)
        (source: string)
        (targetIndex: int)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Instant list * Player list * Player list * (Deck * Deck) =

        let targetName = (List.item targetIndex active).Name

        let rec Flip
            (remaining: uint)
            (flipped: Card list)
            (setAsides: Card list)
            (active: Player list)
            (decks: Deck * Deck)
            : bool * Card list * Card list * Player list * (Deck * Deck) =
            let target = List.item targetIndex active

            if remaining = 0u || Hand.HasFlip7Bonus target.Hand then
                false, List.rev flipped, List.rev setAsides, active, decks
            else

            let decks', card = Deck.Draw1With random decks
            let deck', discards' = decks'
            let flipped' = card :: flipped

            match card with
            // Parked in the target's hand (harmless: action cards do not score
            // or bust) until the flips finish, then resolved below
            | ActionCard Card.Freeze
            | ActionCard Card.Deal3 ->
                let active' = List.updateAt targetIndex (WithCard card target) active
                Flip (remaining - 1u) flipped' (card :: setAsides) active' decks'

            | ActionCard Card.SecondChance when not (HasSecondChance target) ->
                let active' = List.updateAt targetIndex (WithCard card target) active
                Flip (remaining - 1u) flipped' setAsides active' decks'

            | ActionCard Card.SecondChance ->
                let _, active', decks'' = GiveAwaySecondChance random card target.Name active decks'
                Flip (remaining - 1u) flipped' setAsides active' decks''

            | ValueCard _
            | ModifierCard _ ->
                let isBust, reducedHand, removedCards = Hand.Reduce(card :: target.Hand)
                let decks'' = deck', List.fold Deck.Increment discards' removedCards
                let active' = List.updateAt targetIndex { target with Hand = reducedHand } active

                if isBust then
                    true, List.rev flipped', List.rev setAsides, active', decks''
                else
                    Flip (remaining - 1u) flipped' setAsides active' decks''

        let isBust, flipped, setAsides, active', decks' = Flip 3u [] [] active decks
        let event = Dealt3(source, targetName, flipped)

        if isBust then
            let target' = List.item targetIndex active'
            let active'' = List.removeAt targetIndex active'
            let finished' = target' :: finished
            step event active'' finished' decks'
        else
            // Takes the pending set-aside card out of the target's hand
            // (wherever the target sits now) so it can move to its destination.
            let unpark (active: Player list) (finished: Player list) (card: Card) =
                let take =
                    List.map (fun p ->
                        if p.Name = targetName then
                            { p with Hand = RemoveFirst card p.Hand }
                        else
                            p
                    )
                take active, take finished

            let ResolveSetAside
                ((instants, active, finished, decks): Instant list * Player list * Player list * (Deck * Deck))
                (setAside: Card)
                =
                let roundEnded =
                    List.isEmpty active
                    || active |> List.exists (fun player -> Hand.HasFlip7Bonus player.Hand)

                match setAside with
                | _ when roundEnded -> instants, active, finished, decks
                | ActionCard Card.Freeze ->
                    let active, finished = unpark active finished setAside

                    let more, active', finished', decks' =
                        FreezePlayer random targetName setAside active finished decks

                    instants @ more, active', finished', decks'
                | ActionCard Card.Deal3 ->
                    let active, finished = unpark active finished setAside
                    let deck, discards = decks
                    let decks = deck, Deck.Increment discards setAside
                    let index, _ = ChooseAny random active

                    let nested, active', finished', decks' =
                        ResolveDeal3 random targetName index active finished decks

                    instants @ nested, active', finished', decks'
                | _ -> raise (System.InvalidOperationException $"Unexpected set-aside card: {setAside}")

            setAsides |> List.fold ResolveSetAside (step event active' finished decks')

    // Resolves a single card drawn by the current player on a hit: adds it to
    // the hand, gives away or discards an unkeepable second chance, freezes a
    // player, busts on a duplicate, or hands off to the deal3 resolver. Returns
    // the instants produced and the active players, finished players, and decks
    // to continue from. The decks passed in are already past the draw.
    let private ResolveDraw
        (random: System.Random)
        (current: Player)
        (others: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        (card: Card)
        : Instant list * Player list * Player list * (Deck * Deck) =
        let deck, discards = decks

        // Adds a card that can never bust the player to their hand and passes
        // the turn
        let keep () =
            step (Drew(current.Name, card)) (others @ [ WithCard card current ]) finished decks

        match card with
        | ModifierCard _ -> keep ()
        | ActionCard Card.SecondChance when not (HasSecondChance current) -> keep ()

        // Can never bust on a second chance card, but you also can't hold two of
        // them at the same time: give it away, or discard it if no one can hold
        // it
        | ActionCard Card.SecondChance ->
            let recipient, active', decks' =
                GiveAwaySecondChance random card current.Name (others @ [ current ]) decks

            let event =
                match recipient with
                | Some name -> SecondChancePassed(current.Name, name)
                | None -> SecondChanceDiscarded current.Name

            step event active' finished decks'

        // Can never bust on a freeze card, just pick someone to freeze (possibly
        // yourself); they bank their points and are done for the round
        | ActionCard Card.Freeze -> FreezePlayer random current.Name card (others @ [ current ]) finished decks

        // Can bust on a value card, so reduce the hand to see whether the player
        // is done
        | ValueCard _ ->
            let isBust, reducedHand, removedCards = Hand.Reduce(card :: current.Hand)
            let decks' = deck, List.fold Deck.Increment discards removedCards
            let current' = { current with Hand = reducedHand }

            if isBust then
                step (Busted(current.Name, card)) others (current' :: finished) decks'
            else
                step (Drew(current.Name, card)) (others @ [ current' ]) finished decks'

        // The deal3 card itself is used up immediately; the receiving player
        // (possibly yourself) then flips up to three cards
        | ActionCard Card.Deal3 ->
            let decks' = deck, Deck.Increment discards card
            let rotated = others @ [ current ]
            let index, _ = ChooseAny random rotated
            ResolveDeal3 random current.Name index rotated finished decks'

    let rec private GoonSession
        (random: System.Random)
        (decide: Decider)
        (round: uint)
        (turnsTaken: Map<string, uint>)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : seq<Instant> =

        // Invariant: active players should not have busted yet
        assert (active |> List.forall (fun player -> not (Hand.IsBust player.Hand)))

        // Invariant: every card is accounted for across the deck, the discards,
        // and the players' hands
        assert
            (active @ finished
             |> List.map (fun player -> player.Hand)
             |> Simulation.Issues (fst decks) (snd decks)
             |> Seq.isEmpty)

        let flip7Winner =
            active |> List.tryFind (fun player -> Hand.HasFlip7Bonus player.Hand)

        match flip7Winner, active with
        // Base case: if anyone has the Flip7 bonus, the round ends immediately
        // for everyone
        | Some winner, _ -> Seq.singleton (MakeInstant (Flip7Achieved winner.Name) active finished decks)

        // Base case: everyone has stood, busted, or been frozen
        | None, [] -> Seq.empty

        | None, current :: others ->
            let turn = (turnsTaken |> Map.tryFind current.Name |> Option.defaultValue 0u) + 1u
            let turnsTaken = Map.add current.Name turn turnsTaken

            // The first time play comes around the table each player is dealt a
            // card before they may choose, and a player may never stand with no
            // cards, so the opening turn is always a forced hit
            let hitOrStand =
                if turn = 1u then
                    Strategy.Hit
                else
                    decide
                        current.Strategy
                        round
                        turn
                        (current.ToStrategyPlayer())
                        (others |> List.map (fun p -> p.ToStrategyPlayer()))
                        decks

            match hitOrStand with
            | Strategy.Stand -> seq {
                let finished' = current :: finished
                yield MakeInstant (Stood current.Name) others finished' decks
                yield! GoonSession random decide round turnsTaken others finished' decks
              }

            | Strategy.Hit ->
                let decks', card = Deck.Draw1With random decks

                let instants, active', finished', decks'' =
                    ResolveDraw random current others finished decks' card

                seq {
                    yield! instants
                    yield! GoonSession random decide round turnsTaken active' finished' decks''
                }

    /// <summary>
    /// Simulates a full game like SimulateWith, but every hit-or-stand
    /// decision is routed through the given decider, so Prompt and Adaptive
    /// strategies can be answered by a human at the terminal or by an
    /// external model. Pulling the timeline drives the game, so a decider may
    /// block (waiting on a key press, say) and the game simply pauses there.
    /// </summary>
    let public SimulateWithDecider
        (random: System.Random)
        (decide: Decider)
        (players: list<string * Strategy>)
        (seedHands: Map<string, Hand> option)
        (seedScores: Map<string, uint> option)
        (seedDeck: Deck option)
        (seedDiscards: Deck option)
        : Timeline =
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
            AsyncSeq.empty
        else

        // A loop rather than recursion so an arbitrarily long game cannot
        // accumulate nested asyncSeq appends
        asyncSeq {
            let mutable round = 1u
            let mutable players = startingPlayers
            let mutable decks = startingDecks
            let mutable gameOver = false

            while not gameOver do
                let mutable lastInstant = None

                for instant in GoonSession random decide round Map.empty players List.empty decks do
                    lastInstant <- Some instant
                    yield instant

                let finalPlayers, (deck, discards) =
                    lastInstant
                    |> Option.map (fun instant -> instant.Players, (instant.Deck, instant.Discards))
                    |> Option.defaultValue (players, decks)

                let scores =
                    finalPlayers
                    |> List.map (fun player ->
                        let score =
                            if Hand.IsBust player.Hand then
                                0u
                            else
                                Hand.Score player.Hand
                        player.Name, score
                    )
                    |> Map.ofList

                let discards' =
                    finalPlayers
                    |> List.collect (fun player -> player.Hand)
                    |> List.fold Deck.Increment discards

                let scored =
                    players
                    |> List.map (fun player -> {
                        player with
                            FirmScore = player.FirmScore + Map.find player.Name scores
                            Hand = List.empty
                    })

                yield {
                    Event = RoundEnded scores
                    Players = scored
                    Deck = deck
                    Discards = discards'
                }

                if scored |> List.forall (fun player -> player.FirmScore < 200u) then
                    let rotated =
                        match scored with
                        | [] -> []
                        | leader :: rest -> rest @ [ leader ]

                    round <- round + 1u
                    players <- rotated
                    decks <- deck, discards'
                else
                    gameOver <- true
        }

    /// <summary>
    /// Simulates a full game using the given source of randomness and returns
    /// its timeline lazily: one instant per event, with a RoundEnded instant
    /// closing out every round. The last instant of the timeline is the
    /// RoundEnded in which a player first reaches 200 points. Seed values, when
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
        SimulateWithDecider random (Strategy.DecideWith random) players seedHands seedScores seedDeck seedDiscards

    /// <summary>
    /// Simulates a full game and returns its timeline lazily: one instant per
    /// event, with a RoundEnded instant closing out every round. The last
    /// instant of the timeline is the RoundEnded in which a player first reaches
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
