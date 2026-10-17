namespace Flip7

open FSharp.Control

/// <summary>
/// One question a timeline shows a player was asked, with their answer and the
/// table exactly as the engine put the question to them. Choice is Choice1Of2
/// for a hit-or-stand decision, and Choice2Of2 for an aim: what was being aimed
/// and the name of the player it was given to.
///
/// For a hit-or-stand decision OtherPlayers are the players still in the round
/// besides the player, and FinishedPlayers those who stood, busted or were
/// frozen, as the HitOrStandDecider receives them. For an aim OtherPlayers are
/// the candidates: the legal targets in the order the engine passed them to the
/// TargetDecider, which for a freeze or a deal3 may include the player
/// themselves, and FinishedPlayers, Deck and Discards are as the TargetDecider
/// received them. Round counts from one, and Turn is how many turns the player
/// has taken in it, counting the turn the instant shows them taking, forced or
/// not, or zero if they have taken none: an aim made after hitting on a third
/// turn is made on turn three and the card of a forced first hit on turn one,
/// while a card a deal3's target set aside is given out on the turns they had
/// already taken, which may be none.
/// </summary>
type public Observation = {
    Name: string
    Choice: Choice<Strategy.HitOrStand, Strategy.Ask * string>
    Round: uint
    Turn: uint
    Player: Player
    OtherPlayers: Player list
    FinishedPlayers: Player list
    Deck: Deck
    Discards: Deck
}

module public Observation =
    // The player whose choice produced an event, and what that choice was, or
    // None for events not produced by a choice. Drawing a Freeze, SecondChance,
    // or Deal3 still counts as the drawer choosing to hit; how the drawn card
    // resolves afterwards is not their hit-or-stand decision. Not
    // Event.Actor(), which answers whose state an event changed: for a Froze, a
    // SecondChancePassed, or a Dealt3 that is the target, not the player who
    // chose to hit.
    let private Decider (event: Event) : (string * Strategy.HitOrStand) option =
        match event with
        | Drew(name, _) -> Some(name, Strategy.Hit)
        | Busted(name, _) -> Some(name, Strategy.Hit)
        | SecondChanceDiscarded name -> Some(name, Strategy.Hit)
        | Froze(source, _) -> Some(source, Strategy.Hit)
        | SecondChancePassed(source, _) -> Some(source, Strategy.Hit)
        | Dealt3(source, _, _) -> Some(source, Strategy.Hit)
        | Stood name -> Some(name, Strategy.Stand)
        | TableSeated -> None
        | Edited _ -> None
        | Flip7Achieved _ -> None
        | RoundEnded _ -> None

    // How many of a deal3's flipped cards were set aside to be given out
    // afterwards. Each one resolves as a later Froze or Dealt3 event whose
    // source is the deal3's target, and those are not voluntary decisions
    let private SetAsides (cards: Card list) : int =
        cards
        |> List.sumBy (fun card ->
            match card with
            | ActionCard Card.Freeze
            | ActionCard Card.Deal3 -> 1
            | _ -> 0
        )

    // A question a player was asked and their answer, at the table they were
    // asked it at, on the turn the turns taken this round give them, or zero if
    // they have taken none
    let private Asked
        (round: uint)
        (turns: Map<string, uint>)
        (choice: Choice<Strategy.HitOrStand, Strategy.Ask * string>)
        (player: Player)
        (others: Player list)
        (finished: Player list)
        ((deck, discards): Deck * Deck)
        : Observation = {
        Name = player.Name
        Choice = choice
        Round = round
        Turn = turns |> Map.tryFind player.Name |> Option.defaultValue 0u
        Player = player
        OtherPlayers = others
        FinishedPlayers = finished
        Deck = deck
        Discards = discards
    }

    /// <summary>
    /// The state of the decision scan partway through a timeline: everything
    /// FromTimeline tracks between instants, opaque to callers, so a caller
    /// receiving instants one at a time can extract observations incrementally
    /// through Observe.
    /// </summary>
    type public Scan =
        private | Scan of
            Finished: Set<string> *
            Round: uint *
            Turns: Map<string, uint> *
            Pending: Map<string, int> *
            Previous: Instant option

    /// <summary>
    /// The scan before any instant: round one, nobody finished, no turns taken.
    /// </summary>
    let public Start: Scan = Scan(Set.empty, 1u, Map.empty, Map.empty, None)

    /// <summary>
    /// How many turns each player has taken in the scan's current round.
    /// Matches the engine's per-round counter, because every turn produces
    /// exactly one decision event.
    /// </summary>
    let public TurnsTaken (scan: Scan) : Map<string, uint> =
        let (Scan(_, _, turns, _, _)) = scan
        turns

    /// <summary>
    /// The round the scan is in, counting from one.
    /// </summary>
    let public RoundOf (scan: Scan) : uint =
        let (Scan(_, round, _, _, _)) = scan
        round

    /// <summary>
    /// How many of the cards a player set aside during deal3s they have yet to
    /// give out. Each one resolves as a later Froze or Dealt3 event whose
    /// source is the player, and none of those is a voluntary decision.
    /// </summary>
    let internal Owes (scan: Scan) (name: string) : int =
        let (Scan(_, _, _, pending, _)) = scan
        pending |> Map.tryFind name |> Option.defaultValue 0

    /// <summary>
    /// Whether a player is still in the scan's round: neither stood, frozen nor
    /// busted, as the round's events tell. Not their hands: the play editor can
    /// take the busting card back out of a busted player's hand, and the engine
    /// still counts them finished.
    /// </summary>
    let internal InRound (scan: Scan) (player: Player) : bool =
        let (Scan(finished, _, _, _, _)) = scan
        not (Set.contains player.Name finished)

    // The aims an instant reveals, given the scan of every instant before it,
    // the turns taken counting the instant's own, and the instant before it, in
    // the order the engine asked for them, each with the question exactly as
    // the engine put it. The preceding instant is the table the question was
    // asked at, but for what the card's resolution changed before asking: a
    // drawn card leaves the deck, a deal3 card goes to the discards, a
    // set-aside card leaves the hand it was parked in, and a second chance the
    // target of a deal3 flips but cannot keep is aimed partway through the
    // flips, which are replayed card by card from the instant before the deal3.
    // The engine offers a freeze or a deal3 to everyone in the round in turn
    // order, the drawer moved behind the others, and a second chance to
    // everyone in it but its holder who has none. The turn order is the
    // instant's own, but at a RoundEnded the next round starts one seat further
    // round the table. A second chance passed on during the flips is silent in
    // the timeline, and who took the only one is told by who holds one after
    // the deal3 and did not before
    let private Aims (scan: Scan) (turns: Map<string, uint>) (before: Instant) (instant: Instant) : Observation list =
        let secondChance = ActionCard Card.SecondChance
        let holdsSecondChance (player: Player) = List.contains secondChance player.Hand

        // Draws a card the way Deck.DrawWith does, shuffling the discards in as
        // the deck once it is empty
        let drawn ((deck, discards): Deck * Deck) (card: Card) : Deck * Deck =
            if Deck.IsEmpty deck then
                Deck.Decrement discards card, Deck.Empty
            else
                Deck.Decrement deck card, discards

        let aimed ask (chooser: Player) candidates finished decks target =
            Asked (RoundOf scan) turns (Choice2Of2(ask, target)) chooser candidates finished decks

        let decks = before.Deck, before.Discards

        let active, finished =
            match before.Event, before.Players with
            | RoundEnded _, leader :: rest -> rest @ [ leader ], []
            | _ -> before.Players |> List.partition (InRound scan)

        // The drawer is at the head of the turn order, and a card they draw is
        // offered to the others first and then to them
        let drawer (source: string) : (Player * Player list) option =
            match active with
            | current :: others when current.Name = source -> Some(current, others @ [ current ])
            | _ -> None

        // A card a deal3's target set aside is taken out of their hand before
        // they aim it, wherever they sit now
        let unparked (source: string) (card: Card) : (Player * Player list * Player list) option =
            let unpark =
                List.map (fun (player: Player) ->
                    match player.Hand |> List.tryFindIndex ((=) card) with
                    | Some index when player.Name = source -> { player with Hand = List.removeAt index player.Hand }
                    | _ -> player
                )

            let active, finished = unpark active, unpark finished

            active @ finished
            |> List.tryFind (fun player -> player.Name = source)
            |> Option.map (fun chooser -> chooser, active, finished)

        // The second chance the target of a deal3 passed on during its flips,
        // if they passed on exactly one, replayed card by card from the table
        // the deal3 was aimed at. The flips stop at a bust, so the cards the
        // deal3 records never run past one
        let passed (rotation: Player list) (finished: Player list) (decks: Deck * Deck) (target: string) cards =
            let takers =
                rotation
                |> List.where (fun player -> player.Name <> target && not (holdsSecondChance player))

            // The target as the flips leave them, the decks, and every second
            // chance passed on so far, newest first, with the target and the
            // decks as it was aimed
            let flip (dealtTo: Player, decks: Deck * Deck, passes: (Player * (Deck * Deck)) list) (card: Card) =
                let deck, discards = drawn decks card

                match card with
                | ActionCard Card.SecondChance when holdsSecondChance dealtTo ->
                    if passes.Length < takers.Length then
                        dealtTo, (deck, discards), (dealtTo, (deck, discards)) :: passes
                    else
                        dealtTo, (deck, Deck.Increment discards card), passes
                | ActionCard _ -> { dealtTo with Hand = card :: dealtTo.Hand }, (deck, discards), passes
                | ValueCard _
                | ModifierCard _ ->
                    let _, reduced, removed = Hand.Reduce(card :: dealtTo.Hand)
                    { dealtTo with Hand = reduced }, (deck, List.fold Deck.Increment discards removed), passes

            let took =
                takers
                |> List.where (fun taker ->
                    instant.Players
                    |> List.exists (fun player -> player.Name = taker.Name && holdsSecondChance player)
                )

            match rotation |> List.tryFind (fun player -> player.Name = target) with
            | Some dealtTo ->
                match List.fold flip (dealtTo, decks, []) cards, took with
                | (_, _, [ chooser, decks ]), [ taker ] -> [
                    aimed Strategy.Ask.WhoReceivesSecondChance chooser takers finished decks taker.Name
                  ]
                | _ -> []
            | None -> []

        match instant.Event with
        | Froze(source, target) when Owes scan source = 0 ->
            drawer source
            |> Option.map (fun (current, rotation) ->
                aimed Strategy.Ask.WhoToFreeze current rotation finished (drawn decks (ActionCard Card.Freeze)) target
            )
            |> Option.toList
        | Froze(source, target) ->
            unparked source (ActionCard Card.Freeze)
            |> Option.map (fun (chooser, active, finished) ->
                aimed Strategy.Ask.WhoToFreeze chooser active finished decks target
            )
            |> Option.toList
        | SecondChancePassed(source, target) ->
            drawer source
            |> Option.map (fun (current, rotation) ->
                let takers =
                    rotation
                    |> List.where (fun player -> player.Name <> source && not (holdsSecondChance player))

                aimed Strategy.Ask.WhoReceivesSecondChance current takers finished (drawn decks secondChance) target
            )
            |> Option.toList
        | Dealt3(source, target, cards) ->
            let deal3 = ActionCard Card.Deal3

            let asked =
                if Owes scan source = 0 then
                    drawer source
                    |> Option.map (fun (current, rotation) ->
                        let deck, discards = drawn decks deal3
                        current, rotation, finished, (deck, Deck.Increment discards deal3)
                    )
                else
                    unparked source deal3
                    |> Option.map (fun (chooser, active, finished) ->
                        chooser, active, finished, (before.Deck, Deck.Increment before.Discards deal3)
                    )

            match asked with
            | None -> []
            | Some(chooser, rotation, finished, decks) ->
                aimed Strategy.Ask.WhoReceivesDeal3 chooser rotation finished decks target
                :: passed rotation finished decks target cards
        | _ -> []

    /// <summary>
    /// Advances the scan by one instant, yielding every question the instant
    /// shows a player was asked, in the order the engine asked them: the
    /// hit-or-stand decision first, since the card a hit draws is drawn once
    /// the player has chosen, then the aims the card's resolution asked for.
    /// Each instant is a snapshot immediately after its event, so the preceding
    /// instant is the exact state a hit-or-stand decision was made from, and
    /// the table an aim was asked at but for what the card's resolution changed
    /// before asking: the drawn card leaves the deck, a deal3 card goes to the
    /// discards, a set-aside card leaves the hand it was parked in, and a
    /// second chance a deal3's target flips but cannot keep is aimed partway
    /// through the flips. A player's first turn of every round is a forced hit
    /// the engine never asks their strategy about - it is dealing, not a
    /// decision - and gives no hit-or-stand decision; the turn is reconstructed
    /// by counting the player's decision events, which matches the engine's
    /// count because every turn produces exactly one such event. Froze and
    /// Dealt3 events that resolve a set-aside card from an earlier deal3 are
    /// the flipper giving out cards they were dealt, not a voluntary hit, and
    /// give none either.
    ///
    /// The OtherPlayers of a hit-or-stand decision are only the players still
    /// in the round, matching what the hit-or-stand decider receives: players
    /// who stood, busted or were frozen are recognized by replaying the round's
    /// events, which an edit to their hands does not change, and land in
    /// FinishedPlayers.
    ///
    /// Unlike for hit-or-stand, the cards a deal3's target set aside and gives
    /// out afterwards are aims of theirs, since they choose who receives them,
    /// and so is a forced first hit's card. Every aim the engine asks for is
    /// reconstructed, including those with a single candidate, which say
    /// nothing about the player, but for the two a deal3's target is asked when
    /// they pass on two second chances during its flips: the timeline shows who
    /// took them but not which was passed first, which decides who could take
    /// the second, so neither aim is given.
    /// </summary>
    let public Observe (scan: Scan) (instant: Instant) : Scan * Observation list =
        let (Scan(finished, round, turns, pending, previous)) = scan
        let owed = Owes scan

        let voluntary =
            match instant.Event with
            | Froze(source, _)
            | Dealt3(source, _, _) -> owed source = 0
            | _ -> true

        let pending' =
            match instant.Event with
            | Froze(source, _) when not voluntary -> pending |> Map.add source (owed source - 1)
            | Dealt3(source, target, cards) ->
                let spent =
                    if voluntary then
                        pending
                    else
                        pending |> Map.add source (owed source - 1)

                let debt = (spent |> Map.tryFind target |> Option.defaultValue 0) + SetAsides cards
                spent |> Map.add target debt
            | _ -> pending

        let turns' =
            match Decider instant.Event with
            | Some(name, _) when voluntary ->
                let taken = turns |> Map.tryFind name |> Option.defaultValue 0u
                turns |> Map.add name (taken + 1u)
            | _ -> turns

        let decided (before: Instant) : Observation option =
            Decider instant.Event
            |> Option.filter (fun _ -> voluntary)
            |> Option.filter (fun (name, _) -> Map.find name turns' > 1u)
            |> Option.bind (fun (name, choice) ->
                before.Players
                |> List.tryFind (fun player -> player.Name = name)
                |> Option.map (fun actor ->
                    let others, finishedPlayers =
                        before.Players
                        |> List.filter (fun player -> player.Name <> name)
                        |> List.partition (InRound scan)

                    Asked round turns' (Choice1Of2 choice) actor others finishedPlayers (before.Deck, before.Discards)
                )
            )

        let finished', round', turns'', pending'' =
            match instant.Event with
            | Stood name
            | Busted(name, _) -> Set.add name finished, round, turns', pending'
            | Froze(_, target) -> Set.add target finished, round, turns', pending'
            | Dealt3(_, target, _) when
                instant.Players
                |> List.exists (fun player -> player.Name = target && Hand.IsBust player.Hand)
                ->
                Set.add target finished, round, turns', pending'
            | RoundEnded _ -> Set.empty, round + 1u, Map.empty, Map.empty
            | _ -> finished, round, turns', pending'

        let observations =
            match previous with
            | None -> []
            | Some before -> Option.toList (decided before) @ Aims scan turns' before instant

        Scan(finished', round', turns'', pending'', Some instant), observations

    /// <summary>
    /// Extracts every question a timeline shows a player was asked, every
    /// voluntary hit-or-stand decision and every aim, in the order the engine
    /// asked them: the scan applied instant by instant, with Observe's
    /// semantics.
    ///
    /// Observations are produced asynchronously as instants arrive, so a live
    /// game can be observed while it plays and unbounded timelines work.
    /// Re-enumerating replays the timeline from the start.
    /// </summary>
    let public FromTimeline (timeline: Timeline) : AsyncSeq<Observation> =
        timeline
        |> AsyncSeq.scan (fun (scan, _) instant -> Observe scan instant) (Start, [])
        |> AsyncSeq.collect (snd >> AsyncSeq.ofSeq)

    /// <summary>
    /// Extracts and pools the observations of many timelines, e.g. every
    /// persisted game a household has played.
    /// </summary>
    let public FromTimelines (timelines: Timeline seq) : AsyncSeq<Observation> =
        timelines |> AsyncSeq.ofSeq |> AsyncSeq.collect FromTimeline
