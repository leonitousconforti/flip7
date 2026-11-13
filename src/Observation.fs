namespace Flip7

open FSharp.Control

/// <summary>
/// One question a timeline shows a player was asked, with their answer and the
/// table exactly as the engine put the question to them. Choice is Choice1Of2
/// for a hit or stand decision, and Choice2Of2 for an aim: what was being aimed
/// and the name of the player it was given to.
///
/// For a hit or stand decision OtherPlayers are the players still in the round
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
/// already taken, which may be none. After an edit the turns are the engine's,
/// which counts everyone still in the round as having taken as many as the
/// asker had.
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
    // resolves afterwards is not their hit or stand decision. Not
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
        | Dealt3(source, _, _, _) -> Some(source, Strategy.Hit)
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
    /// A step the engine took to produce an instant, as the scan replays it.
    /// Every question, answered or not, costs the game's Random a call.
    /// </summary>
    [<RequireQualifiedAccess>]
    type internal Step =
        /// A hit or stand question and its answer, which every turn asks but a
        /// player's forced first one of a round
        | Asked of Observation
        /// A hit or stand question the play editor was opened at instead of
        /// answering it, which the Edited instant names the asker of
        | Interrupted of Name: string
        /// A card drawn from its pile: the deck, or the discards once the deck
        /// ran out and they were shuffled in as the deck
        | Drawn of Card: Card * Pile: Deck
        /// An aim and the player it was given to
        | Aimed of Observation

    /// <summary>
    /// What the engine did to produce an instant, as the scan replays it from
    /// the instant before: its steps, in the order the engine took them, and
    /// whether the engine could have produced the instant by taking them. The
    /// replay is as lenient as Observe, which gives its observations either
    /// way: it draws a card whose pile holds no copy and flips every card a
    /// deal3 records.
    /// </summary>
    type internal Replay = {
        Instant: Instant
        Steps: Step list
        /// <summary>
        /// Whether the engine could have produced the instant from the ones
        /// before it by taking the steps, which are then the calls it made on
        /// the game's Random to produce it. A game starts from a TableSeated,
        /// so the first instant is possible when it is one and no later one is,
        /// and a Stood that asked no question is not, since a forced first turn
        /// always hits. An edit is possible whatever table it shows. Every
        /// instant must seat each player under a name of their own, as the
        /// engine does, since the replay finds players by name, and seat
        /// whoever the engine asks a question of where the engine would have
        /// them: a hit or stand at the table, and the drawer of a card at the
        /// head of the turn order, or the player who set it aside during a
        /// deal3 anywhere at the table. A deal3 must record just the second
        /// chances its flips pass on, each to somebody who could take it, every
        /// card must be drawn from a pile holding a copy of it, a deal3's flips
        /// must stop just where the engine stops them, and the deck the instant
        /// shows must be the one its draws leave, and for a deal3 the discards
        /// too, the only event whose discards the replay follows. Where it is
        /// not, the steps leave out every question the timeline does not seat
        /// its player for, and every pass of a deal3 whose recorded passes the
        /// engine could not have made.
        /// </summary>
        Possible: bool
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
            Last: Replay option

    /// <summary>
    /// The scan before any instant: round one, nobody finished, no turns taken.
    /// </summary>
    let public Start: Scan = Scan(Set.empty, 1u, Map.empty, Map.empty, None)

    /// <summary>
    /// How many turns each player has taken in the scan's current round.
    /// Matches the engine's per-round counter, because every turn produces
    /// exactly one decision event and an Edited instant counts everyone still
    /// in the round as having taken as many turns as the asker had, as the play
    /// editor's fork continues the game.
    /// </summary>
    let internal TurnsTaken (scan: Scan) : Map<string, uint> =
        let (Scan(_, _, turns, _, _)) = scan
        turns

    /// <summary>
    /// The round the scan is in, counting from one.
    /// </summary>
    let internal RoundOf (scan: Scan) : uint =
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

    /// <summary>
    /// The replay of the instant the scan took in last, or None before any. The
    /// first instant has no instant before it to replay from, so its replay
    /// takes no steps.
    /// </summary>
    let internal ReplayOf (scan: Scan) : Replay option =
        let (Scan(_, _, _, _, last)) = scan
        last

    // Whether an instant seats every player under a name of their own, as the
    // engine does
    let private HasDistinctNames (instant: Instant) : bool =
        instant.Players |> List.distinctBy (fun player -> player.Name) |> List.length = instant.Players.Length

    // What the engine did to produce an instant, given the scan of every
    // instant before it, whether the instant's event was its source's own
    // choice rather than a card they set aside during a deal3 being given out,
    // the turns taken counting the instant's own, and the instant before it.
    // Every question comes with the table exactly as the engine put it. The
    // preceding instant is the table a hit or stand question was asked at, and
    // the one an aim was asked at but for what the card's resolution changed
    // before asking: a drawn card leaves the deck, a deal3 card goes to the
    // discards, a set-aside card leaves the hand it was parked in, and a second
    // chance the target of a deal3 flips but cannot keep is aimed partway
    // through the flips, which are replayed card by card from the instant
    // before the deal3. The engine offers a freeze or a deal3 to everyone in
    // the round in turn order, the drawer moved behind the others, and a second
    // chance to everyone in it but its holder who has none. The turn order is
    // the instant's own, but at a RoundEnded the next round starts one seat
    // further round the table. The steps are built with a hole, None, for every
    // question the engine asked that the timeline does not seat its player for
    // where the engine would have them, and for every pass of a deal3 whose
    // recorded passes the engine could not have made, which leaves the instant
    // impossible
    let private Replayed
        (scan: Scan)
        (voluntary: bool)
        (turns: Map<string, uint>)
        (before: Instant)
        (instant: Instant)
        : Replay =
        let secondChance = ActionCard Card.SecondChance
        let holdsSecondChance (player: Player) = List.contains secondChance player.Hand

        // Draws a card the way Deck.DrawWith does, shuffling the discards in as
        // the deck once it is empty: the step, and the decks after it
        let drawn ((deck, discards): Deck * Deck) (card: Card) : Step * (Deck * Deck) =
            let pile, discards =
                if Deck.IsEmpty deck then
                    discards, Deck.Empty
                else
                    deck, discards

            Step.Drawn(card, pile), (Deck.Decrement pile card, discards)

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
        // they aim it, wherever they sit now: who aims it, with the players in
        // the round and those finished as they aim it, if they sit at the table
        // at all
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

        // The hit or stand question the engine asked before the instant's
        // event, observed at the table it was asked at. An edit is made at a
        // question in place of answering it, and a forced first turn asks none,
        // nor does giving out a set-aside card
        let question: Step option list =
            match instant.Event, Decider instant.Event with
            | Edited asker, _ -> [ Some(Step.Interrupted asker) ]
            | _, Some(name, choice) when voluntary && Map.find name turns > 1u -> [
                before.Players
                |> List.tryFind (fun player -> player.Name = name)
                |> Option.map (fun actor ->
                    let others, finishedPlayers =
                        before.Players
                        |> List.filter (fun player -> player.Name <> name)
                        |> List.partition (InRound scan)

                    Step.Asked(Asked (RoundOf scan) turns (Choice1Of2 choice) actor others finishedPlayers decks)
                )
              ]
            | _ -> []

        // The flips of a deal3, replayed card by card as ResolveDeal3 makes
        // them from the decks it was aimed at, among the players of the
        // rotation it was aimed in and those finished: each a draw from the
        // decks as they stand, number and modifier cards reduced into the
        // target's hand with whatever the reduction removes discarded, a second
        // chance kept, passed on with an aim while somebody can take one, or
        // discarded, and freezes and deal3s set aside in the hand, where they
        // neither score, bust nor count toward a flip7. The engine stops
        // flipping after the third card, on a bust, or on a flip7. Each second
        // chance passed on is aimed at the next player the deal3 records as
        // passed one, among those in the rotation but the target who hold none
        // and were passed none before it, and is a hole unless every one can
        // be. Returns the steps, the decks they leave, and whether the engine
        // flips every card the deal3 records and no more and passes on just the
        // second chances it records, to whom it records
        let flipped
            (rotation: Player list)
            (finished: Player list)
            (decks: Deck * Deck)
            (target: string)
            (cards: Card list)
            (passed: string list)
            : Step option list * (Deck * Deck) * bool =
            let takers =
                rotation
                |> List.where (fun player -> player.Name <> target && not (holdsSecondChance player))

            // The cards left flipped from the target as the flips so far left
            // them, the decks, how many more cards the engine would flip, every
            // second chance passed on so far, newest first, with the target and
            // the decks as it was aimed, whether the engine was still flipping
            // at every card so far, and the steps so far, newest first, each
            // pass a hole; then the steps, the decks, the passes, oldest first,
            // and whether the engine flips just these cards. A loop, since a
            // timeline the engine did not play can record any number of cards
            let rec flip
                (dealtTo: Player)
                (decks: Deck * Deck)
                (remaining: int)
                (passes: (Player * (Deck * Deck)) list)
                (flipping: bool)
                (steps: Step option list)
                (cards: Card list)
                : Step option list * (Deck * Deck) * (Player * (Deck * Deck)) list * bool =
                let stopped = remaining <= 0 || Hand.HasFlip7Bonus dealtTo.Hand

                match cards with
                | [] -> List.rev steps, decks, List.rev passes, flipping && stopped
                | card :: rest ->
                    let step, (deck, discards) = drawn decks card

                    let dealtTo', decks', remaining', passes', aims =
                        match card with
                        | ActionCard Card.SecondChance when holdsSecondChance dealtTo ->
                            if passes.Length < takers.Length then
                                dealtTo,
                                (deck, discards),
                                remaining - 1,
                                (dealtTo, (deck, discards)) :: passes,
                                [ None ]
                            else
                                dealtTo, (deck, Deck.Increment discards card), remaining - 1, passes, []
                        | ActionCard _ ->
                            { dealtTo with Hand = card :: dealtTo.Hand }, (deck, discards), remaining - 1, passes, []
                        | ValueCard _
                        | ModifierCard _ ->
                            let isBust, reduced, removed = Hand.Reduce(card :: dealtTo.Hand)

                            { dealtTo with Hand = reduced },
                            (deck, List.fold Deck.Increment discards removed),
                            (if isBust then 0 else remaining - 1),
                            passes,
                            []

                    flip dealtTo' decks' remaining' passes' (flipping && not stopped) (aims @ Some step :: steps) rest

            match rotation |> List.tryFind (fun player -> player.Name = target) with
            | None -> [], decks, false
            | Some dealtTo ->
                let steps, decks', passes, stops = flip dealtTo decks 3 [] true [] cards

                // Each pass aimed at whom the deal3 records it went to, among
                // those who could still take one, or None unless every one is
                let aims: Observation list option =
                    if passes.Length <> passed.Length then
                        None
                    else
                        List.zip passes passed
                        |> List.fold
                            (fun aims ((passer, decks), recipient) ->
                                aims
                                |> Option.bind (fun (aims, offered: Player list) ->
                                    if offered |> List.exists (fun player -> player.Name = recipient) then
                                        Some(
                                            aimed
                                                Strategy.Ask.WhoReceivesSecondChance
                                                passer
                                                offered
                                                finished
                                                decks
                                                recipient
                                            :: aims,
                                            offered |> List.where (fun player -> player.Name <> recipient)
                                        )
                                    else
                                        None
                                )
                            )
                            (Some([], takers))
                        |> Option.map (fst >> List.rev)

                let steps =
                    match aims with
                    | None -> steps
                    | Some aims ->
                        steps
                        |> List.mapFold
                            (fun aims step ->
                                match step, aims with
                                | None, aim :: rest -> Some(Step.Aimed aim), rest
                                | step, aims -> step, aims
                            )
                            aims
                        |> fst

                steps, decks', stops && aims.IsSome

        let steps, decks', stopped =
            match instant.Event with
            | Drew(_, card)
            | Busted(_, card) ->
                let step, decks = drawn decks card
                [ Some step ], decks, true
            | SecondChanceDiscarded _ ->
                let step, decks = drawn decks secondChance
                [ Some step ], decks, true
            | SecondChancePassed(source, target) ->
                let step, decks = drawn decks secondChance

                let aim =
                    drawer source
                    |> Option.map (fun (current, rotation) ->
                        let takers =
                            rotation
                            |> List.where (fun player -> player.Name <> source && not (holdsSecondChance player))

                        Step.Aimed(aimed Strategy.Ask.WhoReceivesSecondChance current takers finished decks target)
                    )

                [ Some step; aim ], decks, true
            | Froze(source, target) when voluntary ->
                let step, decks = drawn decks (ActionCard Card.Freeze)

                let aim =
                    drawer source
                    |> Option.map (fun (current, rotation) ->
                        Step.Aimed(aimed Strategy.Ask.WhoToFreeze current rotation finished decks target)
                    )

                [ Some step; aim ], decks, true
            | Froze(source, target) ->
                let aim =
                    unparked source (ActionCard Card.Freeze)
                    |> Option.map (fun (chooser, active, finished) ->
                        Step.Aimed(aimed Strategy.Ask.WhoToFreeze chooser active finished decks target)
                    )

                [ aim ], decks, true
            | Dealt3(source, target, cards, passed) ->
                let deal3 = ActionCard Card.Deal3

                // The steps up to the aim, who aims the deal3, with the
                // rotation it is aimed in and those finished, if the timeline
                // seats them where the engine would have them, and the decks as
                // it is aimed: a drawn deal3 goes to the discards once drawn,
                // and a set-aside one once taken out of the hand it was parked
                // in
                let drew, aiming, decks =
                    if voluntary then
                        let step, (deck, discards) = drawn decks deal3

                        let aiming =
                            drawer source
                            |> Option.map (fun (current, rotation) -> current, rotation, finished)

                        [ Some step ], aiming, (deck, Deck.Increment discards deal3)
                    else
                        [], unparked source deal3, (before.Deck, Deck.Increment before.Discards deal3)

                match aiming with
                | None -> drew @ [ None ], decks, true
                | Some(chooser, rotation, finished) ->
                    let aim = aimed Strategy.Ask.WhoReceivesDeal3 chooser rotation finished decks target
                    let flips, decks, stopped = flipped rotation finished decks target cards passed
                    drew @ Some(Step.Aimed aim) :: flips, decks, stopped
            | Edited _ -> [], (instant.Deck, instant.Discards), true
            | TableSeated
            | Stood _
            | Flip7Achieved _
            | RoundEnded _ -> [], decks, true

        let steps = question @ steps
        let deck, discards = decks'

        // The engine seats the table only where a game starts, and a forced
        // first turn always hits
        let asked =
            match instant.Event with
            | TableSeated -> false
            | Stood _ -> not steps.IsEmpty
            | _ -> true

        // The engine puts every question to a player where the timeline seats
        // them, and passes every second chance on to somebody who can take it
        let placed = steps |> List.forall Option.isSome
        let steps = steps |> List.choose id

        let drawable =
            steps
            |> List.forall (fun step ->
                match step with
                | Step.Drawn(card, pile) -> pile |> Map.tryFind card |> Option.exists (fun held -> held > 0u)
                | Step.Asked _
                | Step.Interrupted _
                | Step.Aimed _ -> true
            )

        {
            Instant = instant
            Steps = steps
            Possible =
                asked
                && placed
                && HasDistinctNames instant
                && drawable
                && stopped
                && deck = instant.Deck
                && (not instant.Event.IsDealt3 || discards = instant.Discards)
        }

    /// <summary>
    /// Advances the scan by one instant, yielding every question the instant
    /// shows a player was asked, in the order the engine asked them: the hit or
    /// stand decision first, since the card a hit draws is drawn once the
    /// player has chosen, then the aims the card's resolution asked for. Each
    /// instant is a snapshot immediately after its event, so the preceding
    /// instant is the exact state a hit or stand decision was made from, and
    /// the table an aim was asked at but for what the card's resolution changed
    /// before asking: the drawn card leaves the deck, a deal3 card goes to the
    /// discards, a set-aside card leaves the hand it was parked in, and a
    /// second chance a deal3's target flips but cannot keep is aimed partway
    /// through the flips. A player's first turn of every round is a forced hit
    /// the engine never asks their strategy about - it is dealing, not a
    /// decision - and gives no hit or stand decision; the turn is reconstructed
    /// by counting the player's decision events, which matches the engine's
    /// count because every turn produces exactly one such event, and from an
    /// Edited instant on by counting them from as many turns as the asker had
    /// taken, which the play editor's fork counts for everyone still in the
    /// round. Froze and Dealt3 events that resolve a set-aside card from an
    /// earlier deal3 are the flipper giving out cards they were dealt, not a
    /// voluntary hit, and give none either.
    ///
    /// The OtherPlayers of a hit or stand decision are only the players still
    /// in the round, matching what the hit or stand decider receives: players
    /// who stood, busted or were frozen are recognized by replaying the round's
    /// events, which an edit to their hands does not change, and land in
    /// FinishedPlayers.
    ///
    /// Unlike for hit or stand, the cards a deal3's target set aside and gives
    /// out afterwards are aims of theirs, since they choose who receives them,
    /// and so is a forced first hit's card. Every aim the engine asks for is
    /// reconstructed, including those with a single candidate, which say
    /// nothing about the player, and every second chance a deal3's target
    /// passes on during its flips, which the deal3 records in the order they
    /// were passed.
    /// </summary>
    let public Observe (scan: Scan) (instant: Instant) : Scan * Observation list =
        let (Scan(finished, round, turns, pending, last)) = scan
        let owed = Owes scan

        let voluntary =
            match instant.Event with
            | Froze(source, _)
            | Dealt3(source, _, _, _) -> owed source = 0
            | _ -> true

        let pending' =
            match instant.Event with
            | Froze(source, _) when not voluntary -> pending |> Map.add source (owed source - 1)
            | Dealt3(source, target, cards, _) ->
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

        let replayed =
            match last with
            | None -> {
                Instant = instant
                Steps = []
                Possible = instant.Event.IsTableSeated && HasDistinctNames instant
              }
            | Some last -> Replayed scan voluntary turns' last.Instant instant

        let finished', round', turns'', pending'' =
            match instant.Event with
            | Stood name
            | Busted(name, _) -> Set.add name finished, round, turns', pending'
            | Froze(_, target) -> Set.add target finished, round, turns', pending'
            | Dealt3(_, target, _, _) when
                instant.Players
                |> List.exists (fun player -> player.Name = target && Hand.IsBust player.Hand)
                ->
                Set.add target finished, round, turns', pending'
            | RoundEnded _ -> Set.empty, round + 1u, Map.empty, Map.empty
            | Edited asker ->
                let taken = turns' |> Map.tryFind asker |> Option.defaultValue 0u

                let reset =
                    instant.Players
                    |> List.filter (InRound scan)
                    |> List.fold (fun turns (player: Player) -> turns |> Map.add player.Name taken) turns'

                finished, round, reset, pending'
            | _ -> finished, round, turns', pending'

        let observations =
            replayed.Steps
            |> List.choose (fun step ->
                match step with
                | Step.Asked observation
                | Step.Aimed observation -> Some observation
                | Step.Interrupted _
                | Step.Drawn _ -> None
            )

        Scan(finished', round', turns'', pending'', Some replayed), observations

    /// <summary>
    /// Extracts every question a timeline shows a player was asked, every
    /// voluntary hit or stand decision and every aim, in the order the engine
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
