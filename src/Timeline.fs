namespace Flip7

open FSharp.Control

/// <summary>
/// A timeline is the full history of a game: one instant per event, produced
/// asynchronously so consumers can render or persist instants as they arrive
/// rather than waiting for the whole game to finish.
/// </summary>
type public Timeline = AsyncSeq<Instant>

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

    // What a decider is shown of the game so far: every instant produced before
    // its question, oldest first, as the first count instants of a buffer that
    // is only ever added to, so it reads in order with nothing to reverse.
    // Appending to the newest view adds to the buffer, and appending to any
    // older one copies. Read by index up to its count, a view never changes
    // once given, however long it is kept; the buffer's own enumerator would
    // throw once the buffer grows.
    type private History(buffer: ResizeArray<Instant>, count: int) =
        member _.Append(instant: Instant) : History =
            if count = buffer.Count then
                buffer.Add instant
                History(buffer, count + 1)
            else
                let copy = buffer.GetRange(0, count)
                copy.Add instant
                History(copy, count + 1)

        member this.AppendAll(instants: Instant list) : History =
            List.fold (fun (soFar: History) instant -> soFar.Append instant) this instants

        interface Instant seq with
            member _.GetEnumerator() : Instant System.Collections.Generic.IEnumerator =
                (Seq.init count (fun index -> buffer[index])).GetEnumerator()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator() : System.Collections.IEnumerator = (this :> Instant seq).GetEnumerator()

    // Asks the chooser's targeting policy to pick one of the candidates, which
    // the caller has already narrowed to the legal targets and which is never
    // empty. Returns the pick's position together with the engine's own value
    // for that player, so a decider answering with a stale Player cannot write
    // an old hand or score back into the game.
    let private ChooseTarget
        (decide: Targeting -> Strategy.TargetDecider)
        (history: History)
        (ask: Strategy.Ask)
        (chooser: Player)
        (candidates: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<int * Player> = async {
        assert (not (List.isEmpty candidates))

        let! chosen = decide chooser.Targeting history ask chooser candidates finished decks

        match candidates |> List.tryFindIndex (fun candidate -> candidate.Name = chosen.Name) with
        | Some index -> return index, List.item index candidates
        | None ->
            let legal =
                candidates |> List.map (fun candidate -> candidate.Name) |> String.concat ", "

            return invalidOp $"{chooser.Name} aimed {ask} at {chosen.Name}, which is not among: {legal}"
    }

    let private FreezePlayer
        (decide: Targeting -> Strategy.TargetDecider)
        (history: History)
        (source: Player)
        (card: Card)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<Instant list * Player list * Player list * (Deck * Deck)> = async {
        let! index, target =
            ChooseTarget decide history Strategy.Ask.WhoToFreeze source active finished decks

        return
            step (Froze(source.Name, target.Name)) (List.removeAt index active) (WithCard card target :: finished) decks
    }

    let private GiveAwaySecondChance
        (decide: Targeting -> Strategy.TargetDecider)
        (history: History)
        (card: Card)
        (holder: Player)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<string option * Player list * (Deck * Deck)> = async {
        let candidates =
            active
            |> List.indexed
            |> List.where (fun (_, player) -> player.Name <> holder.Name && not (HasSecondChance player))

        match candidates with
        | [] ->
            let deck, discards = decks
            return None, active, (deck, Deck.Increment discards card)
        | _ ->
            let! position, recipient =
                ChooseTarget
                    decide
                    history
                    Strategy.Ask.WhoReceivesSecondChance
                    holder
                    (List.map snd candidates)
                    finished
                    decks

            let index, _ = List.item position candidates
            return Some recipient.Name, List.updateAt index (WithCard card recipient) active, decks
    }

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
    // - with a nested deal3 resolving recursively. The history is every instant
    //   produced before the deal3 began: a second chance passed during the
    //   flips is aimed before the deal3's own instant exists, while a set-aside
    //   card is aimed after it and after every set-aside resolved before it.
    let rec private ResolveDeal3
        (random: System.Random)
        (decide: Targeting -> Strategy.TargetDecider)
        (history: History)
        (source: string)
        (targetIndex: int)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<Instant list * Player list * Player list * (Deck * Deck)> = async {

        let dealtTo = List.item targetIndex active
        let targetName = dealtTo.Name

        let rec Flip
            (remaining: uint)
            (flipped: Card list)
            (setAsides: Card list)
            (active: Player list)
            (decks: Deck * Deck)
            : Async<bool * Card list * Card list * Player list * (Deck * Deck)> = async {
            let target = List.item targetIndex active

            if remaining = 0u || Hand.HasFlip7Bonus target.Hand then
                return false, flipped, setAsides, active, decks
            else

            let decks', card = Deck.Draw1With random decks
            let deck', discards' = decks'
            let flipped' = flipped @ [ card ]

            match card with
            // Parked in the target's hand (harmless: action cards do not score
            // or bust) until the flips finish, then resolved below
            | ActionCard Card.Freeze
            | ActionCard Card.Deal3 ->
                let active' = List.updateAt targetIndex (WithCard card target) active
                return! Flip (remaining - 1u) flipped' (setAsides @ [ card ]) active' decks'

            | ActionCard Card.SecondChance when not (HasSecondChance target) ->
                let active' = List.updateAt targetIndex (WithCard card target) active
                return! Flip (remaining - 1u) flipped' setAsides active' decks'

            | ActionCard Card.SecondChance ->
                let! _, active', decks'' =
                    GiveAwaySecondChance decide history card target active finished decks'
                return! Flip (remaining - 1u) flipped' setAsides active' decks''

            | ValueCard _
            | ModifierCard _ ->
                let isBust, reducedHand, removedCards = Hand.Reduce(card :: target.Hand)
                let decks'' = deck', List.fold Deck.Increment discards' removedCards
                let active' = List.updateAt targetIndex { target with Hand = reducedHand } active

                if isBust then
                    return true, flipped', setAsides, active', decks''
                else
                    return! Flip (remaining - 1u) flipped' setAsides active' decks''
        }

        let! isBust, flipped, setAsides, active', decks' = Flip 3u [] [] active decks
        let event = Dealt3(source, targetName, flipped)

        if isBust then
            let target' = List.item targetIndex active'
            let active'' = List.removeAt targetIndex active'
            let finished' = target' :: finished
            return step event active'' finished' decks'
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

            // The set-aside cards are given out by whoever flipped them, and by
            // now that player may have moved: an earlier set-aside can have
            // frozen them out of the round. Recover them from wherever they sit
            // now, the way unpark does.
            let chooser (active: Player list) (finished: Player list) : Player =
                active @ finished
                |> List.tryFind (fun player -> player.Name = targetName)
                |> Option.defaultValue dealtTo

            let ResolveSetAside
                ((instants, active, finished, decks): Instant list * Player list * Player list * (Deck * Deck))
                (setAside: Card)
                : Async<Instant list * Player list * Player list * (Deck * Deck)> = async {
                let roundEnded =
                    List.isEmpty active
                    || active |> List.exists (fun player -> Hand.HasFlip7Bonus player.Hand)

                // A bust discards the flipper's remaining set-asides
                // unresolved, and they can still bust here: an earlier
                // set-aside deal3 given to themselves can end their round
                // between one set-aside and the next. Leaving the card parked
                // in their hand is what discards it, since hands go to the
                // discards when the round ends. Being frozen is not busting, so
                // a frozen flipper still gives out the rest.
                let flipperBusted =
                    active @ finished
                    |> List.exists (fun player -> player.Name = targetName && Hand.IsBust player.Hand)

                // The instants this deal3 has produced so far are not yet
                // yielded, but they come before the question all the same
                let history = history.AppendAll instants

                match setAside with
                | _ when roundEnded || flipperBusted -> return instants, active, finished, decks
                | ActionCard Card.Freeze ->
                    let active, finished = unpark active finished setAside

                    let! more, active', finished', decks' =
                        FreezePlayer decide history (chooser active finished) setAside active finished decks

                    return instants @ more, active', finished', decks'
                | ActionCard Card.Deal3 ->
                    let active, finished = unpark active finished setAside
                    let deck, discards = decks
                    let decks = deck, Deck.Increment discards setAside
                    let! index, _ =
                        ChooseTarget
                            decide
                            history
                            Strategy.Ask.WhoReceivesDeal3
                            (chooser active finished)
                            active
                            finished
                            decks

                    let! nested, active', finished', decks' =
                        ResolveDeal3 random decide history targetName index active finished decks

                    return instants @ nested, active', finished', decks'
                | _ -> return invalidOp $"Unexpected set-aside card: {setAside}"
            }

            return!
                setAsides
                |> AsyncSeq.ofSeq
                |> AsyncSeq.foldAsync ResolveSetAside (step event active' finished decks')
    }

    // Resolves a single card drawn by the current player on a hit: adds it to
    // the hand, gives away or discards an unkeepable second chance, freezes a
    // player, busts on a duplicate, or hands off to the deal3 resolver. Returns
    // the instants produced and the active players, finished players, and decks
    // to continue from. The decks passed in are already past the draw.
    //
    // Resolution is asynchronous because a card can pose a choice - who to
    // freeze, who receives a deal3, who is passed a second chance - and whoever
    // answers may need to be awaited: the targeting decider is asked who
    // receives the card, and a human decider can suspend the game there. The
    // history is every instant produced before the draw.
    let private ResolveDraw
        (random: System.Random)
        (decide: Targeting -> Strategy.TargetDecider)
        (history: History)
        (current: Player)
        (others: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        (card: Card)
        : Async<Instant list * Player list * Player list * (Deck * Deck)> = async {
        let deck, discards = decks

        // Adds a card that can never bust the player to their hand and passes
        // the turn
        let keep () =
            step (Drew(current.Name, card)) (others @ [ WithCard card current ]) finished decks

        match card with
        | ModifierCard _ -> return keep ()
        | ActionCard Card.SecondChance when not (HasSecondChance current) -> return keep ()

        // Can never bust on a second chance card, but you also can't hold two
        // of them at the same time: give it away, or discard it if no one can
        // hold it
        | ActionCard Card.SecondChance ->
            let! recipient, active', decks' =
                GiveAwaySecondChance decide history card current (others @ [ current ]) finished decks

            let event =
                match recipient with
                | Some name -> SecondChancePassed(current.Name, name)
                | None -> SecondChanceDiscarded current.Name

            return step event active' finished decks'

        // Can never bust on a freeze card, just pick someone to freeze
        // (possibly yourself); they bank their points and are done for the
        // round
        | ActionCard Card.Freeze ->
            return! FreezePlayer decide history current card (others @ [ current ]) finished decks

        // Can bust on a value card, so reduce the hand to see whether the
        // player is done
        | ValueCard _ ->
            let isBust, reducedHand, removedCards = Hand.Reduce(card :: current.Hand)
            let decks' = deck, List.fold Deck.Increment discards removedCards
            let current' = { current with Hand = reducedHand }

            if isBust then
                return step (Busted(current.Name, card)) others (current' :: finished) decks'
            else
                return step (Drew(current.Name, card)) (others @ [ current' ]) finished decks'

        // The deal3 card itself is used up immediately; the receiving player
        // (possibly yourself) then flips up to three cards
        | ActionCard Card.Deal3 ->
            let decks' = deck, Deck.Increment discards card
            let rotated = others @ [ current ]
            let! index, _ =
                ChooseTarget decide history Strategy.Ask.WhoReceivesDeal3 current rotated finished decks'
            return! ResolveDeal3 random decide history current.Name index rotated finished decks'
    }

    // Plays the rest of a round, asking each hit or stand of hitOrStand and
    // each aim of target, which ContinueWith gives every question's Random and
    // the history that led to it. The history is every instant produced before
    // the round, and every instant the round yields joins it before the next
    // question is asked.
    let rec private GoonSession
        (random: System.Random)
        (hitOrStand: Strategy -> Strategy.HitOrStandDecider)
        (target: Targeting -> Strategy.TargetDecider)
        (history: History)
        (round: uint)
        (turnsTaken: Map<string, uint>)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : AsyncSeq<Instant> =

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
        | Some winner, _ -> AsyncSeq.singleton (MakeInstant (Flip7Achieved winner.Name) active finished decks)

        // Base case: everyone has stood, busted, or been frozen
        | None, [] -> AsyncSeq.empty

        | None, current :: others ->
            let turn = (turnsTaken |> Map.tryFind current.Name |> Option.defaultValue 0u) + 1u
            let turnsTaken = Map.add current.Name turn turnsTaken

            asyncSeq {
                // The first time play comes around the table each player is
                // dealt a card before they may choose, and a player may never
                // stand with no cards, so the opening turn is always a forced
                // hit. Being no question, it draws no Random of its own
                let! choice =
                    if turn = 1u then
                        async.Return Strategy.Hit
                    else
                        hitOrStand current.Strategy history round turn current others finished decks

                match choice with
                | Strategy.Stand ->
                    let finished' = current :: finished
                    let stood = MakeInstant (Stood current.Name) others finished' decks
                    yield stood

                    yield!
                        GoonSession
                            random
                            hitOrStand
                            target
                            (history.Append stood)
                            round
                            turnsTaken
                            others
                            finished'
                            decks

                | Strategy.Hit ->
                    let decks', card = Deck.Draw1With random decks

                    let! instants, active', finished', decks'' =
                        ResolveDraw random target history current others finished decks' card

                    for instant in instants do
                        yield instant

                    yield!
                        GoonSession
                            random
                            hitOrStand
                            target
                            (history.AppendAll instants)
                            round
                            turnsTaken
                            active'
                            finished'
                            decks''
            }

    // Names are the engine's identity key: turn counts, round scores, unparking
    // a set-aside card during a deal3, and matching a targeting decider's
    // answer back to a seat are all done by name, and a duplicate corrupts all
    // four quietly rather than failing. Checked where a game or a continuation
    // is set up, and eagerly rather than inside the timeline, so the error
    // lands where the lineup was written instead of wherever it first came to
    // be enumerated.
    let private EnsureDistinctNames (players: Player list) : unit =
        let duplicated =
            players
            |> List.countBy (fun player -> player.Name)
            |> List.where (fun (_, seats) -> seats > 1)
            |> List.map fst

        if not (List.isEmpty duplicated) then
            let duplicated = String.concat ", " duplicated

            invalidArg
                (nameof players)
                $"Player names must be unique, but these are seated more than once: {duplicated}"

    // A targeting that takes sides more than Targeting.SidesLimit deep would
    // raise at the first aim the engine's decider came to, so it is refused
    // where a game or a continuation is set up instead, and eagerly for the
    // same reason as a duplicate name.
    let private EnsureSidesWithinLimit (players: Player list) : unit =
        let tooDeep =
            players
            |> List.where (fun player -> Targeting.TakesSidesTooDeep player.Targeting)
            |> List.map (fun player -> player.Name)

        if not (List.isEmpty tooDeep) then
            let tooDeep = String.concat ", " tooDeep

            invalidArg
                (nameof players)
                $"A targeting may take sides at most {Targeting.SidesLimit} deep, but these players take them deeper: {tooDeep}"

    // Every player must be named in the seating exactly once: a player it left
    // out would never be dealt into a later round, and a name it repeated would
    // be dealt in twice. Checked where a continuation is set up, and eagerly
    // rather than inside the timeline, so the error lands where the seating was
    // written instead of wherever it first came to be enumerated.
    let private EnsureAllSeated (seating: string list) (players: Player list) : unit =
        let names = players |> List.map (fun player -> player.Name)

        if List.sort seating <> List.sort names then
            let unseated = names |> List.where (fun name -> not (List.contains name seating))

            let strangers =
                seating
                |> List.where (fun name -> not (List.contains name names))
                |> List.distinct

            let repeated =
                seating
                |> List.countBy id
                |> List.where (fun (_, seats) -> seats > 1)
                |> List.map fst

            let problems =
                [
                    "left out", unseated
                    "not at the table", strangers
                    "seated more than once", repeated
                ]
                |> List.where (fun (_, names) -> not (List.isEmpty names))
                |> List.map (fun (problem, names) -> $"""{problem}: {String.concat ", " names}""")
                |> String.concat "; "

            invalidArg (nameof seating) $"The seating must name every player once, but {problems}"

    /// <summary>
    /// Continues a game from a mid-round state: the history, every instant that
    /// led to the state, oldest first; the seating, every player's name in seat
    /// order; the round number; how many turns each player has taken; the
    /// active players in rotation order (the next actor at the head); the
    /// players already finished this round; and the decks. The first round
    /// starts at the head of the seating and each round after it one seat
    /// further round the table, so the round being continued started round - 1
    /// seats along, and the seating is what scores it and deals the rounds that
    /// follow. The round plays out from there and whole rounds follow until a
    /// player first reaches 200 points at a RoundEnded, which is the last
    /// instant of the timeline. A player whose turn count is zero is dealt a
    /// forced first hit, so a continuation should count at least one turn for
    /// anyone already holding cards. Each question shows its decider the
    /// history followed by every instant the continuation produced before it,
    /// but the timeline yields only the continuation's own instants. A decider
    /// that reads the history enumerates it afresh each time, so pass a
    /// snapshot, such as a list or an array, rather than a timeline, which
    /// would play its game again. Players sharing a name, a seating that is not
    /// the table, or a targeting that takes sides more than three deep are
    /// refused with an ArgumentException at the call, before anything is
    /// enumerated.
    /// </summary>
    let public ContinueWith
        (random: System.Random)
        (decide: Strategy.Decider)
        (history: Instant seq)
        (seating: string list)
        (startingRound: uint)
        (startingTurnsTaken: Map<string, uint>)
        (startingActive: Player list)
        (startingFinished: Player list)
        (startingDecks: Deck * Deck)
        : Timeline =
        let everyone = startingActive @ startingFinished
        EnsureDistinctNames everyone
        EnsureSidesWithinLimit everyone
        EnsureAllSeated seating everyone

        if List.isEmpty everyone then
            AsyncSeq.empty
        else

        let along = int ((startingRound + uint seating.Length - 1u) % uint seating.Length)
        let seated =
            List.skip along seating @ List.take along seating
            |> List.map (fun name -> everyone |> List.find (fun player -> player.Name = name))

        // Every question is asked with a Random of its own, seeded by a single
        // draw from the game's as it is asked, so that what a decider draws
        // cannot shift the cards the game deals. These draw each time they are
        // called rather than once where they are passed on, so a card that
        // poses no question draws nothing and one that poses several draws once
        // for each. The engine keeps only what the continuation has produced,
        // so they also put the history that led here in front of it.
        let hitOrStand (strategy: Strategy) (seen: Instant seq) =
            decide.HitOrStand (System.Random(random.Next())) strategy (Seq.append history seen)

        let target (targeting: Targeting) (seen: Instant seq) =
            decide.Target (System.Random(random.Next())) targeting (Seq.append history seen)

        // A loop rather than recursion so an arbitrarily long game cannot
        // accumulate nested asyncSeq appends
        asyncSeq {
            let mutable round = startingRound
            let mutable turnsTaken = startingTurnsTaken
            let mutable active = startingActive
            let mutable finished = startingFinished

            // The round's turn order, which keeps scoring independent of the
            // order players finish in
            let mutable roster = seated
            let mutable decks = startingDecks
            let mutable gameOver = false

            // Every instant yielded so far
            let mutable produced = History(ResizeArray(), 0)

            while not gameOver do
                let mutable lastInstant = None

                for instant in GoonSession random hitOrStand target produced round turnsTaken active finished decks do
                    lastInstant <- Some instant
                    produced <- produced.Append instant
                    yield instant

                let finalPlayers, (deck, discards) =
                    lastInstant
                    |> Option.map (fun instant -> instant.Players, (instant.Deck, instant.Discards))
                    |> Option.defaultValue (roster, decks)

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
                    roster
                    |> List.map (fun player -> {
                        player with
                            FirmScore = player.FirmScore + Map.find player.Name scores
                            Hand = List.empty
                    })

                let roundEnded = {
                    Event = RoundEnded scores
                    Players = scored
                    Deck = deck
                    Discards = discards'
                }

                produced <- produced.Append roundEnded
                yield roundEnded

                if scored |> List.forall (fun player -> player.FirmScore < 200u) then
                    let rotated =
                        match scored with
                        | [] -> []
                        | leader :: rest -> rest @ [ leader ]

                    round <- round + 1u
                    turnsTaken <- Map.empty
                    roster <- rotated
                    active <- rotated
                    finished <- []
                    decks <- deck, discards'
                else
                    gameOver <- true
        }

    /// <summary>
    /// Simulates a full game like SimulateWith, but every hit or stand decision
    /// and every aim is routed through the given decider, so Custom strategies
    /// and ChoosesExternally targeting can be answered by a human at the
    /// terminal. Pulling the timeline drives the game, so a decider may await
    /// (a key press, a network message) and the game simply pauses there
    /// without holding a thread. Every question shows its decider the game so
    /// far, from the TableSeated instant on, and comes with a Random of its own
    /// seeded from the game's, so a decider is never handed the game's Random
    /// and cannot shift the cards the game deals by drawing from its own. The
    /// seed it comes with is the game's own next draw, so in a seeded game it
    /// gives the game's seed away to a decider that sets out to work it back.
    /// Players sharing a name, or a targeting that takes sides more than three
    /// deep, are refused with an ArgumentException at the call, before anything
    /// is enumerated.
    /// </summary>
    let public SimulateWithDecider
        (random: System.Random)
        (decide: Strategy.Decider)
        (players: list<string * Strategy * Targeting>)
        : Timeline =
        let startingPlayers =
            players
            |> List.map (fun (name, strategy, targeting) -> Player.Make(name, strategy, targeting = targeting))

        EnsureDistinctNames startingPlayers
        EnsureSidesWithinLimit startingPlayers

        let seated = {
            Event = TableSeated
            Players = startingPlayers
            Deck = Deck.Full
            Discards = Deck.Empty
        }

        asyncSeq {
            yield seated

            let seating = startingPlayers |> List.map (fun player -> player.Name)
            yield! ContinueWith random decide [ seated ] seating 1u Map.empty startingPlayers [] (Deck.Full, Deck.Empty)
        }

    /// <summary>
    /// Simulates a full game using the given source of randomness and returns
    /// its timeline lazily: one instant per event, with a RoundEnded instant
    /// closing out every round. The last instant of the timeline is the
    /// RoundEnded in which a player first reaches 200 points. A seeded random
    /// makes the timeline reproducible, but note that the timeline is lazy:
    /// enumerating it advances the random, so enumerate it once (or cache it)
    /// when reproducibility matters. The game draws from the random for every
    /// card it deals and once for every question it asks, seeding a Random of
    /// the question's own with that draw, so however much a strategy draws from
    /// its Random it never shifts the cards the game deals.
    /// </summary>
    let public SimulateWith (random: System.Random) (players: list<string * Strategy * Targeting>) : Timeline =
        SimulateWithDecider random Strategy.Decider.Default players

#if !FABLE_COMPILER
    /// <summary>
    /// Simulates a full game without threading a source of randomness.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Timeline.SimulateWith instead")>]
    let public Simulate (players: list<string * Strategy * Targeting>) : Timeline =
        SimulateWith System.Random.Shared players
#endif
