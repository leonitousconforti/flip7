namespace Flip7

open FSharp.Control

/// <summary>
/// How far a rollout plays, and what it is worth when it stops. Played to the
/// end of the game a rollout answers the only question that finally matters,
/// but most of the answer is rounds that have nothing to do with the decision
/// being weighed, and that noise buries the decision's own effect: a hit early
/// on is worth about five points of win probability, which takes the better
/// part of a thousand rollouts to see through it. Stopped at the end of the
/// round, a rollout is worth the points it banked against the best of the
/// table - a smaller question, but a far quieter one - until somebody is close
/// enough to 200 that who wins is the question, and then the effect is stark
/// enough to read in a handful of games.
/// </summary>
type internal Horizon =
    | ToEndOfGame
    | ToEndOfRound

/// <summary>
/// What a hand is worth when it is played on as well as it can be, computed
/// rather than sampled. The deck is a known multiset, so the chance of every
/// card that could come next is known exactly and the average over them can be
/// summed instead of estimated: there is no sampling error in any of this, and
/// nothing here needs a threshold to decide whether it has seen enough.
///
/// Looking one card ahead is what MaximizesExpectedValue already does. Looking
/// further is the same question asked of the hand that card would leave, which
/// is what makes a hand worth holding onto: a fifteen worth keeping is one you
/// can still safely improve, and one card of sight cannot tell the difference.
/// </summary>
module private Lookahead =
    // Standing banks what the hand is worth, and a bust hand banks nothing
    let private Banked (hand: Hand) : float =
        if Hand.IsBust hand then 0.0 else float (Hand.Score hand)

    // The cards the deck could turn up, with the chance of each
    let private Chances (deck: Deck) : (Card * float) list =
        let drawable = deck |> Map.toList |> List.filter (fun (_, count) -> count > 0u)
        let total = drawable |> List.sumBy (snd >> float)

        if total = 0.0 then
            []
        else
            drawable |> List.map (fun (card, count) -> card, float count / total)

    /// <summary>
    /// What playing on is worth: the average over every card that could come,
    /// each one played out as well as it can be from there. A bust banks
    /// nothing, and flipping seven ends the round with the bonus already in
    /// the score.
    /// </summary>
    let rec public AfterHitting (depth: int) (deck: Deck) (hand: Hand) : float =
        match Chances deck with
        | [] -> Banked hand
        | chances ->
            chances
            |> List.sumBy (fun (card, chance) ->
                let isBust, reduced, _ = Hand.Reduce(card :: hand)

                let worth =
                    if isBust then
                        0.0
                    else
                        Worth (depth - 1) (Deck.Decrement deck card) reduced

                chance * worth
            )

    /// <summary>
    /// What a hand is worth held: the better of banking it now and playing on,
    /// as far ahead as the depth allows. A hand that has flipped seven is worth
    /// banking whatever the depth, because the round ends there.
    /// </summary>
    and public Worth (depth: int) (deck: Deck) (hand: Hand) : float =
        let banked = Banked hand

        if depth <= 0 || Hand.HasFlip7Bonus hand then
            banked
        else
            max banked (AfterHitting depth deck hand)

    /// <summary>
    /// What hitting is worth over standing, in points of expected round score.
    /// Positive says hit. Looking one card ahead this is exactly the question
    /// MaximizesExpectedValue asks, so any greater depth knows strictly more
    /// than it does.
    /// </summary>
    let public GainFromHitting (depth: int) (deck: Deck) (hand: Hand) : float =
        // Seven distinct cards ends the round where it stands, so there is no
        // draw left to price
        if Hand.HasFlip7Bonus hand then
            0.0
        else
            AfterHitting depth deck hand - Banked hand

/// <summary>
/// An adaptive opponent for interactive play: it accumulates evidence about
/// every player it has seen (persisted games from earlier sessions plus the
/// current game, one instant at a time) and decides its own hits, stands, and
/// freeze targets by Monte Carlo best response. Each rollout samples one
/// strategy per opponent from their posterior (fixed within the rollout,
/// resampled across rollouts, so model uncertainty propagates into the
/// estimate) and plays the rest of the game through the real engine via
/// Timeline.ContinueWith.
///
/// A Sage is immutable: Sage(history) starts one that has studied past games,
/// and Sage(instant, previous) advances one by a single instant of the
/// current game, so a caller threads the game through it like a fold over
/// the timeline. Each instant advances the observation scan and, when it
/// holds a decision, adds that decision onto the decider's cached
/// log-likelihood sums; posteriors materialize from the sums only when read,
/// so nothing is ever refit and no archive is ever rescored.
/// </summary>
type public Sage private (evidence: Map<string, PlayerEvidence>, scan: Observation.Scan, rollouts: int) =

    // How many cards ahead the within-round search looks. Each card of sight
    // multiplies the work by the number of cards the deck can turn up, so
    // three is where it stops paying: a decision costs about eight
    // milliseconds there against a hundred and twenty at four
    static let lookahead = 3

    // Models are keyed by what a player declared and, for Custom labels only,
    // also by who they are: an engine strategy plays identically no matter
    // who holds it, so those observations pool across players and sessions,
    // while a Custom strategy is decided externally - a human at the
    // terminal - and stays personal to its name
    static member private Key(name: string, strategy: Strategy) : string =
        match strategy with
        | Strategy.Custom _ -> $"{name}|{strategy}"
        | _ -> string strategy

    /// <summary>
    /// Starts a Sage that has studied the given past games, e.g. every
    /// timeline persisted by earlier sessions. Rollouts is the most any one
    /// decision may spend, not what every decision spends: a decision stops
    /// buying rollouts the moment they have settled it, which for an endgame
    /// is usually the first batch. The cap is worth being generous with,
    /// because the decisions that reach it are the ones too close to call any
    /// other way.
    /// </summary>
    new(history: Instant list list, ?rollouts: int)
        =
        let evidence =
            history
            |> Seq.map AsyncSeq.ofSeq
            |> Observation.FromTimelines
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously
            |> List.groupBy (fun observation -> Sage.Key(observation.Name, observation.Player.Strategy))
            |> List.map (fun (key, decisions) -> key, Inference.Evidence key decisions)
            |> Map.ofList

        Sage(evidence, Observation.Start, defaultArg rollouts 400)

    /// <summary>
    /// The fold step: the previous Sage advanced by the next instant of the
    /// current game. The observation scan advances, and when the instant
    /// holds a voluntary decision, that one decision is scored and added onto
    /// the decider's cached evidence; every other player costs nothing.
    /// </summary>
    new(instant: Instant, previous: Sage)
        =
        let scan, observation = Observation.Observe previous.Scan instant

        let evidence =
            match observation with
            | None -> previous.Evidence
            | Some observation ->
                let key = Sage.Key(observation.Name, observation.Player.Strategy)
                let single = Inference.Evidence key [ observation ]

                previous.Evidence
                |> Map.change
                    key
                    (function
                    | Some accumulated -> Some(Inference.Combine accumulated single)
                    | None -> Some single
                    )

        Sage(evidence, scan, previous.Rollouts)

    member private _.Evidence = evidence
    member private _.Scan = scan
    member private _.Rollouts = rollouts

    // A model materializes from the cached evidence only when read, so only
    // the players actually consulted are ever fit
    member private _.ModelFor(key: string) : PlayerModel option =
        evidence |> Map.tryFind key |> Option.map Inference.ModelFrom

    /// <summary>
    /// Plays the rest of the game through the real engine and scores it for
    /// the named player: 1.0 for ending with the top score, 0.5 for a shared
    /// top score, and 0.0 otherwise.
    /// </summary>
    static member private Score
        (horizon: Horizon)
        (random: System.Random)
        (name: string)
        (decide: Strategy.Decider)
        (round: uint)
        (turnsTaken: Map<string, uint>)
        (active: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<float>
        = async {
        let played =
            Timeline.ContinueWith random decide round turnsTaken active finished decks

        // Stopping at the round's end costs a fraction of playing the game
        // out, because the search stops pulling the timeline as it lands
        let! last =
            match horizon with
            | ToEndOfGame -> played |> AsyncSeq.tryLast
            | ToEndOfRound -> played |> AsyncSeq.tryFind (fun instant -> instant.Event.IsRoundEnded)

        let finals =
            last
            |> Option.map (fun instant -> instant.Players |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)
            |> Option.defaultValue (active @ finished |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)

        let mine = finals |> Map.find name
        let best = finals |> Map.remove name |> Map.values |> Seq.fold max 0u

        return
            match horizon with
            | ToEndOfGame ->
                if mine > best then 1.0
                elif mine = best then 0.5
                else 0.0
            // The margin banked carries the same sign as winning but says how
            // much by, so a round that changes nothing about who eventually
            // wins can still report what the decision was worth
            | ToEndOfRound -> float mine - float best
    }

    /// <summary>
    /// One rollout of the rest of the game from a hit-or-stand decision
    /// point: the deciding player's first action is forced when one is given,
    /// and every player otherwise follows their sampled strategy through the
    /// real engine until the game ends.
    /// </summary>
    static member private Rollout
        (horizon: Horizon)
        (random: System.Random)
        (strategies: Map<string, Strategy>)
        (forced: Strategy.HitOrStand option)
        (counted: Map<string, uint> option)
        (round: uint)
        (turn: uint)
        (player: Player)
        (others: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<float>
        =
        // The declared strategies are replaced by the sampled ones (a human's
        // Custom label has no meaning inside a rollout), and everyone aims
        // their action cards at random
        let restrategize (p: Player) : Player = {
            p with
                Strategy = strategies |> Map.find p.Name
                Targeting = Targeting.ChoosesRandomly
        }

        let active = player :: others |> List.map restrategize
        let finished = finished |> List.map restrategize

        // Turn counters: the decider's own is exact, since the engine derived
        // the turn it passed from its counter, and the scan supplies the other
        // players' real counts when Sage has watched the game. Without a scan
        // (a decision point given out of context) empty-handed players are
        // assumed to still await their forced first hit
        let turnsTaken =
            active
            |> List.map (fun p ->
                p.Name,
                if p.Name = player.Name then
                    max turn 1u - 1u
                else
                    match counted with
                    | Some counts -> counts |> Map.tryFind p.Name |> Option.defaultValue 0u
                    | None -> (if List.isEmpty p.Hand then 0u else max turn 1u - 1u)
            )
            |> Map.ofList

        // The forced action, when given, is consumed by the first hit-or-stand
        // ask, which belongs to the deciding player at the head of the
        // rotation; every ask after that plays out the sampled strategies
        let mutable pending = forced
        let canonical = Strategy.DecideWith random

        let decide = {
            canonical with
                Strategy.HitOrStand =
                    fun strategy round turn current others finished decks ->
                        match pending with
                        | Some action when current.Name = player.Name ->
                            pending <- None
                            async.Return action
                        | _ -> canonical.HitOrStand strategy round turn current others finished decks
        }

        Sage.Score horizon random player.Name decide round turnsTaken active finished decks

    // Rollouts are seeded from the position rather than from the game's own
    // randomness, so that consulting Sage - or changing how much it consults
    // itself - cannot shift a single card the engine goes on to deal. Two runs
    // of the same game then deal identically wherever Sage decides the same
    // way, and diverge only where it really chose differently, which is what
    // makes one run measurable against another. It also costs nothing in
    // reproducibility: the same position always gets the same rollouts
    static member private SeedFor(position: string) : System.Random =
        // Not `hash`: .NET randomises string hashing per process, so hashing a
        // position that mentions anybody by name would hand Sage different
        // rollouts on every run and leave a played game impossible to replay.
        // FNV-1a is stable for as long as the text is
        let mutable hash = 2166136261u

        for character in position do
            hash <- (hash ^^^ uint character) * 16777619u

        System.Random(int hash)

    // The independent rollout games fan out across cores. Their opponent
    // samples and seeds are always drawn before they start, so outcomes never
    // depend on scheduling and a decision stays bit-identical to its
    // sequential equivalent
    static member private Parallel(games: Async<'a> seq) : Async<'a array> =
        Async.Parallel(games, maxDegreeOfParallelism = System.Environment.ProcessorCount)

    // Whether paired differences are decisively positive: their mean sits
    // clear of even by the given number of standard errors. A noisy argmax
    // around a sound rule plays worse than the rule itself, so a comparison
    // closer than its own noise is left to the rule rather than acted on
    static member private DecisiveAt (bound: float) (differences: float array) : bool =
        let mean = Array.average differences

        let error =
            differences
            |> Array.sumBy (fun difference -> (difference - mean) * (difference - mean))
            |> fun squares -> sqrt (squares / float (differences.Length * (differences.Length - 1)))

        mean - bound * error > 0.0

    // Two standard errors is the bound for looking once, which is what a
    // finished comparison is
    static member private Decisive(differences: float array) : bool = Sage.DecisiveAt 2.0 differences

    // Who wins becomes the question once anyone is within about two rounds of
    // the finish; before that, a rollout played that far is mostly noise about
    // rounds this decision cannot reach
    static member private HorizonFor(players: Player list) : Horizon =
        if players |> List.exists (fun player -> Player.Showing player >= 150u) then
            ToEndOfGame
        else
            ToEndOfRound

    // Spends rollouts until the comparison is settled instead of spending the
    // same number everywhere. An endgame difference shows itself in a handful
    // of games where an early one can take hundreds, so the budget is a cap
    // rather than a count, and most decisions never reach it
    static member private Race
        (cap: int)
        (draw: unit -> Map<string, Strategy> * int)
        (compare: Map<string, Strategy> -> int -> Async<float>)
        : Async<float array>
        =
        // Stopping early means asking after every batch whether the answer is
        // in, and a bound set for asking once lets noise through when it is
        // asked a dozen times: at two standard errors this race called a
        // difference on pure noise one time in five. Three holds the rate
        // below what a single look at the full budget would give, and the
        // differences worth stopping early for - an endgame is worth ten
        // standard errors by the second batch - clear it just as fast
        let rec more (differences: float array) = async {
            if differences.Length >= cap then
                return differences
            else
                // Drawn before the games start, so that the rollouts stay
                // independent of how they get scheduled
                let drawn = List.init (min 32 (cap - differences.Length)) (fun _ -> draw ())

                let! settled =
                    drawn
                    |> List.map (fun (strategies, seed) -> compare strategies seed)
                    |> Sage.Parallel

                let grown = Array.append differences settled

                if Sage.DecisiveAt 3.0 grown || Sage.DecisiveAt 3.0 (grown |> Array.map (~-)) then
                    return grown
                else
                    return! more grown
        }

        more [||]

    // Plays every candidate target on the same rollouts and keeps the spiteful
    // rule's pick unless one of them beats it by more than that comparison's
    // own noise. Each round of the table carries one opponent sample and one
    // seed, shared by every candidate, so what is compared is the difference
    // aiming elsewhere would have made
    static member private BestOf
        (candidates: Player list)
        (fallback: Player)
        (rounds: (Map<string, Strategy> * int) array)
        (play: Map<string, Strategy> -> System.Random -> Player -> Async<float>)
        : Async<Player>
        = async {
        let! outcomes =
            rounds
            |> Array.toList
            |> List.mapi (fun iteration (strategies, seed) ->
                candidates
                |> List.mapi (fun index candidate -> async {
                    let! outcome = play strategies (System.Random seed) candidate
                    return iteration, index, outcome
                })
            )
            |> List.concat
            |> Sage.Parallel

        let table = Array2D.zeroCreate rounds.Length (List.length candidates)

        for iteration, index, outcome in outcomes do
            table[iteration, index] <- outcome

        let at =
            candidates |> List.findIndex (fun candidate -> candidate.Name = fallback.Name)

        let improvement (index: int) : (int * float) option =
            let differences =
                Array.init rounds.Length (fun iteration -> table[iteration, index] - table[iteration, at])

            if index <> at && Sage.Decisive differences then
                Some(index, Array.average differences)
            else
                None

        match List.init (List.length candidates) improvement |> List.choose id with
        | [] -> return fallback
        | improvements -> return candidates |> List.item (improvements |> List.maxBy snd |> fst)
    }

    /// <summary>
    /// The fitted model of a player under the strategy they declared, or None
    /// before any of their decisions have been observed with it.
    /// </summary>
    member this.ModelOf(name: string, strategy: Strategy) : PlayerModel option = this.ModelFor(Sage.Key(name, strategy))

    /// <summary>
    /// Hit or stand by Monte Carlo best response, shaped as a
    /// HitOrStandDecider (like Strategy.DecideHitOrStandWith) so Sage plugs in
    /// wherever the engine takes a decider. The declared strategy is ignored:
    /// Sage's Custom label names it rather than describes it. So is the
    /// randomness it is handed - Sage draws none of the game's own, seeding
    /// its rollouts from the position instead - but it is still taken, so that
    /// Sage reads as the decider factory it is.
    ///
    /// A decision runs two stages of rollouts against strategies sampled from
    /// the opponents' posteriors (expected-value play when unmodeled). A
    /// small tournament first picks the continuation strategy Sage itself
    /// plays inside the rollouts: the fixed strategy that wins most from here
    /// against the modeled table - policy iteration within the fixed-strategy
    /// class, since rollouts recursing into Sage itself would be
    /// unaffordable. The main stage then forces each action and plays the
    /// tournament winner as Sage's continuation, and the winner's own answer
    /// stands unless the rollout evidence overrules it decisively. Both
    /// stages are paired - one opponent sample and one seed per iteration,
    /// shared by everything compared - so the shared luck cancels out of the
    /// comparisons and only the consequences remain.
    /// </summary>
    member this.Decide(_random: System.Random) : Strategy.HitOrStandDecider =
        fun _strategy round turn player others finished decks ->
            // Before anyone is in reach of 200 the question is what the hand
            // is worth, and that is not a question to sample: the deck is a
            // known multiset, so the answer can be computed outright by
            // playing the hand against every card that could come. One card
            // of sight is what MaximizesExpectedValue already has, so three
            // knows strictly more and knows it exactly - there is no estimate
            // here to be uncertain about, nothing for a threshold to do, and
            // no reason to ask who anyone else is. Two thirds of the hands
            // Sage plays are decided here, and none of what follows is built
            // for them
            let decided () =
                let deck, discards = decks
                let drawable = if Deck.IsEmpty deck then discards else deck

                if Lookahead.GainFromHitting lookahead drawable player.Hand > 0.0 then
                    Strategy.Hit
                else
                    Strategy.Stand

            async {
                match Sage.HorizonFor(player :: others @ finished) with
                | ToEndOfRound -> return decided ()
                | ToEndOfGame ->

                let held (player: Player) =
                    Hand.Serialize player.Hand |> String.concat "-"

                let mine = held player
                let table = others |> List.map held |> String.concat "/"
                let rng =
                    Sage.SeedFor $"{round}:{turn}:{player.Name}:{player.FirmScore}:{mine}:{table}"

                let counted =
                    if scan = Observation.Start then
                        None
                    else
                        Some(Observation.TurnsTaken scan)

                // The posteriors of everyone at the table, materialized from
                // the cached evidence only now that they are wanted
                let modeled =
                    others @ finished
                    |> List.map (fun opponent ->
                        opponent.Name, this.ModelFor(Sage.Key(opponent.Name, opponent.Strategy))
                    )
                    |> Map.ofList

                let sampleOpponents () =
                    others @ finished
                    |> List.map (fun opponent ->
                        let strategy =
                            match Map.find opponent.Name modeled with
                            | Some model -> Inference.SampleWith rng model
                            | None -> Strategy.MaximizesExpectedValue

                        opponent.Name, strategy
                    )

                // Sage plays expected value as itself inside its own
                // rollouts. It used to hold a small tournament here to pick
                // something better, which was worth a great deal when rollouts
                // decided every hand - but an endgame hit is worth some
                // thirty-eight points of win probability against a stand, and
                // next to nothing about that turns on which fixed strategy the
                // rollout assumes afterwards. Taking the tournament out left
                // the win rate exactly where it was and the decision a quarter
                // quicker
                let! differences =
                    Sage.Race
                        rollouts
                        (fun () ->
                            Map.ofList ((player.Name, Strategy.MaximizesExpectedValue) :: sampleOpponents ()),
                            rng.Next()
                        )
                        (fun strategies seed -> async {
                            let forced (action: Strategy.HitOrStand) =
                                Sage.Rollout
                                    ToEndOfGame
                                    (System.Random seed)
                                    strategies
                                    (Some action)
                                    counted
                                    round
                                    turn
                                    player
                                    others
                                    finished
                                    decks

                            let! hitOutcome = forced Strategy.Hit
                            let! standOutcome = forced Strategy.Stand
                            return hitOutcome - standOutcome
                        })

                // Overrule the search's own answer only when the paired
                // evidence is decisive
                if Sage.Decisive differences then
                    return Strategy.Hit
                elif Sage.Decisive(differences |> Array.map (~-)) then
                    return Strategy.Stand
                else
                    return decided ()
            }

    /// <summary>
    /// Who to give an action card to, shaped as a TargetDecider like
    /// Strategy.DecideTargetWith. Freezes and deal3s are aimed by paired Monte
    /// Carlo, with Sage playing the continuation its tournament picked, chosen
    /// once before the card is given so that every candidate is compared under
    /// the same policy. Aiming at whoever the spiteful rule picks stands
    /// unless a candidate beats it by more than the rollouts' own noise.
    ///
    /// A second chance is aimed spitefully: the engine narrows those
    /// candidates to the players who can accept one, so the ask does not carry
    /// the whole table and no legal state can be rebuilt from it. Both rollout
    /// asks also need Sage to have watched the current game, since a targeting
    /// ask is told neither the round nor the turns taken.
    /// </summary>
    member this.Aim(random: System.Random) : Strategy.TargetDecider =
        fun _targeting ask chooser candidates finished decks ->
            let spitefully () =
                Strategy.DecideTargetWith random Targeting.PlaysSpitefully ask chooser candidates finished decks

            // A deal3 is replayed from the chooser's own turn, so it can only
            // be aimed by rollout while the chooser is still in the rotation -
            // a set-aside deal3 given out by someone already frozen is not
            let seated =
                candidates |> List.exists (fun candidate -> candidate.Name = chooser.Name)

            let aimable =
                scan <> Observation.Start
                && List.length candidates > 1
                && match ask with
                   | Strategy.Ask.WhoToFreeze -> true
                   | Strategy.Ask.WhoReceivesDeal3 -> seated
                   | Strategy.Ask.WhoReceivesSecondChance -> false

            if not aimable then
                spitefully ()
            else
                let holding = Hand.Serialize chooser.Hand |> String.concat "-"
                let aiming = Observation.RoundOf scan

                let table =
                    candidates |> List.map (fun candidate -> candidate.Name) |> String.concat "/"

                let rng = Sage.SeedFor $"{ask}:{aiming}:{chooser.Name}:{holding}:{table}"
                let horizon = Sage.HorizonFor(candidates @ finished)
                let round = Observation.RoundOf scan
                let counted = Observation.TurnsTaken scan

                let opponents =
                    candidates @ finished |> List.filter (fun player -> player.Name <> chooser.Name)

                let modeled =
                    opponents
                    |> List.map (fun player -> player.Name, this.ModelFor(Sage.Key(player.Name, player.Strategy)))
                    |> Map.ofList

                let sampleOpponents () =
                    opponents
                    |> List.map (fun player ->
                        let strategy =
                            match Map.find player.Name modeled with
                            | Some model -> Inference.SampleWith rng model
                            | None -> Strategy.MaximizesExpectedValue

                        player.Name, strategy
                    )

                let restrategize (strategies: Map<string, Strategy>) (player: Player) : Player = {
                    player with
                        Strategy = strategies |> Map.find player.Name
                        Targeting = Targeting.ChoosesRandomly
                }

                // Freezing a candidate removes them from the rotation with the
                // freeze card parked in their hand, the way the engine resolves
                // it, so every card stays accounted for. The chooser's turn is
                // spent on the freeze, which the scan has not seen yet
                let freeze (strategies: Map<string, Strategy>) (random: System.Random) (target: Player) =
                    let active =
                        candidates
                        |> List.filter (fun candidate -> candidate.Name <> target.Name)
                        |> List.map (restrategize strategies)

                    let iced =
                        restrategize strategies { target with Hand = ActionCard Card.Freeze :: target.Hand }

                    let spent =
                        counted
                        |> Map.add chooser.Name ((counted |> Map.tryFind chooser.Name |> Option.defaultValue 0u) + 1u)

                    Sage.Score
                        horizon
                        random
                        chooser.Name
                        (Strategy.DecideWith random)
                        round
                        spent
                        active
                        (iced :: (finished |> List.map (restrategize strategies)))
                        decks

                // A deal3 is replayed rather than modelled: the engine is
                // handed a deck holding nothing but the deal3 card the chooser
                // has just played, so it draws that card straight back, aims it
                // where the rollout is testing, and resolves the three flips,
                // any set-asides and any nested deal3 by its own rules. The
                // discards reshuffle into the deck for the first flip, so every
                // card is where it should be by the time one is drawn
                let deal3 (strategies: Map<string, Strategy>) (random: System.Random) (target: Player) =
                    let deck, discards = decks

                    let pooled =
                        Map.fold
                            (fun total card count -> Map.add card (count + Map.find card total) total)
                            deck
                            discards

                    let rigged =
                        Deck.Increment Deck.Empty (ActionCard Card.Deal3), Deck.Decrement pooled (ActionCard Card.Deal3)

                    let active =
                        chooser
                        :: (candidates |> List.filter (fun candidate -> candidate.Name <> chooser.Name))
                        |> List.map (restrategize strategies)

                    let canonical = Strategy.DecideWith random
                    let mutable forcing = true
                    let mutable aiming = true

                    let decide = {
                        Strategy.HitOrStand =
                            fun strategy round turn current others finished decks ->
                                if forcing && current.Name = chooser.Name then
                                    forcing <- false
                                    async.Return Strategy.Hit
                                else
                                    canonical.HitOrStand strategy round turn current others finished decks
                        Strategy.Target =
                            fun targeting ask chooser candidates finished decks ->
                                if aiming && ask = Strategy.Ask.WhoReceivesDeal3 then
                                    aiming <- false
                                    forcing <- false

                                    candidates
                                    |> List.tryFind (fun candidate -> candidate.Name = target.Name)
                                    |> Option.defaultValue (List.head candidates)
                                    |> async.Return
                                else
                                    canonical.Target targeting ask chooser candidates finished decks
                    }

                    Sage.Score
                        horizon
                        random
                        chooser.Name
                        decide
                        round
                        counted
                        active
                        (finished |> List.map (restrategize strategies))
                        rigged

                let play =
                    match ask with
                    | Strategy.Ask.WhoReceivesDeal3 -> deal3
                    | _ -> freeze

                // A freeze ask is made while the engine still holds the card
                // the chooser drew, so it is in neither the deck nor a hand and
                // the table is one card short. Giving it out is what puts it in
                // the target's hand, which every candidate rollout does; the
                // tournament plays no freeze at all, so it sends the card to
                // the discards instead and keeps the count whole. A deal3 is
                // discarded before its own ask and needs nothing
                let counting =
                    match ask with
                    | Strategy.Ask.WhoToFreeze ->
                        let deck, discards = decks
                        deck, Deck.Increment discards (ActionCard Card.Freeze)
                    | _ -> decks

                async {
                    // The continuation Sage plays as itself, picked from the
                    // position as it stands, before the card is given
                    let self = Strategy.MaximizesExpectedValue

                    let rounds =
                        Array.init
                            rollouts
                            (fun _ -> Map.ofList ((chooser.Name, self) :: sampleOpponents ()), rng.Next())

                    let! spiteful = spitefully ()
                    return! Sage.BestOf candidates spiteful rounds play
                }
