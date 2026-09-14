namespace Flip7

open FSharp.Control

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

    // The continuation strategies Sage can adopt inside its rollouts: a
    // conservative-to-aggressive spread of score thresholds, the flip7 chase,
    // the race to 200, and expected-value play first so it holds ties
    static let selfCandidates = [
        Strategy.MaximizesExpectedValue
        Strategy.HitUntilScore 18u
        Strategy.HitUntilScore 22u
        Strategy.HitUntilScore 26u
        Strategy.ChasesFlip7(22u, 5u)
        Strategy.HitUntilTotal 200u
    ]

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
    /// timeline persisted by earlier sessions.
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

        Sage(evidence, Observation.Start, defaultArg rollouts 100)

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
        let! last =
            Timeline.ContinueWith random decide round turnsTaken active finished decks
            |> AsyncSeq.tryLast

        let finals =
            last
            |> Option.map (fun instant -> instant.Players |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)
            |> Option.defaultValue (active @ finished |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)

        let mine = finals |> Map.find name
        let best = finals |> Map.remove name |> Map.values |> Seq.fold max 0u

        return
            if mine > best then 1.0
            elif mine = best then 0.5
            else 0.0
    }

    /// <summary>
    /// One rollout of the rest of the game from a hit-or-stand decision
    /// point: the deciding player's first action is forced when one is given,
    /// and every player otherwise follows their sampled strategy through the
    /// real engine until the game ends.
    /// </summary>
    static member private Rollout
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

        Sage.Score random player.Name decide round turnsTaken active finished decks

    /// <summary>
    /// The fitted model of a player under the strategy they declared, or None
    /// before any of their decisions have been observed with it.
    /// </summary>
    member this.ModelOf(name: string, strategy: Strategy) : PlayerModel option = this.ModelFor(Sage.Key(name, strategy))

    /// <summary>
    /// Hit or stand by Monte Carlo best response, shaped as a
    /// HitOrStandDecider closing over the randomness (like
    /// Strategy.DecideHitOrStandWith) so Sage plugs in wherever the engine
    /// takes a decider. The declared strategy is ignored: Sage's Custom label
    /// names it rather than describes it.
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
    member this.Decide(random: System.Random) : Strategy.HitOrStandDecider =
        fun _strategy round turn player others finished decks ->
            let rng = System.Random(random.Next())

            let counted =
                if scan = Observation.Start then
                    None
                else
                    Some(Observation.TurnsTaken scan)

            // The posteriors of everyone at the table, materialized once per
            // decision from the cached evidence
            let modeled =
                others @ finished
                |> List.map (fun opponent -> opponent.Name, this.ModelFor(Sage.Key(opponent.Name, opponent.Strategy)))
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

            // The samples and seeds are drawn sequentially so the rng stream
            // is deterministic, then the independent rollout games fan out
            // across cores; the outcomes do not depend on scheduling, so a
            // decision is bit-identical to its sequential equivalent
            let parallel' (games: Async<'a> seq) : Async<'a array> =
                Async.Parallel(games, maxDegreeOfParallelism = System.Environment.ProcessorCount)

            async {
                let! outcomes =
                    List.init (max 16 (rollouts / 2)) (fun _ -> sampleOpponents (), rng.Next())
                    |> List.collect (fun (opponents, seed) ->
                        selfCandidates
                        |> List.mapi (fun index candidate -> async {
                            let strategies = Map.ofList ((player.Name, candidate) :: opponents)

                            let! outcome =
                                Sage.Rollout
                                    (System.Random seed)
                                    strategies
                                    None
                                    counted
                                    round
                                    turn
                                    player
                                    others
                                    finished
                                    decks

                            return index, outcome
                        })
                    )
                    |> parallel'

                let scores = Array.zeroCreate (List.length selfCandidates)

                for index, outcome in outcomes do
                    scores[index] <- scores[index] + outcome

                let self =
                    selfCandidates
                    |> List.item (scores |> Array.mapi (fun index score -> index, score) |> Array.maxBy snd |> fst)

                let! differences =
                    List.init rollouts (fun _ -> Map.ofList ((player.Name, self) :: sampleOpponents ()), rng.Next())
                    |> List.map (fun (strategies, seed) -> async {
                        let! hitOutcome =
                            Sage.Rollout
                                (System.Random seed)
                                strategies
                                (Some Strategy.Hit)
                                counted
                                round
                                turn
                                player
                                others
                                finished
                                decks

                        let! standOutcome =
                            Sage.Rollout
                                (System.Random seed)
                                strategies
                                (Some Strategy.Stand)
                                counted
                                round
                                turn
                                player
                                others
                                finished
                                decks

                        return hitOutcome - standOutcome
                    })
                    |> parallel'

                // Overrule the continuation strategy's own answer only when
                // the paired evidence is decisive - two standard errors from
                // even - because a noisy argmax around a good policy plays
                // worse than the policy itself
                let mean = Array.average differences

                let error =
                    differences
                    |> Array.sumBy (fun difference -> (difference - mean) * (difference - mean))
                    |> fun squares -> sqrt (squares / float (rollouts * (rollouts - 1)))

                if mean - 2.0 * error > 0.0 then
                    return Strategy.Hit
                elif mean + 2.0 * error < 0.0 then
                    return Strategy.Stand
                else
                    return! Strategy.DecideHitOrStandWith rng self round turn player others finished decks
            }

    /// <summary>
    /// Who to give an action card to, shaped as a TargetDecider like
    /// Strategy.DecideTargetWith. Freezes are aimed by paired Monte Carlo:
    /// every candidate is frozen out in turn and the rest of the game plays
    /// through the real engine from the scan's round and turn counters,
    /// keeping the target whose removal wins Sage the game most often. A
    /// deal3 or a second chance resolves mid-card, in state a targeting ask
    /// alone cannot reconstruct, so those aim like Targeting.PlaysSpitefully.
    /// The freeze rollouts need Sage to have watched the current game (a
    /// targeting ask is not told the round or the turns taken); without that
    /// it aims spitefully too.
    /// </summary>
    member this.Aim(random: System.Random) : Strategy.TargetDecider =
        fun _targeting ask chooser candidates finished decks ->
            match ask with
            | Strategy.Ask.WhoToFreeze when scan <> Observation.Start && List.length candidates > 1 ->
                let rng = System.Random(random.Next())

                let modeled =
                    candidates @ finished
                    |> List.filter (fun player -> player.Name <> chooser.Name)
                    |> List.map (fun player -> player.Name, this.ModelFor(Sage.Key(player.Name, player.Strategy)))
                    |> Map.ofList

                let sample () =
                    candidates @ finished
                    |> List.map (fun player ->
                        let strategy =
                            if player.Name = chooser.Name then
                                Strategy.MaximizesExpectedValue
                            else
                                match Map.find player.Name modeled with
                                | Some model -> Inference.SampleWith rng model
                                | None -> Strategy.MaximizesExpectedValue

                        player.Name, strategy
                    )
                    |> Map.ofList

                // The chooser's turn is being consumed by this freeze, but the
                // scan has not seen the Froze event yet, so count it by hand
                let counts =
                    let counted = Observation.TurnsTaken scan
                    let taken = counted |> Map.tryFind chooser.Name |> Option.defaultValue 0u
                    counted |> Map.add chooser.Name (taken + 1u)

                let round = Observation.RoundOf scan

                // Freezing a candidate removes them from the rotation with the
                // freeze card parked in their hand, exactly as the engine
                // resolves it, so every card stays accounted for
                let freeze (strategies: Map<string, Strategy>) (target: Player) =
                    let restrategize (p: Player) : Player = {
                        p with
                            Strategy = strategies |> Map.find p.Name
                            Targeting = Targeting.ChoosesRandomly
                    }

                    let active =
                        candidates
                        |> List.filter (fun candidate -> candidate.Name <> target.Name)
                        |> List.map restrategize

                    let iced = restrategize { target with Hand = ActionCard Card.Freeze :: target.Hand }

                    active, iced :: (finished |> List.map restrategize)

                let parallel' (games: Async<'a> seq) : Async<'a array> =
                    Async.Parallel(games, maxDegreeOfParallelism = System.Environment.ProcessorCount)

                async {
                    let! outcomes =
                        List.init rollouts (fun _ -> sample (), rng.Next())
                        |> List.collect (fun (strategies, seed) ->
                            candidates
                            |> List.mapi (fun index target -> async {
                                let random = System.Random seed
                                let active, finished = freeze strategies target

                                let! outcome =
                                    Sage.Score
                                        random
                                        chooser.Name
                                        (Strategy.DecideWith random)
                                        round
                                        counts
                                        active
                                        finished
                                        decks

                                return index, outcome
                            })
                        )
                        |> parallel'

                    let scores = Array.zeroCreate (List.length candidates)

                    for index, outcome in outcomes do
                        scores[index] <- scores[index] + outcome

                    return
                        candidates
                        |> List.item (scores |> Array.mapi (fun index score -> index, score) |> Array.maxBy snd |> fst)
                }
            | _ -> Strategy.DecideTargetWith random Targeting.PlaysSpitefully ask chooser candidates finished decks
