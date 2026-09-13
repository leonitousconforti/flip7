namespace Flip7

open FSharp.Control

/// <summary>
/// An adaptive opponent for interactive play: fits a posterior model of every
/// player it has seen (persisted games from earlier sessions plus the rounds
/// of the current game as they finish) and decides its own hits and stands by
/// Monte Carlo best response. Each rollout samples one strategy per opponent
/// from their posterior (fixed within the rollout, resampled across rollouts,
/// so model uncertainty propagates into the estimate), forces the candidate
/// action, and plays the rest of the game through the real engine via
/// Timeline.ContinueWith, everyone aiming action cards at random.
///
/// A Sage is immutable: Sage(history) starts one that has studied past games,
/// and Sage(instant, previous) advances one by a single instant of the
/// current game, so a caller threads the game through it like a fold over
/// the timeline.
/// </summary>
type public Sage
    private (past: Observation list, recorded: Instant list, models: Map<string, PlayerModel>, rollouts: int)
    =

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

    static member private Fit(observations: Observation list) : Map<string, PlayerModel> =
        observations
        |> List.map (fun observation -> {
            observation with
                Name = Sage.Key(observation.Name, observation.Player.Strategy)
        })
        |> Inference.Fit
        |> List.map (fun model -> model.Name, model)
        |> Map.ofList

    /// <summary>
    /// Starts a Sage that has studied the given past games, e.g. every
    /// timeline persisted by earlier sessions.
    /// </summary>
    new(history: Instant list list, ?rollouts: int)
        =
        let past =
            history
            |> Seq.map AsyncSeq.ofSeq
            |> Observation.FromTimelines
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously

        Sage(past, [], Sage.Fit past, defaultArg rollouts 100)

    /// <summary>
    /// The fold step: the previous Sage advanced by the next instant of the
    /// current game. Recording an instant is cheap; at each round boundary
    /// the game so far is re-observed and the models of everyone refit.
    /// </summary>
    new(instant: Instant, previous: Sage)
        =
        let recorded = instant :: previous.Recorded

        let models =
            match instant.Event with
            | RoundEnded _ ->
                let live =
                    List.rev recorded
                    |> AsyncSeq.ofSeq
                    |> Observation.FromTimeline
                    |> AsyncSeq.toListAsync
                    |> Async.RunSynchronously

                Sage.Fit(previous.Past @ live)
            | _ -> previous.Models

        Sage(previous.Past, recorded, models, previous.Rollouts)

    member private _.Past = past
    member private _.Recorded = recorded
    member private _.Models = models
    member private _.Rollouts = rollouts

    /// <summary>
    /// One rollout of the rest of the game from a decision point: the
    /// deciding player's first action is forced when one is given, and every
    /// player otherwise follows their sampled strategy through the real
    /// engine until the game ends. Turn counters continue from the decider's,
    /// which only matters to StandsAfterTurn; a player yet to see a card
    /// still gets their forced first hit. Returns 1.0 when the decider ends
    /// the game with the top score, 0.5 for a shared top score, and 0.0
    /// otherwise.
    /// </summary>
    static member private Rollout
        (random: System.Random)
        (strategies: Map<string, Strategy>)
        (forced: Strategy.HitOrStand option)
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

        // The decider has always taken their forced first hit - the engine
        // only consults a strategy from turn two - even when that hit left
        // their hand empty (a freeze or deal3 given away), so only other
        // empty-handed players still await theirs. Without this the rollout
        // would force an extra draw on the decider before the interceptor
        // below can apply the candidate action
        let turnsTaken =
            active
            |> List.map (fun p ->
                p.Name,
                (if p.Name <> player.Name && List.isEmpty p.Hand then
                     0u
                 else
                     max turn 1u - 1u)
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

        async {
            let! last =
                Timeline.ContinueWith random decide round turnsTaken active finished decks
                |> AsyncSeq.tryLast

            let finals =
                last
                |> Option.map (fun instant -> instant.Players |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)
                |> Option.defaultValue (
                    player :: others @ finished
                    |> List.map (fun p -> p.Name, p.FirmScore)
                    |> Map.ofList
                )

            let mine = finals |> Map.find player.Name
            let best = finals |> Map.remove player.Name |> Map.values |> Seq.fold max 0u

            return
                if mine > best then 1.0
                elif mine = best then 0.5
                else 0.0
        }

    /// <summary>
    /// The fitted model of a player under the strategy they declared, or None
    /// before any of their decisions have been observed with it.
    /// </summary>
    member _.ModelOf(name: string, strategy: Strategy) : PlayerModel option =
        models |> Map.tryFind (Sage.Key(name, strategy))

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
    /// tournament winner as Sage's continuation, taking the better action.
    /// Both stages are paired - one opponent sample and one seed per
    /// iteration, shared by everything compared - so the shared luck cancels
    /// out of the comparisons and only the consequences remain.
    /// </summary>
    member _.Decide(random: System.Random) : Strategy.HitOrStandDecider =
        fun _strategy round turn player others finished decks ->
            let rng = System.Random(random.Next())

            let sampleOpponents () =
                others @ finished
                |> List.map (fun opponent ->
                    let strategy =
                        match models |> Map.tryFind (Sage.Key(opponent.Name, opponent.Strategy)) with
                        | Some model -> Inference.SampleWith rng model
                        | None -> Strategy.MaximizesExpectedValue

                    opponent.Name, strategy
                )

            async {
                let scores = Array.zeroCreate (List.length selfCandidates)

                for _ in 1 .. max 8 (rollouts / 8) do
                    let opponents = sampleOpponents ()
                    let seed = rng.Next()

                    for index in 0 .. scores.Length - 1 do
                        let strategies =
                            Map.ofList ((player.Name, List.item index selfCandidates) :: opponents)

                        let! outcome =
                            Sage.Rollout (System.Random seed) strategies None round turn player others finished decks

                        scores[index] <- scores[index] + outcome

                let self =
                    selfCandidates
                    |> List.item (scores |> Array.mapi (fun index score -> index, score) |> Array.maxBy snd |> fst)

                let mutable hit = 0.0
                let mutable stand = 0.0

                for _ in 1..rollouts do
                    let strategies = Map.ofList ((player.Name, self) :: sampleOpponents ())
                    let seed = rng.Next()

                    let! hitOutcome =
                        Sage.Rollout
                            (System.Random seed)
                            strategies
                            (Some Strategy.Hit)
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
                            round
                            turn
                            player
                            others
                            finished
                            decks

                    hit <- hit + hitOutcome
                    stand <- stand + standOutcome

                if hit > stand then
                    return Strategy.Hit
                elif stand > hit then
                    return Strategy.Stand
                else
                    return! Strategy.DecideHitOrStandWith rng self round turn player others finished decks
            }
