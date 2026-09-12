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
/// </summary>
type public Sage(history: Instant list list, ?rollouts: int) =
    let rollouts = defaultArg rollouts 100

    let past =
        history
        |> Seq.map AsyncSeq.ofSeq
        |> Observation.FromTimelines
        |> AsyncSeq.toListAsync
        |> Async.RunSynchronously

    let mutable live: Observation list = []
    let mutable models: Map<string, PlayerModel> = Map.empty

    let fit () =
        models <-
            past @ live
            |> Inference.Fit
            |> List.map (fun model -> model.Name, model)
            |> Map.ofList

    do fit ()

    /// <summary>
    /// One rollout of the rest of the game from a decision point: the
    /// deciding player's first action is forced, then every player follows
    /// their sampled strategy through the real engine until the game ends.
    /// Turn counters continue from the decider's, which only matters to
    /// StandsAfterTurn; a player yet to see a card still gets their forced
    /// first hit. Returns 1.0 when the decider ends the game with the top
    /// score, 0.5 for a shared top score, and 0.0 otherwise.
    /// </summary>
    static member private Rollout
        (random: System.Random)
        (strategies: Map<string, Strategy>)
        (forced: Strategy.HitOrStand)
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

        let turnsTaken =
            active
            |> List.map (fun p -> p.Name, (if List.isEmpty p.Hand then 0u else max turn 1u - 1u))
            |> Map.ofList

        // The forced action is consumed by the first hit-or-stand ask, which
        // belongs to the deciding player at the head of the rotation; every
        // ask after that plays out the sampled strategies
        let mutable pending = Some forced
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
    /// The fitted model of a player, or None before any of their decisions
    /// have been observed.
    /// </summary>
    member _.ModelOf(name: string) : PlayerModel option = models |> Map.tryFind name

    /// <summary>
    /// Feeds the current game so far (all instants from the first) and refits
    /// the models. Cheap enough to call at every round boundary.
    /// </summary>
    member _.Learn(gameSoFar: Instant list) : unit =
        live <-
            gameSoFar
            |> AsyncSeq.ofSeq
            |> Observation.FromTimeline
            |> AsyncSeq.toListAsync
            |> Async.RunSynchronously

        fit ()

    /// <summary>
    /// Hit or stand by Monte Carlo best response: estimate the probability of
    /// ending the game with the top score under each action and take the
    /// better one. Opponents play strategies sampled from their posteriors
    /// (expected-value play when unmodeled); Sage's own rollout policy is
    /// also expected-value play, since it cannot recurse into itself.
    /// Asynchronous like every decider, so the rollouts run when the engine
    /// awaits the answer rather than holding a thread here.
    /// </summary>
    member _.Decide
        (random: System.Random)
        (round: uint)
        (turn: uint)
        (player: Player)
        (others: Player list)
        (finished: Player list)
        (decks: Deck * Deck)
        : Async<Strategy.HitOrStand>
        =
        // A derived stream so rollouts do not perturb the game's randomness
        let rng = System.Random(random.Next())

        let sampleStrategies () =
            (player.Name, Strategy.MaximizesExpectedValue)
            :: (others @ finished
                |> List.map (fun opponent ->
                    let strategy =
                        match models |> Map.tryFind opponent.Name with
                        | Some model -> Inference.SampleWith rng model
                        | None -> Strategy.MaximizesExpectedValue

                    opponent.Name, strategy
                ))
            |> Map.ofList

        // The rollouts share the derived random, so they run one at a time
        let estimate (action: Strategy.HitOrStand) : Async<float> = async {
            let mutable total = 0.0

            for _ in 1..rollouts do
                let! outcome =
                    Sage.Rollout rng (sampleStrategies ()) action round turn player others finished decks

                total <- total + outcome

            return total / float rollouts
        }

        async {
            let! hit = estimate Strategy.Hit
            let! stand = estimate Strategy.Stand

            if hit > stand then
                return Strategy.Hit
            elif stand > hit then
                return Strategy.Stand
            else
                return!
                    Strategy.DecideHitOrStandWith
                        rng
                        Strategy.MaximizesExpectedValue
                        round
                        turn
                        player
                        others
                        finished
                        decks
        }
