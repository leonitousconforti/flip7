namespace Flip7.Analysis

open Flip7

/// <summary>
/// A fitted model of one player: a posterior distribution over candidate
/// strategies given the decisions they were observed making.
/// </summary>
type public PlayerModel = {
    Name: string
    Observations: int
    HitRate: float
    /// Candidate strategies with normalized posterior probabilities, most
    /// probable first.
    Posterior: (Strategy * float) list
}

module public Inference =
    /// <summary>
    /// The probability that a strategy hits in the state captured by an
    /// observation. Mirrors Strategy.DecideWith, but returns the probability
    /// of hitting instead of sampling a decision so likelihoods are exact;
    /// probabilistic strategies return their actual curve value.
    /// </summary>
    let public ProbabilityOfHit (strategy: Strategy) (observation: Observation) : float =
        match strategy with
        | AlwaysHits -> 1.0
        | AlwaysStands -> 0.0
        | RandomWithProbability probability -> probability
        | HitUntilScore threshold ->
            if Hand.Score observation.Player.Hand < threshold then 1.0 else 0.0
        | HitUntilNumCards threshold ->
            if uint (List.length observation.Player.Hand) < threshold then
                1.0
            else
                0.0
        | HitUntilBustProbability threshold ->
            let deck, discards = observation.Decks
            let onlyPlayer = List.isEmpty observation.OtherPlayers

            if Simulation.probabilityToBust deck discards observation.Player.Hand onlyPlayer < threshold then
                1.0
            else
                0.0
        | HitUntilNaiveBustProbability threshold ->
            let unseen = observation.Player.Hand |> List.fold Deck.Decrement Deck.Full
            let onlyPlayer = List.isEmpty observation.OtherPlayers

            if Simulation.probabilityToBust unseen Deck.Empty observation.Player.Hand onlyPlayer < threshold then
                1.0
            else
                0.0
        | SoftHitUntilScore(threshold, temperature) ->
            let distance = float (Hand.Score observation.Player.Hand) - float threshold
            1.0 / (1.0 + exp (distance / temperature))
        | HitUntilTotal target ->
            if observation.Player.FirmScore + Hand.Score observation.Player.Hand < target then
                1.0
            else
                0.0
        | HitUntilUniqueValues threshold ->
            if uint (Hand.UniqueValueCards observation.Player.Hand) < threshold then
                1.0
            else
                0.0
        | ChasesFlip7(score, uniques) ->
            if uint (Hand.UniqueValueCards observation.Player.Hand) >= uniques then 1.0
            elif Hand.Score observation.Player.Hand < score then 1.0
            else 0.0
        | EmboldenedBySecondChance threshold ->
            if observation.Player.Hand |> List.contains (ActionCard Card.SecondChance) then 1.0
            elif Hand.Score observation.Player.Hand < threshold then 1.0
            else 0.0
        | HitWhileBehindLeader margin ->
            let total = observation.Player.FirmScore + Hand.Score observation.Player.Hand

            let leader =
                observation.OtherPlayers
                |> List.map (fun other -> other.FirmScore + Hand.Score other.Hand)
                |> List.fold max 0u

            if total < leader + margin then 1.0 else 0.0
        | StandsAfterTurn turns -> if observation.Session < turns then 1.0 else 0.0
        | MaximizesExpectedValue ->
            let deck, discards = observation.Decks

            if Simulation.expectedValueOfHit deck discards observation.Player.Hand > 0.0 then
                1.0
            else
                0.0

    /// <summary>
    /// The default candidate grid: every existing Strategy case at a spread of
    /// parameter values. Anything sampled from a posterior over this grid can
    /// be fed straight back into Timeline.SimulateWith as an opponent model.
    /// </summary>
    let public DefaultCandidates: Strategy list =
        [ AlwaysHits; AlwaysStands ]
        @ ([ 0.25; 0.5; 0.75 ] |> List.map RandomWithProbability)
        @ ([ 2u .. 2u .. 40u ] |> List.map HitUntilScore)
        @ ([ 1u .. 7u ] |> List.map HitUntilNumCards)
        @ ([ 1 .. 9 ] |> List.map (fun tenths -> HitUntilBustProbability(float tenths / 10.0)))

    /// <summary>
    /// The probability of the observed choice under a candidate strategy,
    /// allowing for human noise: with probability epsilon the player ignores
    /// their strategy and flips a coin. Epsilon must be positive so that one
    /// out-of-character decision cannot zero out an otherwise good candidate.
    /// </summary>
    let public Likelihood (epsilon: float) (strategy: Strategy) (observation: Observation) : float =
        let probabilityOfHit =
            (1.0 - epsilon) * ProbabilityOfHit strategy observation + epsilon * 0.5

        match observation.Choice with
        | Strategy.Hit -> probabilityOfHit
        | Strategy.Stand -> 1.0 - probabilityOfHit

    /// <summary>
    /// Fits a posterior over the candidate strategies for every player that
    /// appears in the observations, starting from a uniform prior.
    /// </summary>
    let public FitWith (epsilon: float) (candidates: Strategy list) (observations: Observation list) : PlayerModel list =
        observations
        |> List.groupBy (fun observation -> observation.Name)
        |> List.map (fun (name, decisions) ->
            let logLikelihoods =
                candidates
                |> List.map (fun candidate -> candidate, decisions |> List.sumBy (Likelihood epsilon candidate >> log))

            // Normalizing exponentiated log-likelihoods yields the posterior
            // from a uniform prior; subtracting the max first avoids underflow
            let maxLogLikelihood = logLikelihoods |> List.map snd |> List.max

            let weights =
                logLikelihoods
                |> List.map (fun (candidate, logLikelihood) -> candidate, exp (logLikelihood - maxLogLikelihood))

            let total = weights |> List.sumBy snd

            {
                Name = name
                Observations = List.length decisions
                HitRate =
                    decisions
                    |> List.averageBy (fun observation ->
                        match observation.Choice with
                        | Strategy.Hit -> 1.0
                        | Strategy.Stand -> 0.0
                    )
                Posterior =
                    weights
                    |> List.map (fun (candidate, weight) -> candidate, weight / total)
                    |> List.sortByDescending snd
            }
        )

    /// <summary>
    /// Fits with the default candidate grid and a 10% rate of
    /// out-of-character decisions.
    /// </summary>
    let public Fit (observations: Observation list) : PlayerModel list =
        FitWith 0.1 DefaultCandidates observations

    /// <summary>
    /// The maximum a posteriori strategy: the single candidate that best
    /// explains the player's decisions.
    /// </summary>
    let public MostLikely (model: PlayerModel) : Strategy = model.Posterior |> List.head |> fst

    /// <summary>
    /// How often a strategy's most likely action matches the observed
    /// decisions; 1.0 means it predicts every one of them.
    /// </summary>
    let public Accuracy (strategy: Strategy) (observations: Observation list) : float =
        observations
        |> List.averageBy (fun observation ->
            let predictsHit = ProbabilityOfHit strategy observation >= 0.5

            match observation.Choice with
            | Strategy.Hit -> if predictsHit then 1.0 else 0.0
            | Strategy.Stand -> if predictsHit then 0.0 else 1.0
        )

    /// <summary>
    /// Samples a strategy from a player's posterior. Draw one sample per
    /// Monte Carlo rollout — held fixed within the rollout, resampled across
    /// rollouts — so uncertainty about the player propagates into whatever
    /// the rollouts estimate.
    /// </summary>
    let public SampleWith (random: System.Random) (model: PlayerModel) : Strategy =
        let rec pick (roll: float) (candidates: (Strategy * float) list) : Strategy =
            match candidates with
            | [] -> MostLikely model
            | (candidate, probability) :: rest -> if roll < probability then candidate else pick (roll - probability) rest

        pick (random.NextDouble()) model.Posterior
