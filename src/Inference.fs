namespace Flip7

/// <summary>
/// A fitted model of one player: a posterior distribution over candidate
/// strategies given the hit-or-stand decisions they were observed making, and
/// one over candidate targetings given the aims they were observed making at
/// one of two or more players. Observations and Aims count those decisions and
/// aims, and HitRate is the share of the decisions that hit, which is NaN for a
/// player never seen decide. A posterior with no evidence of its kind is its
/// prior.
/// </summary>
type public PlayerModel = {
    Name: string

    Observations: int
    HitRate: float
    StrategyPosterior: (Strategy * float) list

    Aims: int
    TargetingPosterior: (Targeting * float) list
}

/// <summary>
/// The evidence a set of observations contributes to a player's model:
/// per-candidate log-likelihood sums of their hit-or-stand decisions over the
/// candidate strategies and of their aims at one of two or more players over
/// the candidate targetings, the names the targetings that take sides are over,
/// in order, and the counts the model reports. Log-likelihoods add, so evidence
/// over disjoint sets of observations of a game combines without rescoring:
/// score the game so far once, then combine each new observation into the total
/// as it happens. How a player hits or stands, and how they otherwise aim, hold
/// from game to game, but the sides they take only for a while, so the evidence
/// of the games before is carried into each next game of a sitting, such as a
/// day's games played one after another, with Carry, and a sitting's evidence
/// is concluded with Conclude before the next sitting's is combined onto it,
/// even at a table of different players.
/// </summary>
type public PlayerEvidence = {
    Name: string

    Observations: int
    Hits: int
    StrategyLogLikelihoods: (Strategy * float) list

    Aims: int
    Names: string list
    TargetingLogLikelihoods: (Targeting * float) list
}

module public Inference =
    /// <summary>
    /// The default candidate grid: every strategy Decider.Default can evaluate
    /// - all but Custom, which is decided externally - at a spread of parameter
    /// values. Anything sampled from a posterior over this grid can be fed
    /// straight back into Timeline.SimulateWith as an opponent model.
    /// </summary>
    let public DefaultStrategyCandidates: Strategy list =
        [ Strategy.AlwaysHits; Strategy.AlwaysStands; Strategy.MaximizesExpectedValue ]
        @ ([ 0.25; 0.5; 0.75 ] |> List.map Strategy.RandomWithProbability)
        @ ([ 2u .. 2u .. 40u ] |> List.map Strategy.HitUntilScore)
        @ ([ 1u .. 7u ] |> List.map Strategy.HitUntilNumCards)
        @ ([ 1..9 ]
           |> List.map (fun tenths -> Strategy.HitUntilBustProbability(float tenths / 10.0)))
        @ ([ 1..9 ]
           |> List.map (fun tenths -> Strategy.HitUntilNaiveBustProbability(float tenths / 10.0)))
        @ (List.allPairs [ 10u; 20u; 30u ] [ 2.0; 5.0 ]
           |> List.map Strategy.SoftHitUntilScore)
        @ ([ 40u .. 40u .. 200u ] |> List.map Strategy.HitUntilTotal)
        @ ([ 2u .. 6u ] |> List.map Strategy.HitUntilUniqueValues)
        @ (List.allPairs [ 15u; 20u; 25u ] [ 5u; 6u ] |> List.map Strategy.ChasesFlip7)
        @ ([ 10u .. 5u .. 30u ] |> List.map Strategy.EmboldenedBySecondChance)
        @ ([ 0u .. 5u .. 20u ] |> List.map Strategy.HitWhileBehindLeader)
        @ ([ 1u .. 6u ] |> List.map Strategy.StandsAfterTurn)
        @ ([ 8u .. 2u .. 28u ] |> List.map Strategy.PlaysLikeAHuman)

    /// <summary>
    /// The default policy grid: every targeting policy the engine evaluates
    /// that takes no sides, greedy at a spread of thresholds, and none decided
    /// externally. Inference itself adds the ways of taking sides, over the
    /// names a player is seen aiming at.
    /// </summary>
    let public DefaultTargetingCandidates: Targeting list =
        [ Targeting.ChoosesRandomly; Targeting.PlaysSpitefully ]
        @ (List.allPairs [ 0.1; 0.2; 0.3; 0.4; 0.5 ] [ 0.1; 0.2; 0.3 ]
           |> List.map Targeting.PlaysGreedily)

    // Every candidate targeting over a grid of policies that take no sides and
    // the names of a table: every policy as it is, then every policy as what
    // one who takes sides otherwise does, with every way of favouring one of
    // the names, opposing another, or both. There are (n + 1)^2 - n of them for
    // each policy over n names, so the grid grows with the names a player is
    // seen among, and no faster.
    let private TargetingCandidates (policies: Targeting list) (names: string list) : Targeting list =
        let names = names |> List.distinct |> List.sort
        let sides = None :: (names |> List.map Some)

        policies
        @ [
            for otherwise in policies do
                for favours in sides do
                    for opposes in sides do
                        if favours.IsSome || opposes.IsSome then
                            if favours <> opposes then
                                Targeting.TakesSides(favours, opposes, otherwise)
        ]

    /// <summary>
    /// The probability that a strategy hits in the state captured by an
    /// observation. Mirrors Strategy.DecideHitOrStandWith, but returns the
    /// probability of hitting instead of sampling a decision so likelihoods are
    /// exact; probabilistic strategies return their actual curve value.
    /// </summary>
    let private ProbabilityOfHit (strategy: Strategy) (observation: Observation) : float =
        match strategy with
        | Strategy.AlwaysHits -> 1.0
        | Strategy.AlwaysStands -> 0.0
        | Strategy.RandomWithProbability probability -> probability
        | Strategy.HitUntilScore threshold ->
            if Hand.Score observation.Player.Hand < threshold then
                1.0
            else
                0.0
        | Strategy.HitUntilNumCards threshold ->
            if uint (List.length observation.Player.Hand) < threshold then
                1.0
            else
                0.0
        | Strategy.HitUntilBustProbability threshold ->
            let onlyPlayer = List.isEmpty observation.OtherPlayers
            let bustProbability =
                Simulation.probabilityToBust observation.Deck observation.Discards observation.Player.Hand onlyPlayer
            if bustProbability < threshold then 1.0 else 0.0
        | Strategy.HitUntilNaiveBustProbability threshold ->
            let unseen = observation.Player.Hand |> List.fold Deck.Decrement Deck.Full
            let onlyPlayer = List.isEmpty observation.OtherPlayers
            let bustProbability =
                Simulation.probabilityToBust unseen Deck.Empty observation.Player.Hand onlyPlayer
            if bustProbability < threshold then 1.0 else 0.0
        | Strategy.SoftHitUntilScore(threshold, temperature) ->
            let distance = float (Hand.Score observation.Player.Hand) - float threshold
            1.0 / (1.0 + exp (distance / temperature))
        | Strategy.HitUntilTotal target ->
            if observation.Player.FirmScore + Hand.Score observation.Player.Hand < target then
                1.0
            else
                0.0
        | Strategy.HitUntilUniqueValues threshold ->
            if uint (Hand.UniqueValueCards observation.Player.Hand) < threshold then
                1.0
            else
                0.0
        | Strategy.ChasesFlip7(score, uniques) ->
            if uint (Hand.UniqueValueCards observation.Player.Hand) >= uniques then
                1.0
            elif Hand.Score observation.Player.Hand < score then
                1.0
            else
                0.0
        | Strategy.EmboldenedBySecondChance threshold ->
            if observation.Player.Hand |> List.contains (ActionCard Card.SecondChance) then
                1.0
            elif Hand.Score observation.Player.Hand < threshold then
                1.0
            else
                0.0
        | Strategy.HitWhileBehindLeader margin ->
            let total = observation.Player.FirmScore + Hand.Score observation.Player.Hand
            let leader =
                observation.OtherPlayers @ observation.FinishedPlayers
                |> List.map Player.Showing
                |> List.fold max 0u

            if total < leader + margin then 1.0 else 0.0
        | Strategy.StandsAfterTurn turns -> if observation.Turn <= turns then 1.0 else 0.0
        | Strategy.PlaysLikeAHuman caution ->
            Strategy.HumanHitProbability caution observation.Player observation.OtherPlayers observation.FinishedPlayers
        | Strategy.MaximizesExpectedValue ->
            let deck, discards = observation.Deck, observation.Discards
            if Simulation.expectedValueOfHit deck discards observation.Player.Hand > 0.0 then
                1.0
            else
                0.0
        | Strategy.Custom name ->
            raise (
                System.InvalidOperationException
                    $"Custom {name} strategy is decided externally; it has no likelihood to fit"
            )

    // The probability that a targeting gives the card to the target an aim
    // observed among its candidates, judging busts by the chance of busting on
    // one card drawn from the deck and discards the aim was made at, and one
    // for a hit-or-stand decision
    let private ProbabilityOfTarget (targeting: Targeting) (observation: Observation) : float =
        let bust (player: Player) : float =
            Simulation.probabilityToBust observation.Deck observation.Discards player.Hand false

        match observation.Choice with
        | Choice1Of2 _ -> 1.0
        | Choice2Of2(ask, target) ->
            match Strategy.TargetOf targeting ask observation.Name bust observation.OtherPlayers with
            | Choice1Of2 picked -> if picked.Name = target then 1.0 else 0.0
            | Choice2Of2 among ->
                if among |> List.exists (fun candidate -> candidate.Name = target) then
                    1.0 / float among.Length
                else
                    0.0

    // The probability of a hit-or-stand choice under a strategy, allowing for a
    // player who with probability epsilon ignores their strategy and flips a
    // coin
    let private OfDecision
        (epsilon: float)
        (strategy: Strategy)
        (decision: Observation)
        (choice: Strategy.HitOrStand)
        : float =
        let probabilityOfHit =
            (1.0 - epsilon) * ProbabilityOfHit strategy decision + epsilon * 0.5

        match choice with
        | Strategy.Hit -> probabilityOfHit
        | Strategy.Stand -> 1.0 - probabilityOfHit

    // The probability of an aim under a targeting, allowing for a player who
    // with probability epsilon ignores how they aim and picks any candidate
    // alike
    let private OfAim (epsilon: float) (targeting: Targeting) (aim: Observation) : float =
        (1.0 - epsilon) * ProbabilityOfTarget targeting aim
        + epsilon / float aim.OtherPlayers.Length

    /// <summary>
    /// The probability of what an observation shows under a player who plays
    /// the strategy and aims by the targeting, allowing for human noise: with
    /// probability epsilon the player acts out of character, flipping a coin to
    /// hit or stand, or picking any of an aim's candidates alike. Epsilon must
    /// be positive so that one out-of-character choice cannot zero out an
    /// otherwise good candidate. A decision's probability depends on the
    /// strategy alone and an aim's on the targeting alone. An aim under
    /// ChoosesExternally, or under a targeting that takes sides more than three
    /// deep, raises InvalidOperationException, as it does in the engine.
    /// </summary>
    let public Likelihood
        (epsilon: float)
        (strategy: Strategy)
        (targeting: Targeting)
        (observation: Observation)
        : float =
        match observation.Choice with
        | Choice1Of2 choice -> OfDecision epsilon strategy observation choice
        | Choice2Of2 _ -> OfAim epsilon targeting observation

    /// <summary>
    /// The evidence a player's observations carry: their hit-or-stand decisions
    /// over the candidate strategies, and their aims over the candidate
    /// targetings: the given targetings, which are policies that take no sides,
    /// and each of them taking sides every way over the given names and every
    /// candidate's in the aims. An aim with a single candidate says nothing
    /// about how a player aims, so it is neither scored nor counted. Scoring
    /// one observation at a time over the same names and folding with Combine
    /// produces the same sums as scoring them all at once. Observations of
    /// several games score as though the player took the same sides throughout
    /// them, which scoring each game apart and carrying from one to the next
    /// with Carry does not assume. It raises ArgumentException if a given
    /// targeting takes sides, since it adds the ways of taking sides itself.
    /// </summary>
    let public Evidence
        (epsilon: float)
        (candidates: Strategy list)
        (targetings: Targeting list)
        (names: string list)
        (name: string)
        (observations: Observation list)
        : PlayerEvidence =
        match targetings |> List.where (fun targeting -> targeting.IsTakesSides) with
        | [] -> ()
        | sided ->
            let sided = sided |> List.map string |> String.concat "; "
            invalidArg
                (nameof targetings)
                $"The targetings must be policies that take no sides, since the evidence adds every way of taking sides itself, but these take them: {sided}"

        let decisions =
            observations
            |> List.choose (fun observation ->
                match observation.Choice with
                | Choice1Of2 choice -> Some(observation, choice)
                | Choice2Of2 _ -> None
            )

        let aims =
            observations
            |> List.where (fun observation ->
                match observation.Choice with
                | Choice1Of2 _ -> false
                | Choice2Of2 _ -> observation.OtherPlayers.Length > 1
            )

        let names =
            let otherPlayers =
                aims
                |> List.collect (fun aim -> aim.OtherPlayers |> List.map (fun candidate -> candidate.Name))

            names @ otherPlayers |> List.distinct |> List.sort

        {
            Name = name

            Observations = decisions.Length
            Hits =
                decisions
                |> List.filter (fun (_, choice) -> choice = Strategy.Hit)
                |> List.length
            StrategyLogLikelihoods =
                candidates
                |> List.map (fun candidate ->
                    candidate,
                    decisions
                    |> List.sumBy (fun (decision, choice) -> log (OfDecision epsilon candidate decision choice))
                )

            Aims = aims.Length
            Names = names
            TargetingLogLikelihoods =
                TargetingCandidates targetings names
                |> List.map (fun candidate ->
                    candidate, aims |> List.sumBy (fun aim -> log (OfAim epsilon candidate aim))
                )
        }

    // The policies a grid of candidate targetings is over: the candidates that
    // take no sides, in the order of the grid
    let private PoliciesOf (evidence: PlayerEvidence) : Targeting list =
        evidence.TargetingLogLikelihoods
        |> List.choose (fun (candidate, _) ->
            match candidate with
            | Targeting.TakesSides _ -> None
            | policy -> Some policy
        )

    // The policy a candidate plays, taking sides or not
    let private PolicyOf (candidate: Targeting) : Targeting =
        match candidate with
        | Targeting.TakesSides(_, _, otherwise) -> otherwise
        | policy -> policy

    // The log of the prior weight of a candidate over a number of names: as
    // much for taking sides with a policy as for playing it plainly, shared
    // alike among the n^2 + n ways of taking sides
    let private LogPriorOf (names: int) (candidate: Targeting) : float =
        match candidate with
        | Targeting.TakesSides _ -> -log(float (names * names + names))
        | _ -> 0.0

    // The log of a sum of exponentials, summed from the greatest so that
    // likelihoods too small for a float still add
    let private LogSumExp (logs: float list) : float =
        let greatest = List.max logs

        if System.Double.IsNegativeInfinity greatest then
            greatest
        else
            greatest + log (logs |> List.sumBy (fun each -> exp (each - greatest)))

    // The log of each policy's likelihood whatever sides it is played with: the
    // mean likelihood of its candidates, weighted by the prior among them
    let private LogMarginalsOf (evidence: PlayerEvidence) : Map<Targeting, float> =
        let names = evidence.Names.Length

        evidence.TargetingLogLikelihoods
        |> List.groupBy (fst >> PolicyOf)
        |> List.map (fun (policy, candidates) ->
            policy,
            LogSumExp(
                candidates
                |> List.map (fun (candidate, logLikelihood) -> LogPriorOf names candidate + logLikelihood)
            )
            - LogSumExp(candidates |> List.map (fst >> LogPriorOf names))
        )
        |> Map.ofList

    // The same evidence over more names. A name the evidence never saw was
    // never a candidate for any of its aims, so every aim it holds went to
    // someone else, and a candidate that takes that player's side scores
    // exactly as the one that takes nobody's, or only the other player's, the
    // way every earlier aim would have scored it
    let private Widen (policies: Targeting list) (names: string list) (evidence: PlayerEvidence) : PlayerEvidence =
        if names = evidence.Names then
            evidence
        else
            let scores = Map.ofList evidence.TargetingLogLikelihoods
            let known (name: string option) =
                name |> Option.filter (fun name -> List.contains name evidence.Names)

            {
                evidence with
                    Names = names
                    TargetingLogLikelihoods =
                        TargetingCandidates policies names
                        |> List.map (fun candidate ->
                            let scored =
                                match candidate with
                                | Targeting.TakesSides(favours, opposes, otherwise) ->
                                    match known favours, known opposes with
                                    | None, None -> otherwise
                                    | favours, opposes -> Targeting.TakesSides(favours, opposes, otherwise)
                                | policy -> policy

                            candidate, scores[scored]
                        )
            }

    /// <summary>
    /// Adds newer evidence onto accumulated evidence for the same player and
    /// the same grids, over every name either was scored over. Evidence of one
    /// game adds as it is; evidence of earlier games is first carried into the
    /// newer one's game with Carry, or concluded with Conclude when that game
    /// begins another sitting.
    /// </summary>
    let public Combine (accumulated: PlayerEvidence) (newer: PlayerEvidence) : PlayerEvidence =
        let names = accumulated.Names @ newer.Names |> List.distinct |> List.sort
        let policies = PoliciesOf accumulated

        let accumulated, newer =
            Widen policies names accumulated, Widen policies names newer

        let add (accumulated: ('candidate * float) list) (newer: ('candidate * float) list) =
            List.map2 (fun (candidate, accumulated) (_, newer) -> candidate, accumulated + newer) accumulated newer

        {
            Name = accumulated.Name
            Observations = accumulated.Observations + newer.Observations
            Hits = accumulated.Hits + newer.Hits
            StrategyLogLikelihoods = add accumulated.StrategyLogLikelihoods newer.StrategyLogLikelihoods
            Aims = accumulated.Aims + newer.Aims
            Names = names
            TargetingLogLikelihoods = add accumulated.TargetingLogLikelihoods newer.TargetingLogLikelihoods
        }

    /// <summary>
    /// Evidence of a player's games so far carried into the next game of the
    /// same sitting, such as the next of a day's games played one after
    /// another, among the given names: widened to them as Combine widens, then
    /// with whatever sides the player took, or none, kept with the probability
    /// stickiness and otherwise chosen afresh, as the prior chooses among a
    /// policy's candidates, played plainly or taking sides, while the policy
    /// itself is kept. It is the exact update for a player who keeps the sides
    /// they took from one game to the next with that probability, so one who
    /// took sides in the games before most likely takes the same ones in the
    /// next, while how they otherwise aim and how they hit or stand are learnt
    /// across every game alike. The strategy evidence is the same after as
    /// before, and so is the posterior over the policies over the same names,
    /// though widening to names the evidence never saw shares the prior of
    /// taking sides among more ways, as Combine does, which can move it. At a
    /// stickiness of 1 every side is kept, as though the games were one, and at
    /// 0 every side is forgotten, as Conclude forgets them, though the names
    /// are kept. Carry the evidence once as each game starts, then combine that
    /// game's evidence onto it. It raises ArgumentException for a stickiness
    /// outside 0 to 1.
    /// </summary>
    let public Carry (stickiness: float) (names: string list) (evidence: PlayerEvidence) : PlayerEvidence =
        if not (stickiness >= 0.0 && stickiness <= 1.0) then
            invalidArg (nameof stickiness) $"The stickiness is a probability, from 0 to 1, but was {stickiness}"

        let widened =
            Widen (PoliciesOf evidence) (evidence.Names @ names |> List.distinct |> List.sort) evidence

        let marginals = LogMarginalsOf widened

        {
            widened with
                TargetingLogLikelihoods =
                    widened.TargetingLogLikelihoods
                    |> List.map (fun (candidate, logLikelihood) ->
                        candidate,
                        LogSumExp [
                            log stickiness + logLikelihood
                            log (1.0 - stickiness) + marginals[PolicyOf candidate]
                        ]
                    )
        }

    /// <summary>
    /// Evidence of a sitting that has ended, such as a day's games, ready for
    /// the next sitting's to be combined onto it: whatever sides the player
    /// took forgotten, and with them the names, so that only how they otherwise
    /// aim and how they hit or stand are kept. Each policy keeps its likelihood
    /// whatever sides, or none, it was played with: the mean over its
    /// candidates, played plainly or taking sides, weighted by the prior, so
    /// the posterior over the policies and the strategy evidence are the same
    /// after as before. Without the names, the next sitting's sides are judged
    /// among only the names its own games are scored over, however many people
    /// earlier sittings saw, so a sitting combined onto concluded ones and then
    /// concluded gives what concluding it alone and combining gives, even at a
    /// table of different players. Concluding twice is concluding once, and
    /// carrying concluded evidence into a game only widens it.
    /// </summary>
    let public Conclude (evidence: PlayerEvidence) : PlayerEvidence =
        let marginals = LogMarginalsOf evidence

        {
            evidence with
                Names = []
                TargetingLogLikelihoods = PoliciesOf evidence |> List.map (fun policy -> policy, marginals[policy])
        }

    /// <summary>
    /// Each candidate targeting's log-likelihood plus the log of its prior, in
    /// the order of the grid: the log of its posterior, up to a constant. The
    /// prior gives taking sides with a policy as much weight as playing it
    /// plainly, shared alike among every way of taking sides, so how many
    /// players someone has been seen among does not make them any more likely
    /// to take sides.
    /// </summary>
    let internal AimLogWeights (evidence: PlayerEvidence) : (Targeting * float) list =
        evidence.TargetingLogLikelihoods
        |> List.map (fun (candidate, logLikelihood) ->
            candidate, logLikelihood + LogPriorOf evidence.Names.Length candidate
        )

    /// <summary>
    /// The model a body of evidence supports: the posterior over the strategies
    /// from a uniform prior, and the posterior over the targetings from a prior
    /// that gives taking sides with a policy as much weight as playing it
    /// plainly, shared alike among every way of taking sides, so how many
    /// players someone has been seen among does not make them any more likely
    /// to take sides. Without decisions the strategy posterior is the uniform
    /// prior and HitRate is NaN, and without aims the targeting posterior is
    /// its prior, which over no names is uniform over the policies that take no
    /// sides.
    /// </summary>
    let public ModelFrom (evidence: PlayerEvidence) : PlayerModel =
        // Normalizes log-weights into a posterior, likeliest first. Subtracting
        // the greatest first avoids underflow
        let PosteriorOf (logWeights: ('candidate * float) list) : ('candidate * float) list =
            let maxLogWeight = logWeights |> List.map snd |> List.max

            let weights =
                logWeights
                |> List.map (fun (candidate, logWeight) -> candidate, exp (logWeight - maxLogWeight))

            let total = weights |> List.sumBy snd

            weights
            |> List.map (fun (candidate, weight) -> candidate, weight / total)
            |> List.sortByDescending snd

        {
            Name = evidence.Name
            Observations = evidence.Observations
            HitRate = float evidence.Hits / float evidence.Observations
            StrategyPosterior = PosteriorOf evidence.StrategyLogLikelihoods
            Aims = evidence.Aims
            TargetingPosterior = PosteriorOf(AimLogWeights evidence)
        }

    /// <summary>
    /// The maximum a posterior strategy and targeting: the candidates that best
    /// explain the player's decisions and aims. A posterior with no evidence of
    /// its kind is its prior, whose head is only the first candidate of its
    /// grid and says nothing of the player, so check Observations before
    /// reading the strategy and Aims before reading the targeting.
    /// </summary>
    let public MostLikely (model: PlayerModel) : Strategy * Targeting =
        fst (List.head model.StrategyPosterior), fst (List.head model.TargetingPosterior)
