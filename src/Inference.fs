namespace Flip7

open FSharp.Control

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

/// <summary>
/// What Sage has learnt of the players from games already played: the evidence
/// each player's choices in them carry, their hit-or-stand decisions over a
/// grid of candidate strategies, Inference.DefaultStrategyCandidates unless the
/// study was given one, and their aims over a grid of targeting policies that
/// take no sides, Inference.DefaultTargetingCandidates unless the study was
/// given one, each played plainly and taking sides among everyone at their
/// tables, kept by name rather than by declared strategy or targeting, since at
/// a real table people play under Custom and ChoosesExternally labels and only
/// their names say who they are. A Sage scores the decisions and aims of the
/// game it plays over its study's grids too.
///
/// Games are studied in sittings, such as a day's games played one after
/// another: how a player hits or stands and how they otherwise aim are learnt
/// from every game alike, but the sides they take are carried from one game of
/// a sitting into the next as Inference.Carry carries them, kept with
/// probability 0.8, and forgotten when Conclude ends the sitting. A Sage plays
/// its game as the next of the study's sitting, so conclude a study whose last
/// games were played on another day. It is immutable, so one study serves any
/// number of Sages, playing games one after another or all at once.
/// </summary>
type public Study =
    private | Study of Strategies: Strategy list * Targetings: Targeting list * Evidence: Map<string, PlayerEvidence>

module public Study =
    /// <summary>
    /// A study of no games over the default grids: it knows nobody.
    /// </summary>
    let public Nothing: Study =
        Study(Inference.DefaultStrategyCandidates, Inference.DefaultTargetingCandidates, Map.empty)

    // How likely a player is to keep the sides they took in one game of a
    // sitting in the next. Fixed rather than learnt: over sixteen games of a
    // player who kept their sides throughout, took new ones every game, changed
    // them halfway or took none, a stickiness learnt from the games themselves
    // predicted their aims at most 1.3 nats better than 0.8 did
    [<Literal>]
    let internal Stickiness = 0.8

    /// <summary>
    /// Studies more games, played after those a study holds in the order given,
    /// as more of its sitting: whole games, such as a day's games read with
    /// Persistence.ReadTimeline. The timelines are read once each, one after
    /// another, so they may share a Random, and every voluntary decision and
    /// every aim in them is extracted as Observation.FromTimeline extracts it.
    /// Each game is scored apart, each player's decisions in it against the
    /// study's grid of strategies and their aims over its grid of policies,
    /// each played plainly and taking sides among everyone seated at it, as
    /// Inference.Evidence scores them for a player who makes each choice out of
    /// character with probability epsilon, the players in parallel, and what
    /// the study holds of everyone seated at it from the games before is
    /// carried into it with Inference.Carry, whether or not it asks them
    /// anything. A study concluded with Conclude starts a new sitting. It
    /// raises ArgumentException at the call, before it reads any game, for an
    /// epsilon that is not above 0 and at most 1.
    /// </summary>
    let public ContinueWith (epsilon: float) (timelines: Timeline seq) (study: Study) : Async<Study> =
        if not (epsilon > 0.0 && epsilon <= 1.0) then
            invalidArg
                (nameof epsilon)
                $"The epsilon is the probability of a choice out of character, above 0 and at most 1, but was {epsilon}"

        async {
            let (Study(strategies, targetings, studied)) = study

            // Every game in order, as everyone seated at it and every question
            // it asked them
            let! games: (string list * Observation array) array =
                timelines
                |> AsyncSeq.ofSeq
                |> AsyncSeq.mapAsync (fun timeline -> async {
                    let! instants = AsyncSeq.toArrayAsync timeline

                    let! observations =
                        instants |> AsyncSeq.ofSeq |> Observation.FromTimeline |> AsyncSeq.toArrayAsync

                    let table =
                        instants
                        |> Array.tryHead
                        |> Option.map (fun first -> first.Players |> List.map (fun player -> player.Name))
                        |> Option.defaultValue []

                    return table, observations
                })
                |> AsyncSeq.toArrayAsync

            // What each game shows of everyone seated at it, scored in
            // parallel, in the order of the games
            let shown =
                games
                |> Array.collect (fun (table, observations) ->
                    let seated =
                        table @ [ for observation in observations -> observation.Name ] |> List.distinct

                    [| for name in seated -> seated, name, observations |]
                )
                |> Array.Parallel.map (fun (seated, name, observations) ->
                    seated,
                    name,
                    Inference.Evidence epsilon strategies targetings seated name [
                        for observation in observations do
                            if observation.Name = name then
                                observation
                    ]
                )

            // Everyone seated at a game is carried into it, even if it never
            // asks them anything, as a Sage carries everyone at its table, but a
            // player is studied only once a game shows something of them
            let evidence =
                shown
                |> Array.fold
                    (fun (evidence: Map<string, PlayerEvidence>) (seated, name, game) ->
                        match Map.tryFind name evidence with
                        | Some before ->
                            let carried = Inference.Carry Stickiness seated before
                            evidence |> Map.add name (Inference.Combine carried game)
                        | None when game.Observations > 0 || game.Aims > 0 -> evidence |> Map.add name game
                        | None -> evidence
                    )
                    studied

            return Study(strategies, targetings, evidence)
        }

    /// <summary>
    /// Studies whole games, such as every game a household has played in one
    /// sitting, in the order given, over grids of one's own, as ContinueWith
    /// studies them with 10% of each choice out of character onto a study that
    /// knows nobody: a grid of candidate strategies, such as the default grid
    /// together with strategies a table is known to play between its points,
    /// which no study over the default grid could ever name, and a grid of
    /// targeting policies that take no sides, such as the default grid together
    /// with policies a table is known to aim by, each of which the study plays
    /// plainly and taking sides every way. A strategy or policy a grid holds
    /// more than once counts once. Studying no games gives a study over the
    /// grids that knows nobody. It raises ArgumentException at the call, before
    /// it reads any game, for an empty grid, for a Custom strategy or a policy
    /// decided externally, which have no likelihood to fit, and for a policy
    /// that takes sides, since the study adds every way of taking sides itself.
    /// </summary>
    let public OfTimelinesWith
        (strategies: Strategy list)
        (targetings: Targeting list)
        (timelines: Timeline seq)
        : Async<Study> =
        if strategies.IsEmpty then
            invalidArg (nameof strategies) "A study needs at least one candidate strategy"

        if targetings.IsEmpty then
            invalidArg (nameof targetings) "A study needs at least one targeting policy"

        match strategies |> List.where (fun strategy -> strategy.IsCustom) with
        | [] -> ()
        | custom ->
            let custom = custom |> List.map string |> String.concat "; "
            invalidArg
                (nameof strategies)
                $"The strategies must have a likelihood to fit, but these are decided externally: {custom}"

        match
            targetings
            |> List.where (fun targeting -> targeting.IsTakesSides || targeting.IsChoosesExternally)
        with
        | [] -> ()
        | refused ->
            let refused = refused |> List.map string |> String.concat "; "
            invalidArg
                (nameof targetings)
                $"The targetings must be policies that take no sides, since the study adds every way of taking sides itself, and none decided externally, which have no likelihood to fit, but these are not: {refused}"

        ContinueWith 0.1 timelines (Study(List.distinct strategies, List.distinct targetings, Map.empty))

    /// <summary>
    /// Studies whole games over the default grids, as OfTimelinesWith studies
    /// them over grids of one's own.
    /// </summary>
    let public OfTimelines (timelines: Timeline seq) : Async<Study> =
        OfTimelinesWith Inference.DefaultStrategyCandidates Inference.DefaultTargetingCandidates timelines

    /// <summary>
    /// Ends a study's sitting, as at the end of a day's games: what it holds of
    /// each player concluded with Inference.Conclude, so the sides they took
    /// are forgotten while how they otherwise aim and how they hit or stand are
    /// kept. Games studied onto it with ContinueWith, or played by a Sage that
    /// studied it, start a new sitting.
    /// </summary>
    let public Conclude (study: Study) : Study =
        let (Study(strategies, targetings, evidence)) = study
        Study(strategies, targetings, evidence |> Map.map (fun _ evidence -> Inference.Conclude evidence))

    /// <summary>
    /// The strategies a study scored every decision against.
    /// </summary>
    let internal StrategiesOf (study: Study) : Strategy list =
        let (Study(strategies, _, _)) = study
        strategies

    /// <summary>
    /// The targeting policies a study scored every aim over, each played
    /// plainly and taking sides.
    /// </summary>
    let internal TargetingsOf (study: Study) : Targeting list =
        let (Study(_, targetings, _)) = study
        targetings

    /// <summary>
    /// The evidence a study holds of a player, by name.
    /// </summary>
    let internal EvidenceOf (study: Study) (name: string) : PlayerEvidence option =
        let (Study(_, _, evidence)) = study
        evidence |> Map.tryFind name

    /// <summary>
    /// What a study makes of a player, by name, or None for a player it never
    /// saw decide or aim at one of two or more players.
    /// </summary>
    let public ModelOf (study: Study) (name: string) : PlayerModel option =
        EvidenceOf study name |> Option.map Inference.ModelFrom
