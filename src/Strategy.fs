namespace Flip7

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module public Strategy =
    /// <summary>
    /// To hit or to stand.
    /// </summary>
    type public HitOrStand =
        | Hit
        | Stand

    /// <summary>
    /// Which action card is being aimed. Lives inside the Strategy module
    /// rather than the namespace so that it cannot be confused with
    /// System.Action or with Card.ActionCard.
    /// </summary>
    [<RequireQualifiedAccess>]
    type public Ask =
        | WhoToFreeze
        | WhoReceivesDeal3
        | WhoReceivesSecondChance

    /// <summary>
    /// Decides hit-or-stand for one player: given the player's declared
    /// strategy, the round, the turn, the player, the other active players, the
    /// players who already stood, busted, or were frozen this round, and the
    /// decks.
    /// </summary>
    type public HitOrStandDecider =
        Strategy -> uint -> uint -> Player -> Player list -> Player list -> (Deck * Deck) -> Async<HitOrStand>

    /// <summary>
    /// Decides who a player gives an action card to: given their declared
    /// targeting policy, what is being aimed, the player aiming it, the legal
    /// targets, the players who already stood, busted, or were frozen this
    /// round, and the decks. The candidates are narrowed by the engine and are
    /// never empty; the answer must be one of them.
    /// </summary>
    type public TargetDecider =
        Targeting -> Ask -> Player -> Player list -> Player list -> (Deck * Deck) -> Async<Player>

    /// <summary>
    /// Every decision a game asks of its players. Deciders are asynchronous so
    /// a decision can be awaited (a key press, a network message) without
    /// holding a thread. DecideWith is the canonical Decider; injecting a
    /// different one into Timeline.SimulateWithDecider lets Custom strategies
    /// and Targeting.ChoosesExternally targeting be decided by a human at the
    /// terminal.
    /// </summary>
    type public Decider = { HitOrStand: HitOrStandDecider; Target: TargetDecider }

    /// <summary>
    /// A strategy that randomly hits or stands with a 50% probability.
    /// </summary>
    let public Random: Strategy = Strategy.RandomWithProbability 0.5

    /// <summary>
    /// Evaluates a strategy using the given source of randomness, given the
    /// current round number, the current turn (how many times play has come
    /// around the table this round, counting from one for the player being
    /// asked), the player, the other players still in the round, the players
    /// who already stood, busted, or were frozen this round, and the decks,
    /// returning whether to hit or stand.
    /// </summary>
    let public DecideHitOrStandWith (random: System.Random) : HitOrStandDecider =
        fun strategy round turn player otherPlayers finishedPlayers decks ->
            match strategy with
            | Strategy.AlwaysHits -> Hit
            | Strategy.AlwaysStands -> Stand
            | Strategy.RandomWithProbability probability -> if random.NextDouble() < probability then Hit else Stand
            | Strategy.HitUntilScore threshold -> if Hand.Score player.Hand < threshold then Hit else Stand
            | Strategy.HitUntilNumCards threshold ->
                if uint (List.length player.Hand) < threshold then
                    Hit
                else
                    Stand
            | Strategy.HitUntilBustProbability threshold ->
                let deck, discards = decks
                let onlyPlayer = List.isEmpty otherPlayers
                if Simulation.probabilityToBust deck discards player.Hand onlyPlayer < threshold then
                    Hit
                else
                    Stand
            | Strategy.HitUntilNaiveBustProbability threshold ->
                let unseen = player.Hand |> List.fold Deck.Decrement Deck.Full
                let onlyPlayer = List.isEmpty otherPlayers
                if Simulation.probabilityToBust unseen Deck.Empty player.Hand onlyPlayer < threshold then
                    Hit
                else
                    Stand
            | Strategy.SoftHitUntilScore(threshold, temperature) ->
                let distance = float (Hand.Score player.Hand) - float threshold
                let probability = 1.0 / (1.0 + exp (distance / temperature))
                if random.NextDouble() < probability then Hit else Stand
            | Strategy.HitUntilTotal target ->
                if player.FirmScore + Hand.Score player.Hand < target then
                    Hit
                else
                    Stand
            | Strategy.HitUntilUniqueValues threshold ->
                if uint (Hand.UniqueValueCards player.Hand) < threshold then
                    Hit
                else
                    Stand
            | Strategy.ChasesFlip7(score, uniques) ->
                if uint (Hand.UniqueValueCards player.Hand) >= uniques then
                    Hit
                elif Hand.Score player.Hand < score then
                    Hit
                else
                    Stand
            | Strategy.EmboldenedBySecondChance threshold ->
                if player.Hand |> List.contains (ActionCard Card.SecondChance) then
                    Hit
                elif Hand.Score player.Hand < threshold then
                    Hit
                else
                    Stand
            | Strategy.HitWhileBehindLeader margin ->
                let total = player.FirmScore + Hand.Score player.Hand
                let leader =
                    otherPlayers @ finishedPlayers |> List.map Player.Showing |> List.fold max 0u
                if total < leader + margin then Hit else Stand
            | Strategy.StandsAfterTurn turns -> if turn <= turns then Hit else Stand
            | Strategy.MaximizesExpectedValue ->
                let deck, discards = decks
                if Simulation.expectedValueOfHit deck discards player.Hand > 0.0 then
                    Hit
                else
                    Stand
            | Strategy.Custom name ->
                raise (
                    System.InvalidOperationException
                        $"Custom {name} strategy is decided via `Timeline.SimulateWithDecider`, not by DecideHitOrStandWith"
                )
            |> async.Return

    /// <summary>
    /// Evaluates a targeting policy using the given source of randomness: which
    /// of the candidates a player gives an action card to. The candidates are
    /// the legal targets, already narrowed by the engine, and are never empty.
    /// For a freeze or a deal3 the chooser is usually among them, but not
    /// always: a player frozen earlier still hands out the cards they set aside
    /// during a deal3, and can no longer keep one for themselves.
    /// </summary>
    let public DecideTargetWith (random: System.Random) : TargetDecider =
        fun targeting ask chooser candidates _finishedPlayers decks ->
            let deck, discards = decks

            let self =
                candidates |> List.tryFind (fun candidate -> candidate.Name = chooser.Name)

            let opponents =
                candidates |> List.where (fun candidate -> candidate.Name <> chooser.Name)

            // Whether one more card busts them. Deliberately not the
            // only-player form: a policy is comparing players here, and the
            // extra risk a lone player carries would distort the comparison
            let bustProbability (player: Player) =
                Simulation.probabilityToBust deck discards player.Hand false

            // Hurts the most: the player nearest to winning, or the one nearest
            // to busting when the card is three forced flips. With no opponent
            // to aim at the card is forced back on the chooser
            let hurts () =
                match ask, opponents with
                | _, [] -> List.head candidates
                | Ask.WhoReceivesDeal3, opponents -> opponents |> List.maxBy bustProbability
                | _, opponents -> opponents |> List.maxBy Player.Showing

            // Helps the least: whoever is furthest behind
            let helpsLeast () = candidates |> List.minBy Player.Showing

            match targeting with
            | Targeting.ChoosesRandomly -> candidates |> List.randomChoiceWith random
            | Targeting.PlaysSpitefully ->
                match ask with
                | Ask.WhoReceivesSecondChance -> helpsLeast ()
                | Ask.WhoToFreeze -> hurts ()
                | Ask.WhoReceivesDeal3 -> hurts ()
            | Targeting.PlaysGreedily(banksAbove, deal3sBelow) ->
                match ask, self with
                | Ask.WhoReceivesSecondChance, _ -> helpsLeast ()
                | Ask.WhoToFreeze, Some self when bustProbability self >= banksAbove -> self
                | Ask.WhoReceivesDeal3, Some self when bustProbability self <= deal3sBelow -> self
                | _ -> hurts ()
            | Targeting.ChoosesExternally name ->
                raise (
                    System.InvalidOperationException
                        $"ChoosesExternally {name} targeting is decided via `Timeline.SimulateWithDecider`, not by DecideTargetWith"
                )
            |> async.Return

    /// <summary>
    /// Every decision, evaluated against the given source of randomness.
    /// </summary>
    let public DecideWith (random: System.Random) : Decider = {
        HitOrStand = DecideHitOrStandWith random
        Target = DecideTargetWith random
    }

    /// <summary>
    /// Evaluates a strategy without threading a source of randomness.
    /// </summary>
    [<System.Obsolete("Hidden shared randomness is a footgun: thread a System.Random from the edge of the program into Strategy.DecideWith instead")>]
    let public Decide: Decider = DecideWith System.Random.Shared
