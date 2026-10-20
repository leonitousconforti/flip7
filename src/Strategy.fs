namespace Flip7

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module public rec Strategy =
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
    /// strategy, the game so far, the round, the turn, the player, the other
    /// active players, the players who already stood, busted, or were frozen
    /// this round, and the decks. The game so far is every instant the game
    /// produced before the question, oldest first. It is a snapshot rather than
    /// the timeline: it never changes once the question is asked, and
    /// enumerating it plays nothing again.
    /// </summary>
    type public HitOrStandDecider =
        Instant seq -> uint -> uint -> Player -> Player list -> Player list -> (Deck * Deck) -> Async<HitOrStand>

    /// <summary>
    /// Decides who a player gives an action card to: given their declared
    /// targeting policy, the game so far, what is being aimed, the player
    /// aiming it, the legal targets, the players who already stood, busted, or
    /// were frozen this round, and the decks. The game so far is every instant
    /// the game produced before the question, oldest first, so while a card is
    /// still being resolved it already holds the instants the resolution has
    /// produced, such as the deal3 whose set-aside card is being aimed, even
    /// though the timeline has yet to yield them. The candidates are narrowed
    /// by the engine and are never empty; the answer must be one of them.
    /// </summary>
    type public TargetDecider =
        Instant seq -> Ask -> Player -> Player list -> Player list -> (Deck * Deck) -> Async<Player>

    /// <summary>
    /// How likely a person is to hit in this position. People do not count the
    /// deck. They push on while a hand is small, grow reluctant as it turns
    /// into something worth losing, chase a flip7 once one is close, take more
    /// risk while somebody is running away with the game, and feel safe enough
    /// to push holding a second chance. Caution is the hand score they start to
    /// baulk at, and the response to it is soft rather than a threshold,
    /// because nobody plays the same borderline hand the same way twice. Shared
    /// with Inference so that a fitted human and a played one cannot drift
    /// apart.
    /// </summary>
    let internal HumanHitProbability
        (caution: uint)
        (player: Player)
        (otherPlayers: Player list)
        (finishedPlayers: Player list)
        : float =
        if Hand.UniqueValueCards player.Hand >= 5 then
            0.9
        else

        let showing = Player.Showing player
        let allPlayers = otherPlayers @ finishedPlayers
        let leader = allPlayers |> List.map Player.Showing |> List.fold max 0u
        let behind = float (leader - min leader showing)

        let emboldened =
            if player.Hand |> List.contains (ActionCard Card.SecondChance) then
                6.0
            else
                0.0

        let threshold = float caution + min 10.0 (behind / 4.0) + emboldened
        1.0 / (1.0 + exp ((float (Hand.Score player.Hand) - threshold) / 3.0))

    /// <summary>
    /// Evaluates a strategy using the given source of randomness, given the
    /// game so far (which no strategy reads), the current round number, the
    /// current turn (how many times play has come around the table this round,
    /// counting from one for the player being asked), the player, the other
    /// players still in the round, the players who already stood, busted, or
    /// were frozen this round, and the decks, returning whether to hit or
    /// stand.
    /// </summary>
    let private DecideHitOrStandWith (random: System.Random) (strategy: Strategy) : HitOrStandDecider =
        fun _history round turn player otherPlayers finishedPlayers decks ->
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
            | Strategy.PlaysLikeAHuman caution ->
                let probability = HumanHitProbability caution player otherPlayers finishedPlayers
                if random.NextDouble() < probability then Hit else Stand
            | Strategy.MaximizesExpectedValue ->
                let deck, discards = decks
                if Simulation.expectedValueOfHit deck discards player.Hand > 0.0 then
                    Hit
                else
                    Stand
            | Strategy.Custom name ->
                raise (
                    System.InvalidOperationException
                        $"Custom {name} strategy should be added to a Decider via `Strategy.Decider.addHitOrStandStrategy` and then simulated via `Timeline.SimulateWithDecider`, not by DecideHitOrStandWith"
                )
            |> async.Return

    // The chance of busting on one card at or below which one who takes sides
    // gives the favoured player a deal3, and at or above which they give it to
    // the opposed one. At one in ten, three cards pass safely about three times
    // in four, and at a little over one in three, one of them busts the hand
    // about three times in four. They are fixed rather than fitted, since every
    // pair of them would multiply the candidates Inference fits who take sides
    [<Literal>]
    let private Kind = 0.1

    [<Literal>]
    let private Cruel = 0.35

    /// <summary>
    /// Where a targeting gives a card among the candidates, judging busts by
    /// the given function: Choice1Of2 the player it decides on, or Choice2Of2
    /// the players it picks among at random. Shared with Inference so that a
    /// fitted targeting and a played one cannot drift apart. ChoosesExternally,
    /// alone or as what one who takes sides otherwise does, has no rule here
    /// and raises InvalidOperationException, as does a targeting that takes
    /// sides more than Targeting.SidesLimit deep, before it takes any side.
    /// </summary>
    let rec internal TargetOf
        (targeting: Targeting)
        (ask: Ask)
        (chooser: string)
        (bust: Player -> float)
        (candidates: Player list)
        : Choice<Player, Player list> =
        if Targeting.TakesSidesTooDeep targeting then
            raise (
                System.InvalidOperationException
                    $"A targeting takes sides at most {Targeting.SidesLimit} deep, and this one takes them deeper"
            )

        let self = candidates |> List.tryFind (fun candidate -> candidate.Name = chooser)

        let opponents =
            candidates |> List.where (fun candidate -> candidate.Name <> chooser)

        // Hurts the most: the player nearest to winning, or the one nearest to
        // busting when the card is three forced flips. With no opponent to aim
        // at the card is forced back on the chooser
        let hurts () =
            match ask, opponents with
            | _, [] -> List.head candidates
            | Ask.WhoReceivesDeal3, opponents -> opponents |> List.maxBy bust
            | _, opponents -> opponents |> List.maxBy Player.Showing

        // Helps the least: whoever is furthest behind
        let helpsLeast () = candidates |> List.minBy Player.Showing

        match targeting with
        | Targeting.ChoosesRandomly -> Choice2Of2 candidates
        | Targeting.PlaysSpitefully ->
            match ask with
            | Ask.WhoReceivesSecondChance -> Choice1Of2(helpsLeast ())
            | Ask.WhoToFreeze -> Choice1Of2(hurts ())
            | Ask.WhoReceivesDeal3 -> Choice1Of2(hurts ())
        | Targeting.PlaysGreedily(banksAbove, deal3sBelow) ->
            match ask, self with
            | Ask.WhoReceivesSecondChance, _ -> Choice1Of2(helpsLeast ())
            | Ask.WhoToFreeze, Some self when bust self >= banksAbove -> Choice1Of2 self
            | Ask.WhoReceivesDeal3, Some self when bust self <= deal3sBelow -> Choice1Of2 self
            | _ -> Choice1Of2(hurts ())
        | Targeting.TakesSides(favours, opposes, otherwise) ->
            let named (name: string option) =
                name
                |> Option.bind (fun name -> candidates |> List.tryFind (fun candidate -> candidate.Name = name))

            let favoured, opposed = named favours, named opposes

            let picked =
                match ask with
                | Ask.WhoReceivesSecondChance -> favoured
                | Ask.WhoToFreeze -> opposed
                | Ask.WhoReceivesDeal3 ->
                    match favoured, opposed with
                    | Some favoured, _ when bust favoured <= Kind -> Some favoured
                    | _, Some opposed when bust opposed >= Cruel -> Some opposed
                    | _ -> None

            match picked with
            | Some picked -> Choice1Of2 picked
            | None ->
                let named =
                    List.choose id [ favoured; opposed ] |> List.map (fun player -> player.Name)

                match
                    candidates
                    |> List.where (fun candidate -> not (List.contains candidate.Name named))
                with
                | [] -> TargetOf otherwise ask chooser bust candidates
                | others -> TargetOf otherwise ask chooser bust others
        | Targeting.ChoosesExternally name ->
            raise (
                System.InvalidOperationException
                    $"ChoosesExternally {name} targeting is decided externally: it has to be added to a Decider via `Strategy.addTargetingStrategy` and simulated via `Timeline.SimulateWithDecider`, and has no likelihood to fit"
            )

    /// <summary>
    /// Evaluates a targeting policy using the given source of randomness: which
    /// of the candidates a player gives an action card to. No policy reads the
    /// game so far. The candidates are the legal targets, already narrowed by
    /// the engine, and are never empty. For a freeze or a deal3 the chooser is
    /// usually among them, but not always: a player frozen earlier still hands
    /// out the cards they set aside during a deal3, and can no longer keep one
    /// for themselves. Where TargetOf decides on a player that player is the
    /// answer and nothing is drawn, and where it picks among players at random
    /// one of them is drawn from the Random. Where TargetOf raises, for
    /// ChoosesExternally or for a targeting that takes sides too deep, so does
    /// this.
    /// </summary>
    let private DecideTargetWith (random: System.Random) (targeting: Targeting) : TargetDecider =
        fun _history ask chooser candidates _finishedPlayers decks ->
            let deck, discards = decks
            let bust (player: Player) : float =
                Simulation.probabilityToBust deck discards player.Hand false

            match TargetOf targeting ask chooser.Name bust candidates with
            | Choice1Of2 picked -> picked
            | Choice2Of2 among -> among |> List.randomChoiceWith random
            |> async.Return

    /// <summary>
    /// Every decision a game asks of its players. Deciders are asynchronous so
    /// a decision can be awaited (a key press, a network message) without
    /// holding a thread. Decider.Default is the canonical Decider; injecting a
    /// different one into Timeline.SimulateWithDecider lets Custom strategies
    /// and Targeting.ChoosesExternally targeting be decided by a human at the
    /// terminal. Every question is asked with a Random of its own, seeded from
    /// the game's with a single draw just before it is asked, and never with
    /// the game's Random itself. So whatever a decider draws from its Random
    /// cannot shift the cards the game deals, and a seeded game whose deciders
    /// answer alike plays out the same way every time. A question's Random is
    /// no secret from the game's, though: its seed is the game's next draw,
    /// which in a seeded game gives the game's seed away to a decider that sets
    /// out to work it back.
    /// </summary>
    type public Decider = {
        HitOrStand: System.Random -> Strategy -> HitOrStandDecider
        Target: System.Random -> Targeting -> TargetDecider
    } with

        member public self.addHitOrStandStrategy
            (name: string, implementation: System.Random -> HitOrStandDecider)
            : Decider
            =
            self |> addHitOrStandStrategy name implementation

        member public self.addTargetingStrategy
            (name: string, implementation: System.Random -> TargetDecider)
            : Decider
            =
            self |> addTargetingStrategy name implementation

        static member public Default = {
            HitOrStand = DecideHitOrStandWith
            Target = DecideTargetWith
        }

    let public addHitOrStandStrategy
        (name: string)
        (implementation: System.Random -> HitOrStandDecider)
        (decider: Decider)
        : Decider = {
        decider with
            HitOrStand =
                fun random strategy ->
                    if strategy = Strategy.Custom name then
                        implementation random
                    else
                        decider.HitOrStand random strategy
    }

    let public addTargetingStrategy
        (name: string)
        (implementation: System.Random -> TargetDecider)
        (decider: Decider)
        : Decider = {
        decider with
            Target =
                fun random targeting ->
                    if targeting = Targeting.ChoosesExternally name then
                        implementation random
                    else
                        decider.Target random targeting
    }

    /// <summary>
    /// A strategy that randomly hits or stands with a 50% probability.
    /// </summary>
    let public Random: Strategy = Strategy.RandomWithProbability 0.5
