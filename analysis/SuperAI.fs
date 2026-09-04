namespace Flip7.Analysis

open FSharp.Control

open Flip7

/// <summary>
/// A player's state within one Monte Carlo rollout.
/// </summary>
type private RolloutPlayer = { Name: string; FirmScore: uint; Hand: Hand; Turn: uint }

/// <summary>
/// How a rollout hit resolved for the player who took it.
/// </summary>
[<RequireQualifiedAccess>]
type private Fate =
    | Alive
    | Busted
    | Frozen

/// <summary>
/// An adaptive opponent for interactive play: fits a posterior model of every
/// player it has seen (persisted games from earlier sessions plus the rounds
/// of the current game as they finish) and decides its own hits and stands by
/// Monte Carlo best response. Each rollout samples one strategy per opponent
/// from their posterior (fixed within the rollout, resampled across rollouts,
/// so model uncertainty propagates into the estimate), plays the rest of the
/// current round with a lightweight continuation, and hands any remaining
/// rounds to the real simulator.
/// </summary>
type public SuperAI(history: Instant list list, ?rollouts: int) =
    let rollouts = defaultArg rollouts 100
    let past = history |> List.collect Observation.FromTimeline
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
    /// One rollout of the rest of the game from a decision point, forcing the
    /// deciding player's first action, with every player following the
    /// sampled strategies. Returns 1.0 when the decider ends the game with
    /// the top score, 0.5 for a shared top score, and 0.0 otherwise.
    ///
    /// The current round is continued with a lightweight approximation of
    /// the real rules: value cards can bust (second chances still save, via
    /// Hand.Reduce), a drawn Freeze freezes the drawer, and a drawn Deal3
    /// deals three to the drawer, so action cards keep roughly their real
    /// effect without the targeting randomness. Turn counters continue from
    /// the decider's, which only matters to StandsAfterTurn. Rounds after the
    /// current one run through the real simulator seeded with the banked
    /// scores and the rollout's decks.
    /// </summary>
    static member private Rollout
        (random: System.Random)
        (strategies: Map<string, Strategy>)
        (forced: Strategy.HitOrStand)
        (round: uint)
        (turn: uint)
        (player: Strategy.StrategyPlayer)
        (others: Strategy.StrategyPlayer list)
        (finished: Strategy.StrategyPlayer list)
        (decks: Deck * Deck)
        : float
        =
        let decks = ref decks
        let flip7 = ref false

        let draw () =
            let decks', card = Deck.Draw1With random decks.Value
            decks.Value <- decks'
            card

        let discard (card: Card) =
            let deck, discards = decks.Value
            decks.Value <- deck, Deck.Increment discards card

        // Resolves the cards a hit flips into the hand: one for a normal hit,
        // three more for a drawn Deal3
        let rec resolve (hand: Hand) (remainingFlips: uint) : Fate * Hand =
            match draw () with
            | ActionCard Card.SecondChance as card when not (List.contains card hand) ->
                continueFlips (card :: hand) remainingFlips
            | ActionCard Card.SecondChance as card ->
                discard card
                continueFlips hand remainingFlips
            | ActionCard Card.Freeze as card ->
                discard card
                Fate.Frozen, hand
            | ActionCard Card.Deal3 as card ->
                discard card
                continueFlips hand (remainingFlips + 3u)
            | card ->
                let isBust, reducedHand, removed = Hand.Reduce(card :: hand)
                removed |> List.iter discard

                if isBust then
                    Fate.Busted, reducedHand
                elif Hand.HasFlip7Bonus reducedHand then
                    flip7.Value <- true
                    Fate.Alive, reducedHand
                else
                    continueFlips reducedHand remainingFlips

        and continueFlips (hand: Hand) (remainingFlips: uint) : Fate * Hand =
            if remainingFlips = 0u then
                Fate.Alive, hand
            else
                resolve hand (remainingFlips - 1u)

        // Play out the current round: the active rotation keeps taking turns
        // until everyone has stood, busted, or frozen, or a flip7 lands.
        // Banked hands are collected and discarded at round end, like the
        // real rules, so every card stays accounted for going into the
        // simulated future rounds.
        let banked = ResizeArray<string * uint>()
        let bankedHands = ResizeArray<Hand>()

        let bank (rollout: RolloutPlayer) (isBust: bool) =
            let score = if isBust then 0u else Hand.Score rollout.Hand
            banked.Add(rollout.Name, rollout.FirmScore + score)
            bankedHands.Add rollout.Hand

        let toRollout (p: Strategy.StrategyPlayer) = {
            Name = p.Name
            FirmScore = p.FirmScore
            Hand = p.Hand
            Turn = turn
        }

        let asStrategyPlayer (p: RolloutPlayer) : Strategy.StrategyPlayer = {
            Name = p.Name
            FirmScore = p.FirmScore
            Hand = p.Hand
        }

        for finishedPlayer in finished do
            bank (toRollout finishedPlayer) (Hand.IsBust finishedPlayer.Hand)

        let active =
            System.Collections.Generic.Queue<RolloutPlayer>(toRollout player :: (others |> List.map toRollout))

        let mutable firstDecision = Some forced

        while active.Count > 0 && not flip7.Value do
            let current = active.Dequeue()

            let choice =
                match firstDecision with
                | Some action ->
                    firstDecision <- None
                    action
                | None when List.isEmpty current.Hand -> Strategy.Hit
                | None ->
                    Strategy.DecideWith
                        random
                        (strategies |> Map.find current.Name)
                        round
                        current.Turn
                        (asStrategyPlayer current)
                        (active |> Seq.map asStrategyPlayer |> List.ofSeq)
                        decks.Value

            match choice with
            | Strategy.Stand -> bank current false
            | Strategy.Hit ->
                let fate, hand = resolve current.Hand 0u
                let current = { current with Hand = hand }

                match fate with
                | Fate.Busted -> bank current true
                | Fate.Frozen -> bank current false
                | Fate.Alive when flip7.Value -> bank current false
                | Fate.Alive -> active.Enqueue { current with Turn = current.Turn + 1u }

        // A flip7 ends the round for everyone still active
        for remaining in active do
            bank remaining false

        for hand in bankedHands do
            hand |> List.iter discard

        let totals = banked |> Map.ofSeq

        // Rounds after this one run through the real simulator, everyone
        // playing their sampled strategy
        let finals =
            if totals |> Map.exists (fun _ total -> total >= 200u) then
                totals
            else
                let deck, discards = decks.Value

                let seat =
                    totals
                    |> Map.toList
                    |> List.map (fun (name, _) -> name, strategies |> Map.find name)

                Timeline.SimulateWith random seat None (Some totals) (Some deck) (Some discards)
                |> AsyncSeq.tryLast
                |> Async.RunSynchronously
                |> Option.map (fun instant -> instant.Players |> List.map (fun p -> p.Name, p.FirmScore) |> Map.ofList)
                |> Option.defaultValue totals

        let mine = finals |> Map.find player.Name
        let best = finals |> Map.remove player.Name |> Map.values |> Seq.fold max 0u

        if mine > best then 1.0
        elif mine = best then 0.5
        else 0.0

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
        live <- Observation.FromTimeline gameSoFar
        fit ()

    /// <summary>
    /// Hit or stand by Monte Carlo best response: estimate the probability of
    /// ending the game with the top score under each action and take the
    /// better one. Opponents play strategies sampled from their posteriors
    /// (expected-value play when unmodeled); the SuperAI's own rollout policy
    /// is also expected-value play, since it cannot recurse into itself.
    /// </summary>
    member _.Decide
        (random: System.Random)
        (round: uint)
        (turn: uint)
        (player: Strategy.StrategyPlayer)
        (others: Strategy.StrategyPlayer list)
        (finished: Strategy.StrategyPlayer list)
        (decks: Deck * Deck)
        : Strategy.HitOrStand
        =
        // A derived stream so rollouts do not perturb the game's randomness
        let rng = System.Random(random.Next())

        let sampleStrategies () =
            (player.Name, MaximizesExpectedValue)
            :: (others @ finished
                |> List.map (fun opponent ->
                    let strategy =
                        match models |> Map.tryFind opponent.Name with
                        | Some model -> Inference.SampleWith rng model
                        | None -> MaximizesExpectedValue

                    opponent.Name, strategy
                ))
            |> Map.ofList

        let estimate (action: Strategy.HitOrStand) : float =
            Seq.init
                rollouts
                (fun _ -> SuperAI.Rollout rng (sampleStrategies ()) action round turn player others finished decks)
            |> Seq.average

        let hit = estimate Strategy.Hit
        let stand = estimate Strategy.Stand

        if hit > stand then
            Strategy.Hit
        elif stand > hit then
            Strategy.Stand
        else
            Strategy.DecideWith rng MaximizesExpectedValue round turn player others decks
