namespace Flip7.Analysis

open Flip7

/// <summary>
/// An adaptive opponent for interactive play: fits a posterior model of every
/// player it has seen (persisted games from earlier sessions plus the rounds
/// of the current game as they finish) and uses those models to decide its own
/// hits and stands. Version 1 decides with an expected-value baseline plus
/// race awareness from the learned models; Monte Carlo best response over
/// strategies sampled from the posteriors is the planned upgrade.
/// </summary>
type public SuperAI(history: Instant list list) =
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
    /// What an opponent is projected to end the round with: their firm score
    /// plus the stand target their most likely strategy aims for, when it has
    /// a threshold to read, and never less than what they already hold.
    /// Players who already stood or were frozen are not visible here (the
    /// decider only sees active players), a known v1 blind spot.
    /// </summary>
    member private _.Projected(other: Strategy.StrategyPlayer) : uint =
        let floor = other.FirmScore + Hand.Score other.Hand

        match models |> Map.tryFind other.Name |> Option.map Inference.MostLikely with
        | Some(HitUntilScore target) -> max floor (other.FirmScore + target)
        | Some(SoftHitUntilScore(target, _)) -> max floor (other.FirmScore + target)
        | Some(ChasesFlip7(target, _)) -> max floor (other.FirmScore + target)
        | Some(EmboldenedBySecondChance target) -> max floor (other.FirmScore + target)
        | Some(HitUntilTotal total) -> max floor total
        | _ -> floor

    /// <summary>
    /// Hit or stand for the SuperAI's own turn. The baseline is pure expected
    /// value; the learned models add race awareness at the end of the game:
    /// stand immediately when standing wins outright over every projected
    /// finish, and keep hitting when a modeled opponent is projected to end
    /// the game ahead, because standing would concede.
    /// </summary>
    member self.Decide
        (random: System.Random)
        (round: uint)
        (turn: uint)
        (player: Strategy.StrategyPlayer)
        (others: Strategy.StrategyPlayer list)
        (decks: Deck * Deck)
        : Strategy.HitOrStand
        =
        let tentative = player.FirmScore + Hand.Score player.Hand
        let bestProjected = others |> List.map self.Projected |> List.fold max 0u

        if tentative >= 200u && tentative > bestProjected then
            Strategy.Stand
        elif bestProjected >= 200u && tentative <= bestProjected then
            Strategy.Hit
        else
            Strategy.DecideWith random MaximizesExpectedValue round turn player others decks
