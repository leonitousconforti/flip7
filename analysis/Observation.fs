namespace Flip7.Analysis

open Flip7

/// <summary>
/// A single voluntary hit-or-stand decision reconstructed from a timeline: the
/// actor's pre-decision state (taken from the instant immediately before the
/// event) and the choice they made. Player, OtherPlayers, and Decks mirror
/// exactly what Strategy.DecideWith receives, so any candidate strategy can be
/// replayed against an observation.
/// </summary>
type public Observation = {
    Name: string
    Choice: Strategy.HitOrStand
    Player: Strategy.StrategyPlayer
    OtherPlayers: Strategy.StrategyPlayer list
    Decks: Deck * Deck
}

module public Observation =
    let private ToStrategyPlayer (player: Player) : Strategy.StrategyPlayer = {
        Name = player.Name
        FirmScore = player.FirmScore
        Hand = player.Hand
    }

    /// <summary>
    /// The player whose choice produced an event, and what that choice was, or
    /// None for events not produced by a choice. Drawing a Freeze,
    /// SecondChance, or Deal3 still counts as the drawer choosing to hit; how
    /// the drawn card resolves afterwards is not their hit-or-stand decision.
    /// </summary>
    let public Actor (event: Event) : (string * Strategy.HitOrStand) option =
        match event with
        | Drew(name, _) -> Some(name, Strategy.Hit)
        | Busted(name, _) -> Some(name, Strategy.Hit)
        | SecondChanceDiscarded name -> Some(name, Strategy.Hit)
        | Froze(source, _) -> Some(source, Strategy.Hit)
        | SecondChancePassed(source, _) -> Some(source, Strategy.Hit)
        | Dealt3(source, _, _) -> Some(source, Strategy.Hit)
        | Stood name -> Some(name, Strategy.Stand)
        | Flip7Achieved _ -> None
        | RoundEnded _ -> None

    /// <summary>
    /// Extracts every voluntary hit-or-stand decision from a timeline. Each
    /// instant is a snapshot immediately after its event, so the preceding
    /// instant is the exact state the actor decided from. Flips made while the
    /// actor's hand was empty are dealing, not decisions, and are excluded;
    /// this also covers the first event of every round, whose predecessor is
    /// the previous RoundEnded snapshot with all hands discarded.
    ///
    /// OtherPlayers contains only the players still in the round, matching
    /// what Strategy.DecideWith receives: busted players are recognized by
    /// their still-bust hands, players who stood or were frozen by replaying
    /// the round's events.
    /// </summary>
    let public FromTimeline (timeline: Timeline) : Observation list =
        let observe (finished: Set<string>) (before: Instant) (event: Event) : Observation option =
            Actor event
            |> Option.bind (fun (name, choice) ->
                before.Players
                |> List.tryFind (fun player -> player.Name = name)
                |> Option.filter (fun actor -> not (List.isEmpty actor.Hand))
                |> Option.map (fun actor -> {
                    Name = name
                    Choice = choice
                    Player = ToStrategyPlayer actor
                    OtherPlayers =
                        before.Players
                        |> List.filter (fun player ->
                            player.Name <> name
                            && not (Set.contains player.Name finished)
                            && not (Hand.IsBust player.Hand)
                        )
                        |> List.map ToStrategyPlayer
                    Decks = before.Deck, before.Discards
                })
            )

        let step (finished: Set<string>, previous: Instant option, observations: Observation list) (instant: Instant) =
            let observations' =
                match previous |> Option.bind (fun before -> observe finished before instant.Event) with
                | Some observation -> observation :: observations
                | None -> observations

            let finished' =
                match instant.Event with
                | Stood name -> Set.add name finished
                | Froze(_, target) -> Set.add target finished
                | RoundEnded _ -> Set.empty
                | _ -> finished

            finished', Some instant, observations'

        let _, _, observations = timeline |> Seq.fold step (Set.empty, None, [])
        observations |> List.rev

    /// <summary>
    /// Extracts and pools the decisions of many timelines, e.g. every
    /// persisted game a household has played.
    /// </summary>
    let public FromTimelines (timelines: Timeline seq) : Observation list =
        timelines |> Seq.collect FromTimeline |> Seq.toList
