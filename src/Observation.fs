namespace Flip7

open FSharp.Control

type public Observation = {
    Name: string
    Choice: Strategy.HitOrStand
    Round: uint
    Turn: uint
    Player: Player
    OtherPlayers: Player list
    FinishedPlayers: Player list
    Deck: Deck
    Discards: Deck
}

module public Observation =
    // The player whose choice produced an event, and what that choice was, or
    // None for events not produced by a choice. Drawing a Freeze, SecondChance,
    // or Deal3 still counts as the drawer choosing to hit; how the drawn card
    // resolves afterwards is not their hit-or-stand decision. Not
    // Event.Actor(), which answers whose state an event changed: for a Froze, a
    // SecondChancePassed, or a Dealt3 that is the target, not the player who
    // chose to hit.
    let private Decider (event: Event) : (string * Strategy.HitOrStand) option =
        match event with
        | Drew(name, _) -> Some(name, Strategy.Hit)
        | Busted(name, _) -> Some(name, Strategy.Hit)
        | SecondChanceDiscarded name -> Some(name, Strategy.Hit)
        | Froze(source, _) -> Some(source, Strategy.Hit)
        | SecondChancePassed(source, _) -> Some(source, Strategy.Hit)
        | Dealt3(source, _, _) -> Some(source, Strategy.Hit)
        | Stood name -> Some(name, Strategy.Stand)
        | TableSeated -> None
        | Edited _ -> None
        | Flip7Achieved _ -> None
        | RoundEnded _ -> None

    // How many of a deal3's flipped cards were set aside to be given out
    // afterwards. Each one resolves as a later Froze or Dealt3 event whose
    // source is the deal3's target, and those are not voluntary decisions
    let private SetAsides (cards: Card list) : int =
        cards
        |> List.sumBy (fun card ->
            match card with
            | ActionCard Card.Freeze
            | ActionCard Card.Deal3 -> 1
            | _ -> 0
        )

    /// <summary>
    /// Extracts every voluntary hit-or-stand decision from a timeline. Each
    /// instant is a snapshot immediately after its event, so the preceding
    /// instant is the exact state the actor decided from. A player's first turn
    /// of every round is a forced hit the engine never asks their strategy
    /// about - it is dealing, not a decision - and is excluded; the turn is
    /// reconstructed by counting the player's decision events, which matches
    /// the engine's count because every turn produces exactly one such event.
    /// Froze and Dealt3 events that resolve a set-aside card from an earlier
    /// deal3 are the flipper giving out cards they were dealt, not a voluntary
    /// hit, and are excluded too.
    ///
    /// OtherPlayers contains only the players still in the round, matching what
    /// the hit-or-stand decider receives: busted players are recognized by
    /// their still-bust hands, players who stood or were frozen by replaying
    /// the round's events; both land in FinishedPlayers.
    ///
    /// Observations are produced asynchronously as instants arrive, so a live
    /// game can be observed while it plays and unbounded timelines work.
    /// Re-enumerating replays the timeline from the start.
    /// </summary>
    let public FromTimeline (timeline: Timeline) : AsyncSeq<Observation> =
        let step
            (
                finished: Set<string>,
                round: uint,
                turns: Map<string, uint>,
                pending: Map<string, int>,
                previous: Instant option,
                _: Observation option
            )
            (instant: Instant)
            =
            let owed (name: string) : int =
                pending |> Map.tryFind name |> Option.defaultValue 0

            let voluntary =
                match instant.Event with
                | Froze(source, _)
                | Dealt3(source, _, _) -> owed source = 0
                | _ -> true

            let pending' =
                match instant.Event with
                | Froze(source, _) when not voluntary -> pending |> Map.add source (owed source - 1)
                | Dealt3(source, target, cards) ->
                    let spent =
                        if voluntary then
                            pending
                        else
                            pending |> Map.add source (owed source - 1)

                    let debt = (spent |> Map.tryFind target |> Option.defaultValue 0) + SetAsides cards
                    spent |> Map.add target debt
                | _ -> pending

            let turns' =
                match Decider instant.Event with
                | Some(name, _) when voluntary ->
                    let taken = turns |> Map.tryFind name |> Option.defaultValue 0u
                    turns |> Map.add name (taken + 1u)
                | _ -> turns

            let observe (before: Instant) : Observation option =
                Decider instant.Event
                |> Option.filter (fun _ -> voluntary)
                |> Option.filter (fun (name, _) -> Map.find name turns' > 1u)
                |> Option.bind (fun (name, choice) ->
                    before.Players
                    |> List.tryFind (fun player -> player.Name = name)
                    |> Option.map (fun actor ->
                        let others, finishedPlayers =
                            before.Players
                            |> List.filter (fun player -> player.Name <> name)
                            |> List.partition (fun player ->
                                not (Set.contains player.Name finished) && not (Hand.IsBust player.Hand)
                            )

                        {
                            Name = name
                            Choice = choice
                            Round = round
                            Turn = turns' |> Map.find name
                            Player = actor
                            OtherPlayers = others
                            FinishedPlayers = finishedPlayers
                            Deck = before.Deck
                            Discards = before.Discards
                        }
                    )
                )

            let finished', round', turns'', pending'' =
                match instant.Event with
                | Stood name -> Set.add name finished, round, turns', pending'
                | Froze(_, target) -> Set.add target finished, round, turns', pending'
                | RoundEnded _ -> Set.empty, round + 1u, Map.empty, Map.empty
                | _ -> finished, round, turns', pending'

            finished', round', turns'', pending'', Some instant, previous |> Option.bind observe

        timeline
        |> AsyncSeq.scan step (Set.empty, 1u, Map.empty, Map.empty, None, None)
        |> AsyncSeq.choose (fun (_, _, _, _, _, observation) -> observation)

    /// <summary>
    /// Extracts and pools the decisions of many timelines, e.g. every persisted
    /// game a household has played.
    /// </summary>
    let public FromTimelines (timelines: Timeline seq) : AsyncSeq<Observation> =
        timelines |> AsyncSeq.ofSeq |> AsyncSeq.collect FromTimeline
