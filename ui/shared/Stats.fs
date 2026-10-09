module public TimelineStats

open Flip7

type public Stats = {
    /// How many rounds were played
    Rounds: float

    /// How many turns each player made by round
    Decisions: Map<uint, Map<string, float>>
    /// How many targeting decisions each player made by round
    Targeting: Map<uint, Map<string, float>>

    /// How many points each player scored by round
    Points: Map<uint, Map<string, float>>
    /// How many flip7s each player achieved by round
    Flip7s: Map<uint, Map<string, float>>
}

let public Empty: Stats = {
    Rounds = 0.0
    Decisions = Map.empty
    Targeting = Map.empty
    Points = Map.empty
    Flip7s = Map.empty
}

let private currentRound (stats: Stats) : uint = uint stats.Rounds + 1u
let private tally (byRound: Map<uint, Map<string, float>>) (round: uint) (player: string) =
    let players = byRound |> Map.tryFind round |> Option.defaultValue Map.empty
    let count = players |> Map.tryFind player |> Option.defaultValue 0.0
    byRound |> Map.add round (players |> Map.add player (count + 1.0))

let private updateRoundEnded (stats: Stats) (scoreboard: Map<string, uint>) : Stats =
    let round = currentRound stats
    let scoreboard' = scoreboard |> Map.map (fun _ score -> float score)
    let points = Map.add round scoreboard' stats.Points
    { stats with Points = points; Rounds = float round }

let public Update (stats: Stats) (instant: Instant) : Stats =
    let round = currentRound stats
    match instant.Event with
    | Event.RoundEnded scoreboard -> updateRoundEnded stats scoreboard
    | Event.Flip7Achieved player -> { stats with Flip7s = tally stats.Flip7s round player }
    | Event.Drew(player, _card) -> {
        stats with
            Decisions = tally stats.Decisions round player
      }
    | Event.Stood player -> {
        stats with
            Decisions = tally stats.Decisions round player
      }
    | Event.Froze(player, _target) -> {
        stats with
            Targeting = tally stats.Targeting round player
      }
    | Event.SecondChancePassed(player, _target) -> {
        stats with
            Targeting = tally stats.Targeting round player
      }
    | Event.Dealt3(player, _target, _cards) -> {
        stats with
            Targeting = tally stats.Targeting round player
      }
    | _ -> stats

// Stats paired with the games they cover merge as a games-weighted mean. The
// mean runs over every key on either side with an absent key counting as zero:
// a player who made no targeting decision in a game, or a round that only a
// long game reached, still averages in as nothing for that game
let public Aggregate ((games1, stats1): uint * Stats) ((games2, stats2): uint * Stats) : uint * Stats =
    if games1 + games2 = 0u then
        0u, Empty
    else

    let weight1, weight2 = float games1, float games2
    let mean (a: float) (b: float) : float =
        (a * weight1 + b * weight2) / (weight1 + weight2)

    let merge (zero: 'v) (combine: 'v -> 'v -> 'v) (a: Map<'k, 'v>) (b: Map<'k, 'v>) : Map<'k, 'v> =
        Seq.append (Map.keys a) (Map.keys b)
        |> Seq.distinct
        |> Seq.map (fun key ->
            let valueA = a |> Map.tryFind key |> Option.defaultValue zero
            let valueB = b |> Map.tryFind key |> Option.defaultValue zero
            key, combine valueA valueB
        )
        |> Map.ofSeq

    let meanByRound = merge Map.empty (merge 0.0 mean)

    games1 + games2,
    {
        Rounds = mean stats1.Rounds stats2.Rounds
        Decisions = meanByRound stats1.Decisions stats2.Decisions
        Targeting = meanByRound stats1.Targeting stats2.Targeting
        Points = meanByRound stats1.Points stats2.Points
        Flip7s = meanByRound stats1.Flip7s stats2.Flip7s
    }

let public Summary ((games, stats): uint * Stats) : string =
    let perPlayer (byRound: Map<uint, Map<string, float>>) : string =
        byRound
        |> Map.toList
        |> List.collect (fun (_round, players) -> Map.toList players)
        |> List.groupBy fst
        |> List.map (fun (player, rounds) -> player, rounds |> List.sumBy snd)
        |> List.sortBy fst
        |> List.map (fun (player, total) -> $"{player}: %.2f{total}")
        |> String.concat ", "

    $"Games: {games}, rounds per game: %.2f{stats.Rounds}\n"
    + $"Decisions per player per game: {perPlayer stats.Decisions}\n"
    + $"Targeting decisions per player per game: {perPlayer stats.Targeting}\n"
    + $"Points per player per game: {perPlayer stats.Points}\n"
    + $"Flip7s per player per game: {perPlayer stats.Flip7s}"
