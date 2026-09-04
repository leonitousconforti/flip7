namespace Flip7

/// <summary>
/// A strategy decides whether a player hits or stands. Strategies are
/// represented as data rather than functions so that they can be serialized
/// and deserialized; use Strategy.DecideWith to evaluate one.
/// </summary>
type public Strategy =
    | AlwaysHits
    | AlwaysStands
    | RandomWithProbability of float
    | HitUntilScore of uint
    | HitUntilNumCards of uint
    | HitUntilBustProbability of float
    | HitUntilNaiveBustProbability of float
    | SoftHitUntilScore of Threshold: uint * Temperature: float
    | HitUntilTotal of uint
    | HitUntilUniqueValues of uint
    | ChasesFlip7 of Score: uint * Uniques: uint
    | EmboldenedBySecondChance of uint
    | HitWhileBehindLeader of uint
    | StandsAfterTurn of uint
    | MaximizesExpectedValue
    /// Decided by a human at the terminal; label only, routed to the decider
    /// injected into Timeline.SimulateWithDecider rather than DecideWith.
    | Prompt

    override self.ToString() : string =
        let writeFloat (value: float) =
            value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

        match self with
        | AlwaysHits -> "AlwaysHits"
        | AlwaysStands -> "AlwaysStands"
        | RandomWithProbability probability -> $"RandomWithProbability {writeFloat probability}"
        | HitUntilScore threshold -> $"HitUntilScore {threshold}"
        | HitUntilNumCards threshold -> $"HitUntilNumCards {threshold}"
        | HitUntilBustProbability threshold -> $"HitUntilBustProbability {writeFloat threshold}"
        | HitUntilNaiveBustProbability threshold -> $"HitUntilNaiveBustProbability {writeFloat threshold}"
        | SoftHitUntilScore(threshold, temperature) -> $"SoftHitUntilScore {threshold} {writeFloat temperature}"
        | HitUntilTotal target -> $"HitUntilTotal {target}"
        | HitUntilUniqueValues threshold -> $"HitUntilUniqueValues {threshold}"
        | ChasesFlip7(score, uniques) -> $"ChasesFlip7 {score} {uniques}"
        | EmboldenedBySecondChance threshold -> $"EmboldenedBySecondChance {threshold}"
        | HitWhileBehindLeader margin -> $"HitWhileBehindLeader {margin}"
        | StandsAfterTurn turns -> $"StandsAfterTurn {turns}"
        | MaximizesExpectedValue -> "MaximizesExpectedValue"
        | Prompt -> "Prompt"

    static member public Parse(string: string) : Strategy =
        let readFloat (value: string) =
            System.Double.Parse(value, System.Globalization.CultureInfo.InvariantCulture)

        let readUint (value: string) =
            System.UInt32.Parse(value, System.Globalization.CultureInfo.InvariantCulture)

        match string.Split ' ' with
        | [| "AlwaysHits" |] -> AlwaysHits
        | [| "AlwaysStands" |] -> AlwaysStands
        | [| "RandomWithProbability"; probability |] -> RandomWithProbability(readFloat probability)
        | [| "HitUntilScore"; threshold |] -> HitUntilScore(readUint threshold)
        | [| "HitUntilNumCards"; threshold |] -> HitUntilNumCards(readUint threshold)
        | [| "HitUntilBustProbability"; threshold |] -> HitUntilBustProbability(readFloat threshold)
        | [| "HitUntilNaiveBustProbability"; threshold |] -> HitUntilNaiveBustProbability(readFloat threshold)
        | [| "SoftHitUntilScore"; threshold; temp |] -> SoftHitUntilScore(readUint threshold, readFloat temp)
        | [| "HitUntilTotal"; target |] -> HitUntilTotal(readUint target)
        | [| "HitUntilUniqueValues"; threshold |] -> HitUntilUniqueValues(readUint threshold)
        | [| "ChasesFlip7"; score; uniques |] -> ChasesFlip7(readUint score, readUint uniques)
        | [| "EmboldenedBySecondChance"; threshold |] -> EmboldenedBySecondChance(readUint threshold)
        | [| "HitWhileBehindLeader"; margin |] -> HitWhileBehindLeader(readUint margin)
        | [| "StandsAfterTurn"; turns |] -> StandsAfterTurn(readUint turns)
        | [| "MaximizesExpectedValue" |] -> MaximizesExpectedValue
        | [| "Prompt" |] -> Prompt
        | _ -> raise (System.ArgumentException $"Invalid strategy string: {string}")

    static member TryParse(string: string) : Strategy option =
        try
            string |> Strategy.Parse |> Some
        with :? System.ArgumentException ->
            None

module public Strategy =
    /// <summary>
    /// To hit or to stand.
    /// </summary>
    type public HitOrStand =
        | Hit
        | Stand

    /// <summary>
    /// A simplified player type for strategies, containing only the information
    /// that strategies need to make decisions.
    /// </summary>
    type public StrategyPlayer = { Name: string; FirmScore: uint; Hand: Hand }

    /// <summary>
    /// Decides hit-or-stand for one player: given the player's declared
    /// strategy, the round, the turn, the player, the other active players,
    /// the players who already stood, busted, or were frozen this round, and
    /// the decks. DecideWith is the canonical Decider; injecting a different
    /// one into Timeline.SimulateWithDecider lets Prompt strategies be decided
    /// by a human at the terminal.
    /// </summary>
    type public Decider =
        Strategy
            -> uint
            -> uint
            -> StrategyPlayer
            -> StrategyPlayer list
            -> StrategyPlayer list
            -> (Deck * Deck)
            -> HitOrStand

    /// <summary>
    /// A strategy that randomly hits or stands with a 50% probability.
    /// </summary>
    let public Random: Strategy = RandomWithProbability 0.5

    /// <summary>
    /// Evaluates a strategy using the given source of randomness, given the
    /// current round number, the current turn (how many times play has come
    /// around the table this round, counting from one for the player being
    /// asked), the player, the other players still in the round, the players
    /// who already stood, busted, or were frozen this round, and the decks,
    /// returning whether to hit or stand.
    /// </summary>
    let public DecideWith (random: System.Random) : Decider =
        fun strategy round turn player otherPlayers finishedPlayers decks ->
            match strategy with
            | AlwaysHits -> Hit
            | AlwaysStands -> Stand
            | RandomWithProbability probability -> if random.NextDouble() < probability then Hit else Stand
            | HitUntilScore threshold -> if Hand.Score player.Hand < threshold then Hit else Stand
            | HitUntilNumCards threshold ->
                if uint (List.length player.Hand) < threshold then
                    Hit
                else
                    Stand
            | HitUntilBustProbability threshold ->
                let deck, discards = decks
                let onlyPlayer = List.isEmpty otherPlayers
                if Simulation.probabilityToBust deck discards player.Hand onlyPlayer < threshold then
                    Hit
                else
                    Stand
            | HitUntilNaiveBustProbability threshold ->
                let unseen = player.Hand |> List.fold Deck.Decrement Deck.Full
                let onlyPlayer = List.isEmpty otherPlayers
                if Simulation.probabilityToBust unseen Deck.Empty player.Hand onlyPlayer < threshold then
                    Hit
                else
                    Stand
            | SoftHitUntilScore(threshold, temperature) ->
                let distance = float (Hand.Score player.Hand) - float threshold
                let probability = 1.0 / (1.0 + exp (distance / temperature))
                if random.NextDouble() < probability then Hit else Stand
            | HitUntilTotal target ->
                if player.FirmScore + Hand.Score player.Hand < target then
                    Hit
                else
                    Stand
            | HitUntilUniqueValues threshold ->
                if uint (Hand.UniqueValueCards player.Hand) < threshold then
                    Hit
                else
                    Stand
            | ChasesFlip7(score, uniques) ->
                if uint (Hand.UniqueValueCards player.Hand) >= uniques then
                    Hit
                elif Hand.Score player.Hand < score then
                    Hit
                else
                    Stand
            | EmboldenedBySecondChance threshold ->
                if player.Hand |> List.contains (ActionCard Card.SecondChance) then
                    Hit
                elif Hand.Score player.Hand < threshold then
                    Hit
                else
                    Stand
            | HitWhileBehindLeader margin ->
                let total = player.FirmScore + Hand.Score player.Hand

                // The leader may already be out of the round: a stood or frozen
                // player's hand is locked in, while a busted player's is worthless
                let showing (other: StrategyPlayer) =
                    if Hand.IsBust other.Hand then
                        other.FirmScore
                    else
                        other.FirmScore + Hand.Score other.Hand

                let leader = otherPlayers @ finishedPlayers |> List.map showing |> List.fold max 0u

                if total < leader + margin then Hit else Stand
            | StandsAfterTurn turns -> if turn <= turns then Hit else Stand
            | MaximizesExpectedValue ->
                let deck, discards = decks
                if Simulation.expectedValueOfHit deck discards player.Hand > 0.0 then
                    Hit
                else
                    Stand
            | Prompt ->
                raise (
                    System.InvalidOperationException
                        $"{strategy} is decided externally via Timeline.SimulateWithDecider, not by DecideWith"
                )
