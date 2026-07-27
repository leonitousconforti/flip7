namespace Flip7

/// <summary>
/// A strategy decides whether a player hits or stands. Strategies are
/// represented as data rather than functions so that they can be serialized
/// and deserialized; use Strategy.Decide to evaluate one.
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

    override self.ToString() : string =
        match self with
        | AlwaysHits -> "AlwaysHits"
        | AlwaysStands -> "AlwaysStands"
        | RandomWithProbability probability ->
            let invariant =
                probability.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"RandomWithProbability {invariant}"
        | HitUntilScore threshold -> $"HitUntilScore {threshold}"
        | HitUntilNumCards threshold -> $"HitUntilNumCards {threshold}"
        | HitUntilBustProbability threshold ->
            let invariant =
                threshold.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"HitUntilBustProbability {invariant}"
        | HitUntilNaiveBustProbability threshold ->
            let invariant =
                threshold.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"HitUntilNaiveBustProbability {invariant}"
        | SoftHitUntilScore(threshold, temperature) ->
            let invariant =
                temperature.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"SoftHitUntilScore {threshold} {invariant}"
        | HitUntilTotal target -> $"HitUntilTotal {target}"
        | HitUntilUniqueValues threshold -> $"HitUntilUniqueValues {threshold}"
        | ChasesFlip7(score, uniques) -> $"ChasesFlip7 {score} {uniques}"
        | EmboldenedBySecondChance threshold -> $"EmboldenedBySecondChance {threshold}"
        | HitWhileBehindLeader margin -> $"HitWhileBehindLeader {margin}"
        | StandsAfterTurn turns -> $"StandsAfterTurn {turns}"
        | MaximizesExpectedValue -> "MaximizesExpectedValue"

    static member public Parse(string: string) : Strategy =
        match string.Split ' ' with
        | [| "AlwaysHits" |] -> AlwaysHits
        | [| "AlwaysStands" |] -> AlwaysStands
        | [| "RandomWithProbability"; probability |] ->
            RandomWithProbability(System.Double.Parse(probability, System.Globalization.CultureInfo.InvariantCulture))
        | [| "HitUntilScore"; threshold |] -> HitUntilScore(System.UInt32.Parse threshold)
        | [| "HitUntilNumCards"; threshold |] -> HitUntilNumCards(System.UInt32.Parse threshold)
        | [| "HitUntilBustProbability"; threshold |] ->
            HitUntilBustProbability(System.Double.Parse(threshold, System.Globalization.CultureInfo.InvariantCulture))
        | [| "HitUntilNaiveBustProbability"; threshold |] ->
            HitUntilNaiveBustProbability(
                System.Double.Parse(threshold, System.Globalization.CultureInfo.InvariantCulture)
            )
        | [| "SoftHitUntilScore"; threshold; temperature |] ->
            SoftHitUntilScore(
                System.UInt32.Parse threshold,
                System.Double.Parse(temperature, System.Globalization.CultureInfo.InvariantCulture)
            )
        | [| "HitUntilTotal"; target |] -> HitUntilTotal(System.UInt32.Parse target)
        | [| "HitUntilUniqueValues"; threshold |] -> HitUntilUniqueValues(System.UInt32.Parse threshold)
        | [| "ChasesFlip7"; score; uniques |] -> ChasesFlip7(System.UInt32.Parse score, System.UInt32.Parse uniques)
        | [| "EmboldenedBySecondChance"; threshold |] -> EmboldenedBySecondChance(System.UInt32.Parse threshold)
        | [| "HitWhileBehindLeader"; margin |] -> HitWhileBehindLeader(System.UInt32.Parse margin)
        | [| "StandsAfterTurn"; turns |] -> StandsAfterTurn(System.UInt32.Parse turns)
        | [| "MaximizesExpectedValue" |] -> MaximizesExpectedValue
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
    /// A strategy that randomly hits or stands with a 50% probability.
    /// </summary>
    let public Random: Strategy = RandomWithProbability 0.5

    /// <summary>
    /// Evaluates a strategy using the given source of randomness, given the
    /// current session number, the player, the list of other players, and the
    /// decks, returning whether to hit or stand.
    /// </summary>
    let public DecideWith
        (random: System.Random)
        (strategy: Strategy)
        (session: uint)
        (player: StrategyPlayer)
        (otherPlayers: StrategyPlayer list)
        (decks: Deck * Deck)
        : HitOrStand =
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
            // No other active players means freeze and deal3 cards must be
            // played on ourselves, so they count towards busting
            let onlyPlayer = List.isEmpty otherPlayers

            if Simulation.probabilityToBust deck discards player.Hand onlyPlayer < threshold then
                Hit
            else
                Stand
        | HitUntilNaiveBustProbability threshold ->
            // A naive player knows the full deck composition and what they
            // hold, but does not track the discards or other players' cards
            let unseen = player.Hand |> List.fold Deck.Decrement Deck.Full
            let onlyPlayer = List.isEmpty otherPlayers

            if Simulation.probabilityToBust unseen Deck.Empty player.Hand onlyPlayer < threshold then
                Hit
            else
                Stand
        | SoftHitUntilScore(threshold, temperature) ->
            // A noisy threshold: certain far from it, a coin flip at it. The
            // temperature (in points, positive) controls how wide the
            // uncertain band is
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
            // Plays like HitUntilScore until enough unique value cards put
            // the flip7 bonus within reach, then keeps flipping for it
            if uint (Hand.UniqueValueCards player.Hand) >= uniques then
                Hit
            elif Hand.Score player.Hand < score then
                Hit
            else
                Stand
        | EmboldenedBySecondChance threshold ->
            // A second chance card means the next duplicate cannot bust, so
            // hit fearlessly while holding one
            if player.Hand |> List.contains (ActionCard Card.SecondChance) then
                Hit
            elif Hand.Score player.Hand < threshold then
                Hit
            else
                Stand
        | HitWhileBehindLeader margin ->
            // Races the visible table: the best rival total among players
            // still in the round, counting their unbanked hands
            let total = player.FirmScore + Hand.Score player.Hand

            let leader =
                otherPlayers
                |> List.map (fun other -> other.FirmScore + Hand.Score other.Hand)
                |> List.fold max 0u

            if total < leader + margin then Hit else Stand
        | StandsAfterTurn turns -> if session < turns then Hit else Stand
        | MaximizesExpectedValue ->
            let deck, discards = decks

            if Simulation.expectedValueOfHit deck discards player.Hand > 0.0 then
                Hit
            else
                Stand

    /// <summary>
    /// Evaluates a strategy given the current session number, the player, the
    /// list of other players, and the decks, returning whether to hit or stand.
    /// </summary>
    let public Decide
        (strategy: Strategy)
        (session: uint)
        (player: StrategyPlayer)
        (otherPlayers: StrategyPlayer list)
        (decks: Deck * Deck)
        : HitOrStand =
        DecideWith System.Random.Shared strategy session player otherPlayers decks
