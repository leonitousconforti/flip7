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

    override self.ToString() : string =
        match self with
        | AlwaysHits -> "AlwaysHits"
        | AlwaysStands -> "AlwaysStands"
        | RandomWithProbability probability ->
            let invariant = probability.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"RandomWithProbability {invariant}"
        | HitUntilScore threshold -> $"HitUntilScore {threshold}"
        | HitUntilNumCards threshold -> $"HitUntilNumCards {threshold}"
        | HitUntilBustProbability threshold ->
            let invariant = threshold.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            $"HitUntilBustProbability {invariant}"

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
