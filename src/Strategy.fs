namespace Flip7

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
    /// A strategy is a function that takes the current session number, the
    /// player, the list of other players, and the decks, and returns whether to
    /// hit or stand.
    /// </summary>
    type public Strategy = unit -> StrategyPlayer -> StrategyPlayer list -> (Deck * Deck) -> HitOrStand

    /// <summary>
    /// A strategy that always hits.
    /// </summary>
    let public AlwaysHits: Strategy = fun _session _player _otherPlayers _decks -> Hit

    /// <summary>
    /// A strategy that always stands.
    /// </summary>
    let public AlwaysStands: Strategy =
        fun _session _player _otherPlayers _decks -> Stand

    /// <summary>
    /// A strategy that randomly hits or stands with a given probability.
    /// </summary>
    let public RandomWithProbability: float -> Strategy =
        let random = System.Random()
        fun probability _session _player _otherPlayers _decks ->
            if random.NextDouble() < probability then Hit else Stand

    /// <summary>
    /// A strategy that randomly hits or stands with a 50% probability.
    /// </summary>
    let public Random: Strategy = RandomWithProbability 0.5

    /// <summary>
    /// A strategy that hits until the hand's score is at least the given
    /// threshold, then stands.
    /// </summary>
    let public HitUntilScore: uint -> Strategy =
        fun threshold _session player _otherPlayers _decks -> if Hand.Score player.Hand < threshold then Hit else Stand

    /// <summary>
    /// A strategy that hits until the hand has a certain number of cards, then
    /// stands.
    /// </summary>
    let public HitUntilNumCards: uint -> Strategy =
        fun threshold _session player _otherPlayers _decks ->
            if uint (List.length player.Hand) < threshold then
                Hit
            else
                Stand

/// <summary>
/// A strategy is a function that takes the current session number, the
/// player, the list of other players, and the decks, and returns whether to
/// hit or stand.
/// </summary>
type public Strategy = Strategy.Strategy
