namespace Flip7

module Strategy =
    /// <summary>
    /// A strategy that always hits.
    /// </summary>
    let public AlwaysHits: Strategy =
        fun _session _player _otherPlayers _decks -> Strategy.Hit

    /// <summary>
    /// A strategy that always stands.
    /// </summary>
    let public AlwaysStands: Strategy =
        fun _session _player _otherPlayers _decks -> Strategy.Stand

    /// <summary>
    /// A strategy that randomly decides to hit or stand with equal probability.
    /// </summary>
    let public Random: Strategy =
        let random = System.Random()
        fun _session _player _otherPlayers _decks ->
            if random.NextDouble() > 0.5 then
                Strategy.Hit
            else
                Strategy.Stand

    /// <summary>
    /// A strategy that randomly decides to hit or stand with given probability.
    /// </summary>
    let public RandomWithProbability: float -> Strategy =
        let random = System.Random()
        fun probability _session _player _otherPlayers _decks ->
            if random.NextDouble() < probability then
                Strategy.Hit
            else
                Strategy.Stand

    /// <summary>
    /// A strategy that hits until the hand's score reaches a certain threshold,
    /// then stands.
    /// </summary>
    let public HitUntilScore: uint -> Strategy =
        fun threshold _session player _otherPlayers _decks ->
            if Hand.Score player.Hand < threshold then
                Strategy.Hit
            else
                Strategy.Stand

    /// <summary>
    /// A strategy that hits until the hand has a certain number of cards, then
    /// stands.
    /// </summary>
    let public HitUntilNumCards: uint -> Strategy =
        fun threshold _session player _otherPlayers _decks ->
            if uint (List.length player.Hand) < threshold then
                Strategy.Hit
            else
                Strategy.Stand

    /// <summary>
    /// A strategy that prompts the user for input to decide whether to hit or
    /// stand.
    /// </summary>
    let rec public Prompt: Strategy =
        fun _session _player _otherPlayers _decks ->
            match System.Console.ReadLine() with
            | "h" -> Strategy.Hit
            | "hit" -> Strategy.Hit
            | "s" -> Strategy.Stand
            | "stand" -> Strategy.Stand
            | _ -> Prompt _session _player _otherPlayers _decks
