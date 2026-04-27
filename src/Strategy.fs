namespace Flip7

module Strategy =
    /// <summary>
    /// To hit or to stand.
    /// </summary>
    type public HitOrStand =
        | Hit
        | Stand

    /// <summary>
    /// A strategy is a function that takes the current session number, the
    /// player's hand, the other players' hands, and the remaining deck, and
    /// returns whether to hit or stand.
    /// </summary>
    type public Strategy = uint -> Hand -> Hand list -> Deck -> HitOrStand

    /// <summary>
    /// A strategy that always hits.
    /// </summary>
    let public AlwaysHits: Strategy = fun _session _hand _otherInHands _deck -> Hit

    /// <summary>
    /// A strategy that always stands.
    /// </summary>
    let public AlwaysStands: Strategy = fun _session _hand _otherInHands _deck -> Stand

    /// <summary>
    /// A strategy that randomly decides to hit or stand with equal probability.
    /// </summary>
    let public Random: Strategy =
        let random = System.Random()
        fun _session _hand _otherInHands _deck -> if random.NextDouble() > 0.5 then Hit else Stand

    /// <summary>
    /// A strategy that randomly decides to hit or stand with given probability.
    /// </summary>
    let public RandomWithProbability: float -> Strategy =
        let random = System.Random()
        fun probability _session _hand _otherInHands _deck -> if random.NextDouble() < probability then Hit else Stand

    /// <summary>
    /// A strategy that hits until the hand's score reaches a certain threshold,
    /// then stands.
    /// </summary>
    let public HitUntilScore: uint -> Strategy =
        fun threshold _session hand _otherInHands _deck -> if Hand.Score hand < threshold then Hit else Stand

    /// <summary>
    /// A strategy that hits until the hand has a certain number of cards, then
    /// stands.
    /// </summary>
    let public HitUntilNumCards: uint -> Strategy =
        fun threshold _session hand _otherInHands _deck -> if uint (List.length hand) < threshold then Hit else Stand

    /// <summary>
    /// A strategy that prompts the user for input to decide whether to hit or
    /// stand.
    /// </summary>
    let rec public Prompt: Strategy =
        fun _session hand _otherInHands _deck ->
            printfn "Your hand: %A (score: %d)" hand (Hand.Score hand)
            printfn "Do you want to hit or stand? (hit/stand)"
            match System.Console.ReadLine() with
            | "h" -> Hit
            | "hit" -> Hit
            | "s" -> Stand
            | "stand" -> Stand
            | _ ->
                printfn "Invalid input, please enter 'h' or 'hit' for hit or 's' or 'stand' for stand."
                Prompt _session hand _otherInHands _deck

/// <summary>
/// A strategy is a function that takes the current session number, the player's
/// hand, the other players' hands, and the remaining deck, and returns whether
/// to hit or stand.
/// </summary>
type public Strategy = Strategy.Strategy
