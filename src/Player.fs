namespace Flip7

module Strategy =
    /// <summary>
    /// To hit or to stand.
    /// </summary>
    type public HitOrStand =
        | Hit
        | Stand

type public Strategy = uint -> Player -> Player list -> (Deck * Deck) -> Strategy.HitOrStand
and public Player = {
    Name: string
    Strategy: Strategy
    FirmScore: uint
    Hand: Hand
} with

    static member Make(name: string, strategy: Strategy, ?firmScore: uint, ?hand: Hand) : Player =
        let hand = defaultArg hand []
        let firmScore = defaultArg firmScore 0u

        {
            Name = name
            Strategy = strategy
            FirmScore = firmScore
            Hand = hand
        }
