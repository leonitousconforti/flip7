namespace Flip7

type public Player = {
    Name: string
    Strategy: Strategy
    FirmScore: uint
    Hand: Hand
} with

    static member Make(name: string, strategy: Strategy, ?firmScore: uint, ?hand: Hand) : Player =
        let firmScore = defaultArg firmScore 0u
        let hand = defaultArg hand []

        {
            Name = name
            Strategy = strategy
            FirmScore = firmScore
            Hand = hand
        }

    member public this.ToStrategyPlayer() : Strategy.StrategyPlayer = {
        Name = this.Name
        FirmScore = this.FirmScore
        Hand = this.Hand
    }
