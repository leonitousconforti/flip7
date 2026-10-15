# Flip7

The card game [Flip 7](https://rules.dized.com/game/dPDRM857TU-BFRF7LzGE0g) in
F#, for .NET 10: a rules engine that plays whole games, the odds a hand busts
and what hitting it is worth, an exact search of the decision, strategies to
play against, and inference of the strategy a player is using from the choices
they make. The terminal app built on it, for Linux, macOS and Windows, is
attached to every [GitHub
release](https://github.com/leonitousconforti/flip7/releases).

## Games as timelines

A game is a `Timeline`: one `Instant` per event, each a snapshot of the players,
the deck and the discards just after it. Every player has a strategy, which
decides when they hit or stand, and a targeting policy, which decides who gets
the action cards they draw. The game ends with the first round in which someone
reaches 200 points.

```fsharp
open FSharp.Control
open Flip7

let players = [
    "Alice", Strategy.PlaysLikeAHuman 20u, Targeting.PlaysSpitefully
    "Bob", Strategy.MaximizesExpectedValue, Targeting.PlaysGreedily(0.8, 0.2)
    "Chloe", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
]

let game = Timeline.SimulateWith (System.Random 7) players |> AsyncSeq.toArraySynchronously
let winner = (Array.last game).Players |> List.maxBy (fun player -> player.FirmScore)
```

A timeline is lazy and draws from its `Random` as it is enumerated, so a seeded
one reproduces a game when it is enumerated once. `Persistence` writes a
timeline to a directory as it goes, one subdirectory per instant, and reads it
back. `Timeline.SimulateWithDecider` sends every decision through a `Decider`
of your own, which is how the terminal app gives a seat to a person.

## The odds

`Simulation` prices the next card exactly, from the cards left to draw.

```fsharp
let hand = [ ValueCard Card.Twelve; ValueCard Card.Eleven; ModifierCard Card.Plus4 ]
let deck = List.fold Deck.Decrement Deck.Full hand

Hand.Score hand                                           // 27u
Simulation.probabilityToBust deck Deck.Empty hand false   // 0.231
Simulation.expectedValueOfHit deck Deck.Empty hand        // -1.46, so stand
```

`Lookahead` sees further. It searches every card that could come next, and
every card after those, as deep as it is told to, and returns what hitting is
worth over standing when the rest of the hand is played as well as it can be.
Nothing is sampled, and one card of sight agrees with `expectedValueOfHit`. It
takes the deck the hand was dealt from: the full deck unless one is given.

```fsharp
use search = new Lookahead(3, [ hand ])
search.GainFromHitting hand |> Async.RunSynchronously     // Ok -1.19
```

## Strategies

`Strategy` is data rather than a function, so it can be written out and parsed
back: thresholds such as `HitUntilScore` and `HitUntilBustProbability`,
`ChasesFlip7`, `MaximizesExpectedValue`, and `PlaysLikeAHuman`, a soft model of
how people play that grows reluctant as a hand becomes worth losing.
`Targeting` is the same for action cards. `PlaysSpitefully` freezes whoever is
closest to winning and deals three to whoever is closest to busting;
`PlaysGreedily` keeps a Freeze or a Deal3 for itself when its own hand wants
one.

## Inference

`Observation` extracts every hit and stand decision from a timeline, a recorded
one included, and `Inference` fits a posterior over the strategies that could
have made them.

```fsharp
let models =
    Observation.FromTimeline (AsyncSeq.ofSeq game)
    |> AsyncSeq.toListSynchronously
    |> Inference.Fit

for model in models do
    printfn "%s plays like %O" model.Name (Inference.MostLikely model)

// Alice plays like PlaysLikeAHuman 20
// Bob plays like MaximizesExpectedValue
// Chloe plays like HitUntilScore 24
```
