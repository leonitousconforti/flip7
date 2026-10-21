# Changelog
## 0.1.2 (2026-10-21)

### Features

#### Model how players aim and take sides, and study earlier games

An `Observation` now records aims as well as hit-or-stand decisions: its
`Choice` is `Choice1Of2` for a decision and `Choice2Of2` for an aim, and
`Observe` returns every observation an instant makes. A `PlayerModel` now holds
how a player aims as well as how they play, in `TargetingPosterior` beside
`StrategyPosterior`, and `MostLikely` gives a strategy and a targeting.
`Likelihood` takes a targeting beside the strategy. `Evidence` replaces
`EvidenceWith`, scoring a player's observations over a grid of strategies, such
as `Inference.DefaultStrategyCandidates`, which was `DefaultCandidates`, and a
grid of targeting policies, such as the new
`Inference.DefaultTargetingCandidates`, at a given probability of each choice
out of character. `Fit`, `FitWith`, `Accuracy` and `SampleWith` are gone.

A `Study` holds what earlier games showed of each player, by name:
`Study.OfTimelines` studies timelines over the default grids,
`Study.OfTimelinesWith` over a grid of strategies and a grid of targeting
policies of one's own, such as the default grids together with what a table is
known to play, and `Study.ModelOf` gives what it makes of a player.

The sides a player takes are carried from game to game rather than pooled:
`Inference.Carry` carries a player's evidence into the next game of a sitting,
such as a day's games, keeping the sides they took with a given probability,
and `Inference.Conclude` ends a sitting, forgetting the sides and keeping how
they otherwise aim. A `Study` takes its games as one sitting, carrying sides
from each game into the next with probability 0.8; `Study.ContinueWith`
studies more games onto one, at a given probability of each choice out of
character, and `Study.Conclude` ends its sitting.

`Targeting.TakesSides` is a new targeting policy: it favours a player, opposes
one, or both, and otherwise aims by the targeting it holds, nesting at most
three deep, written like
`TakesSides for "Carol" against "Bob" otherwise PlaysSpitefully`. The engine
plays it like any other, and Inference fits it over the names a player is seen
aiming at.

Fixes:

- `Simulation.probabilityToBust` reads the discards reshuffled once the deck
  runs out, as `Simulation.expectedValueOfHit` already did, instead of returning
  0.
- `Lookahead` passes on a second second chance drawn onto a hand that holds one,
  as the engine does, instead of keeping it as extra cover.
- After an `Edited` instant, `Observation` counts everyone still in the round as
  having taken as many turns as the asker had, as the play editor continues the
  game.
- Drawing when the deck and the discards are both empty raises an
  `InvalidOperationException` instead of looping forever.
- `Strategy.TryParse` returns None for a count that does not fit a `uint`, such
  as `HitUntilScore -1`, instead of raising `OverflowException`.
- `Deck.TryDeserialize` returns None for any deck `Deck.Serialize` would not
  write, instead of raising or reading it anyway.
- A `TimelineStore` that reads an instant before ingesting it no longer caches
  it twice, which let the cache grow past its capacity.

## 0.1.1 (2026-10-15)

### Features

#### Compile with Fable

The package carries its F# sources, so Fable can compile it to JavaScript.
`Lookahead` and `Persistence` stay .NET only, as do the obsolete helpers that
draw from `System.Random.Shared`.

## 0.1.0 (2026-10-14)

### Breaking Changes

#### Initial release

`Flip7`: the card game Flip 7 for .NET 10. A rules engine for the official
rules and FAQ that plays whole games as lazy timelines, one instant per event,
with persistence of timelines to disk; the exact probability that the next card
busts a hand and the expected value of hitting it; `Lookahead`, an exact search
of whether to hit with any number of cards of sight; strategies from simple
thresholds to `MaximizesExpectedValue` and `PlaysLikeAHuman`, with targeting
policies for action cards; and `Inference`, which fits a posterior over the
strategies to the decisions a player was seen making.

The terminal app, attached to the release as a single self-contained executable
for Linux, macOS on Intel and Apple silicon, and Windows: play a game against
the strategies, simulate one and scrub through it as it unfolds, or replay a
recorded one.
