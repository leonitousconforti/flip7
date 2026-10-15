# Changelog
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
