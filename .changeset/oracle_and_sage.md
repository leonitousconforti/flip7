---
default: minor
---

# Add `Oracle` and `Sage`

`Oracle` recovers the seed of the `System.Random` a game was dealt with from its
timeline alone, usually within the first dozen cards, without knowing anyone's
strategy. It is immutable and takes one instant at a time, and once it has
pinned the seed it gives a `Random` wound to where the game stands, from which
`Timeline.ContinueWith` plays the rest of the game again.

`Sage` plays one game for a protagonist and any supporters, answering for their
seats, which a table seats as `Custom` and `ChoosesExternally` under their names
and registers from `Sage.Strategies` with `Strategy.addHitOrStandStrategy` and
`addTargetingStrategy`. It models every player by name, from a `Study` of
earlier timelines that any number of Sages can share and from every decision and
aim the game shows it, and recovers the game's seed with an `Oracle` as it goes.
Once the seed is pinned it replays the rest of the game with everyone on their
most likely strategy and aim, searches for answers that win it, and plays them
for as long as the game goes as the replay did. `Sage.Follow` shows it a game
without asking it anything. Like `Lookahead`, `Oracle` and `Sage` are .NET only.

An `Observation` now records aims as well as hit-or-stand decisions: its
`Choice` is `Choice1Of2` for a decision and `Choice2Of2` for an aim, and
`Observe` returns every observation an instant makes. A `PlayerModel` now holds
how a player aims as well as how they play: `Evidence`, `Combine`, `ModelFrom`
and `Fit` read both, and `MostLikely` and `SampleWith` give a strategy and an
aiming. `Likelihood` takes an aiming beside the strategy, `FitWith` a grid of
targetings beside the strategies, and `EvidenceWith` that grid and the names
aims are scored over.

The terminal app seats Sage in the first seat the humans leave open and bots in
the rest, each of which always plays the same strategy under the same name, so
that studying by name learns each bot. Before the game Sage studies the games
under `timelines/`, and when the game ends it prints what it made of how each
person plays and aims. Every game is dealt from a `Random` fresh from its seed;
without `--seed`, one is drawn and printed at the end.

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
