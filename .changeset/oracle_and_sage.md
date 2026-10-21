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
for as long as the game goes as the replay did. It follows the game only
through the game so far its seats' questions come with. Like `Lookahead`,
`Oracle` and `Sage` are .NET only.

An `Observation` now records aims as well as hit-or-stand decisions: its
`Choice` is `Choice1Of2` for a decision and `Choice2Of2` for an aim, and
`Observe` returns every observation an instant makes. A `PlayerModel` now holds
how a player aims as well as how they play: `Evidence`, `Combine`, `ModelFrom`
and `Fit` read both, and `MostLikely` and `SampleWith` give a strategy and a
targeting. `Likelihood` takes a targeting beside the strategy, `FitWith` a grid
of targetings beside the strategies, and `EvidenceWith` that grid and the names
aims are scored over.

The sides a player takes are carried from game to game rather than pooled:
`Inference.Carry` carries a player's evidence into the next game of a sitting,
such as a day's games, keeping the sides they took with a given probability,
and `Inference.Conclude` ends a sitting, forgetting the sides and keeping how
they otherwise aim. A `Study` takes its games as one sitting, carrying sides
from each game into the next with probability 0.8; `Study.ContinueWith`
studies more games onto one, at a given probability of each choice out of
character, and `Study.Conclude` ends its sitting. A Sage carries what it
studied into its game as the next of the sitting. `Study.OfTimelinesWith`
studies games over a grid of strategies and a grid of targeting policies of
one's own, such as the default grids together with what a table is known to
play, and a Sage given the study scores its game over the same grids.

`Targeting.TakesSides` is a new targeting policy: it favours a player, opposes
one, or both, and otherwise aims by the targeting it holds, nesting at most
three deep, written like
`TakesSides for "Carol" against "Bob" otherwise PlaysSpitefully`. The engine
plays it like any other, and Inference fits it over the names a player is seen
aiming at.

The terminal app seats Sage in the first seat the humans leave open and bots in
the rest, each of which always plays the same strategy under the same name, so
that studying by name learns each bot. Before the game Sage studies the games
under `timelines/` in the order they were played, a day at a time, concluding
every day before today, and when the game ends it prints what it made of how
each person plays and aims. Every game is dealt from a `Random` fresh from its
seed; without `--seed`, one is drawn and printed at the end.

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
