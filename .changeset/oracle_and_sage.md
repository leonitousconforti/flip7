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
through the game so far its seats' questions come with. A Sage carries what it
studied into its game as the next of the study's sitting, and scores its game
over the study's grids. Like `Lookahead`, `Oracle` and `Sage` are .NET only.

The terminal app seats Sage in the first seat the humans leave open and bots in
the rest, each of which always plays the same strategy under the same name, so
that studying by name learns each bot. Before the game Sage studies the games
under `timelines/` in the order they were played, a day at a time, concluding
every day before today, and when the game ends it prints what it made of how
each person plays and aims. Every game is dealt from a `Random` fresh from its
seed; without `--seed`, one is drawn and printed at the end.
