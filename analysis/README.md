# Flip7.Analysis

A prototype that builds a **player-specific model** from persisted timeline
data: given the games someone has played, infer what strategy they appear to
be following. The intended consumer is a future Monte Carlo advisor — rollouts
from a live game state need something to play the *other* players with, and a
posterior fitted to their actual past decisions is better than a guess.

## How it works

### 1. Decisions are reconstructed from timelines (`Observation.fs`)

A persisted timeline stores one full snapshot per event, and every instant is
the state *immediately after* its event. So for each consecutive pair of
instants, the earlier one is the exact state the actor decided from, and the
later one's event says what they chose:

| Event | Interpreted as |
| --- | --- |
| `Drew`, `Busted`, `SecondChanceDiscarded` | that player chose **Hit** |
| `Froze`, `SecondChancePassed`, `Dealt3` | the *source* chose **Hit** (drawing the action card was the hit; how it resolved was not their hit-or-stand decision) |
| `Stood` | that player chose **Stand** |
| `Flip7Achieved`, `RoundEnded` | not a decision |

Flips made while the actor's hand was empty are excluded: everyone is dealt at
least one card before they may choose, so those are dealing, not decisions.
This also covers the first event of every round, whose predecessor is the
previous `RoundEnded` snapshot with all hands discarded.

Each observation carries the same context `Strategy.DecideWith` receives
(`StrategyPlayer`, other players, deck and discards), so any candidate
strategy can be replayed against it. `OtherPlayers` contains only the players
still in the round, matching the simulator: busted players are recognized by
their still-bust hands, players who stood or were frozen by replaying the
round's events since the last `RoundEnded`.

### 2. A posterior over candidate strategies is fitted (`Inference.fs`)

The candidate grid is every existing `Strategy` case at a spread of parameter
values (`HitUntilScore 2..40`, `HitUntilNumCards 1..7`,
`HitUntilBustProbability 0.1..0.9`, a few `RandomWithProbability` values,
`AlwaysHits`, `AlwaysStands`) — deliberately, so that anything sampled from
the posterior can be handed straight to `Timeline.SimulateWith` as an
opponent model.

For each candidate, the likelihood of an observed choice uses a
trembling-hand noise model: with probability ε (default 0.1) the player
ignores their strategy and flips a coin. That keeps every likelihood positive,
so one out-of-character decision can't zero out an otherwise good candidate.
Log-likelihoods are summed per player and normalized into a posterior from a
uniform prior.

`Inference.SampleWith` draws a strategy from the posterior — the intended use
is one draw per Monte Carlo rollout (fixed within a rollout, resampled across
rollouts) so that uncertainty about a player propagates into the win-probability
estimate rather than being averaged away.

## Usage

```sh
# Simulate games with hidden strategies, persist them, read them back,
# fit models, and compare against the ground truth:
nix develop -c dotnet run --project analysis -- --demo 5

# Fit models from real persisted timelines (directories of numbered
# instant directories, as written by Persistence.WriteTimeline*):
nix develop -c dotnet run --project analysis -- --analyze timelines/*
```

Unit tests live in `test/AnalysisTests.fs` (extraction rules, consistency of
`ProbabilityOfHit` with `Strategy.DecideWith`, threshold recovery).

## Known limitations / next steps

- **Thresholds are only identified up to the data.** If no observed hand ever
  scored between 22 and 24, `HitUntilScore 23` variants explain the data
  equally well and posterior mass spreads across neighbors. That spread is
  honest — sampling rollouts from the posterior handles it correctly.
- **The persisted `Strategy` field is deliberately ignored.** Simulated
  timelines happen to record the true strategy, but live-companion data would
  carry a placeholder; inference must come from behavior only.
- **The live companion doesn't persist timelines yet.** `Persistence` has the
  writers; the UI just needs to call them so real games accumulate data.
- Freeze/Deal3/SecondChance *targeting* choices are not modeled (the simulator
  targets randomly today).
