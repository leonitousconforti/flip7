---
default: major
---

# Observe both second chances a deal3 passes on, and make the scan's turn and round internal

`Observation.Observe` now gives the aims of both second chances a deal3's
target passes on during its flips, read from the deal3's `Passed`, where it
used to give neither. A single pass is read from `Passed` too, instead of from
who holds a second chance after the deal3, so a deal3 saved by an earlier
version, which records no passes, gives no aim for a second chance passed
during its flips.

`Observation.TurnsTaken` and `Observation.RoundOf` are now internal.
