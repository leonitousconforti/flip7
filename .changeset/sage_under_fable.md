---
default: minor
---

# Compile `Sage` and `Oracle` under Fable

`Sage`, `Oracle` and the modules behind them now compile under Fable, so Sage
can play in a browser. In JavaScript the oracle pins the same seeds as on .NET
and Sage plays the same games, a whole game of five in about a second in Node.
To get there, the oracle's walk over seeds keeps its residues as ints rather
than int64s, on .NET too, since an int64 under Fable is a BigInt, and under
Fable a search replays one game at a time.
