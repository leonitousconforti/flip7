---
default: patch
---

# Deal and write the same in JavaScript as on .NET

A seeded game compiled with Fable now deals the cards it deals on .NET, and
strategy and targeting strings are written the same on both. On .NET every seed
deals what it always has, and drawing a card is faster.

Fixes:

- A draw makes the calls on the `System.Random` that FSharp.Core's
  `Array.randomShuffleWith` made and takes the card it put first, rather than
  calling it. Fable shuffles another way, so in JavaScript a seed was dealt
  other cards from the first draw on.
- A spiteful or greedy player passes a second chance to the first of the
  players tied furthest behind in JavaScript too, where it went to the last of
  them, and `Inference` reads such an aim the same way.
- A float in a strategy or targeting string is written in JavaScript as .NET
  writes it, `-0`, `1.5E-05` and `1E+21` included, where it was written `0`,
  `0.000015` and `1e+21`. Both platforms still read either form.
- `Deck.Random`, `Deck.Draw1`, `Deck.Draw3` and `Timeline.Simulate` compile
  with Fable too. On both platforms they draw from a new unseeded
  `System.Random` rather than `System.Random.Shared`, and are still obsolete. A
  `Timeline.Simulate` timeline keeps its one `System.Random`, so enumerate it
  from one consumer at a time.
