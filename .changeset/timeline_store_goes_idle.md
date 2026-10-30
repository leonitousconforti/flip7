---
default: patch
---

# A complete `TimelineStore` with no ingest delay goes idle

A `TimelineStore` with no ingest delay, such as the one the terminal app opens
at `--pace 0`, no longer keeps a core busy once the game it tails is complete.
It used to keep waking as fast as it could for as long as it was open, with
nothing left to ingest; once it has ingested the instant that ends the game it
now waits, and only the viewer wakes it. While the game is still unfinished it
looks for the next instant as fast as it can, as before.

With an ingest delay, the store now folds in each instant as soon as it has
read it, rather than when it next looks, and waits the delay after that before
it looks for the next one, so instants arrive at the delay plus the time a read
takes rather than exactly at the delay.
