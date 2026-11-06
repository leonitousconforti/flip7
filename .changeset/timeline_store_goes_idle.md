---
default: patch
---

# A complete `TimelineStore` with no ingest delay goes idle

A `TimelineStore` with no ingest delay, such as the one the terminal app opens
at `--pace 0`, no longer keeps a core busy once the game it tails is complete.
It used to keep waking as fast as it could for as long as it was open, with
nothing left to ingest; once it has ingested the instant that ends the game it
now waits, and only the viewer wakes it. While the game is still unfinished it
looks for the next instant as fast as it can, as before. A store whose ingest
delay is longer than `Int32.MaxValue` milliseconds no longer keeps a core busy
either, and disposing a store as it starts a read no longer raises
`ObjectDisposedException` in its agent.
