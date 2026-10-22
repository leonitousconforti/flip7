---
default: patch
---

# A complete `TimelineStore` with no ingest delay goes idle

A `TimelineStore` with no ingest delay, such as the one the terminal app opens
at `--pace 0`, no longer keeps a core busy once the game it tails is complete.
It used to go on looking for the next instant as fast as it could for as long
as it was open; it now stops looking once it has ingested the instant that ends
the game, and only the viewer wakes it. While the game is still unfinished it
looks as fast as it can, as before.
