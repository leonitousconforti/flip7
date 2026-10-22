---
default: minor
---

# Compile `Lookahead` with Fable

Fable now compiles `Lookahead` to JavaScript as well, where it gives the same
answers to the bit. JavaScript has the one thread, so there the search starts
on the event loop's next turn and runs to the end without yielding; its doc
comment says how cancellation differs. `Persistence` stays .NET only.

On .NET the search runs about a tenth faster, finding states with a binary
search of its own rather than `Array.BinarySearch`.
