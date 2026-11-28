---
default: minor
---

# Compile `Lookahead` with Fable

Fable now compiles `Lookahead` to JavaScript as well, where it gives the same
answers to the bit. JavaScript has the one thread, so there the search starts
once the code that made it yields and runs to the end without yielding.
`Persistence` stays .NET only.

The JavaScript build needs a Fable release with
[fable-compiler/Fable#5172](https://github.com/fable-compiler/Fable/pull/5172)
(`Array.BinarySearch`) and
[fable-compiler/Fable#5199](https://github.com/fable-compiler/Fable/pull/5199)
(linked cancellation token sources and `Task.Run`).
