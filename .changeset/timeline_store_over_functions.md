---
default: minor
---

# Open a `TimelineStore` over any storage, and compile `Persistence` under Fable

`TimelineStore`'s primary constructor now takes the two functions it reads a
timeline through: `exists`, whether the instant at an index has been published
yet, and `read`, which reads it. The directory constructor stays and passes the
disk versions. The store outlives functions that throw: an `exists` that throws
is asked again at the next look, and a `read` that throws reports that instant
as an error.

`Persistence` now compiles under Fable, minus the disk functions and the
directory constructor, which stay .NET only.
