---
default: minor
---

# Write floats and draw from `System.Random.Shared` under Fable as on .NET

Under Fable, strategy and targeting strings now write their floats as .NET's
`ToString("R")` does, -0 and exponents included, and the obsolete helpers that
draw from `System.Random.Shared` compile too. Compiling the package with Fable
needs Fable 5.27.0 or later.
