---
default: major
---

# Record whom a deal3's second chances are passed on to

`Event.Dealt3` has a fourth field, `Passed`: whom each second chance the target
flipped while already holding one was passed on to, in the order they were
passed. A second chance nobody could take is discarded and not listed. The
engine fills it in, so a timeline now shows which of two second chances passed
during one deal3 went first, which decides who could take the second.

A deal3's caption ends with whom its second chances went to, as in "passing
SecondChance to Carol then Dave". A saved deal3 lists its cards as before, then
a `passed: <name>` line for each second chance passed on. A deal3 saved by an
earlier version reads back as passing none on.
