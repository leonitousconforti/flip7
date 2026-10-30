---
default: patch
---

# Deal modifier cards from the play editor on macOS and Linux

The play editor deals the x2, +2, +4, +6 and +8 modifier cards for `!`, `@`,
`$`, `^` and `*`, and its help lists those characters. On macOS and Linux the
console reports them without the shift or the digit, so shift+1 to shift+8
dealt nothing there. On Windows, `+` and `-` now work from the main keyboard as
well as the keypad.
