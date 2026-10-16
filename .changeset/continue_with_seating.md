---
default: major
---

# `Timeline.ContinueWith` takes the seating

`ContinueWith` takes every player's name in seat order, after the decider. The
round being continued is scored in its own turn order and the rounds after it
start one seat further round the table each, so a game continued from any
hit-or-stand question with its seating, turn counts and `Random` plays out
exactly as the game it was taken from. It used to guess the seating from the
active and finished players, which went wrong once anyone had finished.
