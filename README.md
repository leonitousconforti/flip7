# Flip7

In this press your luck game, be the first player to score 200 points to win.
You score points based on the total number value of the cards in front of you.
The more valuable a card is, the more copies of that card there are in the deck.
If you can successfully Flip 7 unique Number cards into your line, you
automatically end the round for everyone and score 15 bonus points. However, if
you ever draw a second card with the same number as one already in your line,
you bust and are out of the round, scoring nothing.

The terminal app has three modes: an interactive game you play against the AIs,
a simulator that plays out a full game with scripted strategies, and a replay
viewer for scrubbing a recorded game. Each one is best suited with a 80x24
terminal.

## Play

![Playing a hand: the game stops at your turn, then the editor's deck
distributions and card entry, a bust and undo, then hitting and
standing](media/play.gif)

Run `dotnet run --project ui -- play You "A Friend"` to take a seat; AIs fill
the rest. The dealer runs and the game stops at each of your turns, showing your
points in hand, your bust probability, and the expected value of hitting. Press
`h` to hit, `s` to stand, or `e` to open the editor and reconcile the table:
move the cursor along the deck's pdf/cdf sparklines with the arrow keys to read
the exact probability of drawing any card, use `+`/`-` to move a copy between
the deck and the discards, and rotate onto a player to deal or take back their
cards (`0`-`9`, `x`/`e`/`t` for 10/11/12, shift for the modifier cards, and
`s`/`d`/`f` for Second Chance, Deal3, and Freeze); `?` opens the full key
reference. Pass one to five names to seat that many humans, and `--seed` with
the same names in the same order to be dealt the same lineup and first cards
again; without it a seed is drawn, and printed when you quit or the game ends.
Games are written under `timelines/`, so they can be replayed later.

The first open seat goes to Sage, so a table of five humans has none, and the
rest to bots drawn from ten that each always play the same strategy under the
same name, so no human may take a bot's name or Sage's, regardless of case.
Before the game Sage studies every game played under `timelines/`, and the
current one as it goes, to model by name how everyone plays and whom they aim
their action cards at, so it learns each bot as well as each of you. It skips
simulated games, which seat no human, and any game that seats a bot's name on a
strategy other than that bot's, as games played before each bot kept its name
do. It works out the game's seed from the cards it sees, usually within the
first round, and from then on plans its play knowing the cards to come: it
replays the rest of the game with everyone else playing as it modelled them,
plays the line that went best for it, and plans again whenever the game strays
from that line or you edit the table. Until it has the seed it plays for
expected value. When you quit or the game ends, it tells you what it made of
how you play and whom you aim at. Dealt a game again, Sage may play it
differently, having studied more games since, the first deal among them, and
once anyone plays differently the cards that follow differ too.

## Simulate and replay

![Timeline replay: scrubbing instant by instant, jumping between rounds, and the
game-over caption at the end](media/replay.gif)

Run `dotnet run --project ui -- simulate --player Alice --player Bob --player
Chloe --player Dave --player Eve` to play a full game to 200 points (each player
is given a strategy and a targeting policy, optionally written as `--player
"Alice,HitUntilScore 25,PlaysSpitefully"`) and scrub through it as it unfolds.
The left and right arrows step instant by instant, up and down jump between
rounds, Home and End snap to the ends, and the final instant names the winner.
Simulated games are written under `timelines/`, so `dotnet run --project ui --
replay <directory>` reopens one later.
