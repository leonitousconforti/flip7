# Flip7

In this press your luck game, be the first player to score 200 points to win.
You score points based on the total number value of the cards in front of you.
The more valuable a card is, the more copies of that card there are in the deck.
If you can successfully Flip 7 unique Number cards into your line, you
automatically end the round for everyone and score 15 bonus points. However, if
you ever draw a second card with the same number as one already in your line,
you bust and are out of the round, scoring nothing.

## Rules

Formalized from the [official rules and
FAQ](https://rules.dized.com/game/dPDRM857TU-BFRF7LzGE0g). The rule book calls
the Deal3 card "Flip Three"; this repo uses Deal3 throughout.

### The deck

94 cards in total:

- 79 Number cards: each number from 1 to 12 appears as many times as its value (twelve 12s, eleven 11s, ... one 1), plus a single 0. The 0 scores nothing but still counts toward a Flip 7.
- 6 Modifier cards: one each of +2, +4, +6, +8, +10, and x2.
- 9 Action cards: three each of Deal3, Freeze, and Second Chance.

### Setup

Shuffle the deck thoroughly and choose a player to be the dealer for the first round. Keep score on paper; play continues until someone reaches 200 points.

### Dealing

In turn order, the dealer deals one card face up to each player, including themselves. If an action card comes up during the deal, pause dealing immediately to resolve it, then continue until everyone has been dealt a card. Because action cards are resolved mid-deal, players will not start the round evenly: some may have a single Number or Modifier card, while others may have no cards or even three or four.

### Playing the round

A player is active while they have not busted and have not stayed (a frozen player counts as having stayed). In turn order, the dealer offers each active player the choice to hit (be dealt another card face up) or stay (bank the round's points and exit the round). A player may only stay if they have at least one card in front of them. Number cards go in a single row, with Modifier and Action cards placed above them.

- **Bust:** if a player receives a Number card whose number already appears in their row, they bust. They are out of the round and score zero for it. Their cards stay on the table, flipped face down, until the round ends.
- Only Number cards can bust you; Modifier and Action cards never do.
- **Flip 7:** if a player ever has seven unique Number cards in their row, the round ends immediately for them and everyone else. No further cards are drawn, and that player scores a +15 bonus. Modifier and Action cards do not count toward the seven.

The round ends when no active players remain (everyone has busted, stayed, or been frozen) or when a player Flips 7.

### Scoring

Busted players score zero for the round. Every other player scores their row in this order:

1. Add the values of your Number cards.
2. If you have the x2 modifier, double that sum.
3. Add your +2/+4/+6/+8/+10 modifier cards.
4. If you Flipped 7, add the 15 point bonus.

A player who exits the round with only Modifier cards still scores them: a +N with no Number cards scores N points, while an x2 with no Number cards scores nothing.

### Starting the next round

Set all cards played during the round aside into a discard pile; do not shuffle them back into the deck. Pass the remaining deck to the left, and that player deals the next round. When the deck runs out, shuffle the discard pile to form a new deck. If the deck runs out mid-round, leave every card in front of the players where it is, even the cards of players who have busted.

### End of the game

The game ends at the end of any round in which at least one player has 200 or more points, and the player with the most points wins. If there is a tie for the most points, everyone (not just the tied players) plays another round to break it.

### Action cards

An action card must be resolved the moment it is flipped, whether during the initial deal or on a hit. Freeze and Deal3 are given by whoever flips them to any active player, including themselves; if the flipper is the only active player left, they must apply the card to themselves. Resolved action cards sit above your Number cards.

#### Freeze

The player who receives Freeze immediately banks all points they have collected this round and is out of the round. If it comes up during the initial deal it must still be used right away and can be given to any player (everyone is active at that point).

#### Second Chance

Second Chance stays with the player who flipped it. If that player would bust, they instead discard the Second Chance together with the duplicate Number card; their turn is done, but they are still in the round. A player can hold at most one: if they flip another, they must give it to an active player of their choice who does not have one, and if no such player exists it is discarded. All unused Second Chance cards are discarded when the round ends.

#### Deal3

The player who receives Deal3 must flip the next three cards from the deck, one at a time. Cards of every type count toward the three. The forced flips stop early only if the player busts or completes a Flip 7 (which, as always, ends the round immediately for everyone). The cards flipped resolve as follows:

- Number and Modifier cards join the row as normal, and a duplicate Number card busts the player (unless a Second Chance saves them).
- A Second Chance is kept, or passed along per the usual rules, and may be used later within the same Deal3 to prevent a bust.
- A Freeze or another Deal3 is set aside until the three flips finish. If the player did not bust, each set-aside card is then given to an active player (themselves included, and forced on themselves if they are the only active player left) and resolved. If the player busted, the set-aside cards are discarded unresolved.

A Deal3 received during the initial deal is resolved immediately: the recipient either flips the three cards themselves or gives the card to another player, who resolves it immediately. Once a Deal3 fully resolves, play continues with the next player clockwise from whoever originally received it.
