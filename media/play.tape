# Record from the repo root after `dotnet build -c Release`:
#   nix develop --command nix shell nixpkgs#vhs --command vhs media/play.tape
# You take one seat; AIs fill the rest and the game blocks at your turn.
Output media/play.mp4
Output media/play.gif

Set Shell "bash"
Set FontSize 20
Set Width 1180
Set Height 766
Set Padding 40
Set Theme "Catppuccin Mocha"

Hide
Type "ui/bin/Release/net10.0/Flip7.UI play You --seed 5 --pace 350"
Enter
Sleep 5
Show

# The deal runs, the AIs play, and the game stops for your first decision
Sleep 2

# Open the editor to reconcile the table before deciding
Type "e"
Sleep 1.3

# Walk the cursor along the deck distributions (the pdf/cdf readouts update)
Right@300ms 6
Sleep 1.3

# Rotate up onto your own hand and deal yourself a couple of cards
Up
Sleep 0.8
Type@450ms "35"
Sleep 1.3

# A duplicate would bust you: the row turns red and blocks resuming
Type "5"
Sleep 1.8
# Backspace takes the duplicate back
Backspace
Sleep 1.3

# The full key reference
Type "?"
Sleep 3
Space
Sleep 0.8

# Take the two cards back to leave the hand as it was, then cancel out
Backspace
Sleep 0.3
Backspace
Sleep 0.6
Escape
Sleep 1.5

# Make the real decision: hit once, then stand to bank the round
Type "h"
Sleep 2
Type "s"
Sleep 2.5

# The AIs finish the round and deal the next; the game blocks at your turn again
Sleep 5

Hide
Type "q"
