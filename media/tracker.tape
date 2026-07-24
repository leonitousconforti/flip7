# Record from the repo root after `dotnet build -c Release`:
#   nix develop --command nix shell nixpkgs#vhs --command vhs media/tracker.tape
Output media/tracker.mp4
Output media/tracker.gif

Set Shell "bash"
Set FontSize 20
Set Width 1180
Set Height 766
Set Padding 40
Set Theme "Catppuccin Mocha"

Hide
Type "ui/bin/Release/net10.0/Flip7.UI Alice Bob Carol"
Enter
Sleep 3
Show

Sleep 2

# Walk the cursor along the card distributions (pdf/cdf readouts update)
Right@300ms 8
Sleep 1.5

# Select Alice and deal her a hand
Down
Sleep 1
Type@600ms "37et"
Sleep 2

# Bob draws a couple cards plus a Second Chance
Down
Sleep 1
Type@600ms "58s"
Sleep 2

# Carol draws 10, 9 ... and a duplicate 9: bust
Down
Sleep 1
Type@600ms "x9"
Sleep 1
Type "9"
Sleep 2.5

# Undo the bust with backspace
Backspace
Sleep 2

# Commit the round: firm scores update, hands go to the discard pile
Enter
Sleep 2.5

# Deck stats now reflect the discards
Right@300ms 4
Sleep 2

Hide
Type "q"
