# Record from the repo root after `dotnet build -c Release`:
#   nix develop --command nix shell nixpkgs#vhs --command vhs media/replay.tape
# The game plays itself out to 200 points; scrubbing to the end shows the winner.
Output media/replay.mp4
Output media/replay.gif

Set Shell "bash"
Set FontSize 20
Set Width 1180
Set Height 766
Set Padding 40
Set Theme "Catppuccin Mocha"

Hide
Type "ui/bin/Release/net10.0/Flip7.UI simulate --player Alice --player Bob --player Carol --player Dana --player Eve --seed 3 --pace 45"
Enter
Sleep 5
Show

Sleep 2

# Scrub forward instant by instant through the opening round
Right@70ms 45
Sleep 1.5

# Jump forward round by round (up steps to the next round's end)
Up@800ms 4
Sleep 1.5

# Keep scrubbing within a later round
Right@70ms 30
Sleep 1.5

# Jump to the end of the game: the final instant names the winner
Up@250ms 14
Sleep 2.5

# Rewind back to the very start
Down@150ms 14
Left@30ms 90
Sleep 2

# Fast scrub through the opening
Right@35ms 60
Sleep 2

Hide
Type "q"
