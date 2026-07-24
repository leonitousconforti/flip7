# Record from the repo root after `dotnet build -c Release`:
#   nix develop --command nix shell nixpkgs#vhs --command vhs media/replay.tape
Output media/replay.mp4
Output media/replay.gif

Set Shell "bash"
Set FontSize 20
Set Width 1180
Set Height 766
Set Padding 40
Set Theme "Catppuccin Mocha"

Hide
Type "ui/bin/Release/net10.0/Flip7.UI --simulate Alice Bob Carol Dana Eve"
Enter
Sleep 4
Show

Sleep 2

# Scrub forward instant by instant
Right@70ms 45
Sleep 1.5

# Jump forward round by round
Down@800ms 4
Sleep 1.5

# Keep scrubbing within a later round
Right@70ms 30
Sleep 1.5

# Flip round by round to the end of the game
Down@120ms 40
Sleep 2

# Jump back a few rounds
Up@800ms 3
Sleep 1.5

# Back to the start
Up@120ms 40
Sleep 2

# Fast scrub through the opening
Right@35ms 60
Sleep 2

Hide
Type "q"
