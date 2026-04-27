[<AutoOpen>]
module public Ansi

let public CursorHide = "\x1b[?25l"
let public CursorShow = "\x1b[?25h"
let public ClearScreen = "\x1b[2J\x1b[H"

// Control codes
let public Reset = "\x1b[0m"
let public Bright = "\x1b[1m"
let public Dim = "\x1b[2m"
let public Italic = "\x1b[3m"
let public Underline = "\x1b[4m"
let public SlowBlink = "\x1b[5m"
let public RapidBlink = "\x1b[6m"
let public Inverse = "\x1b[7m"
let public Conceal = "\x1b[8m"
let public StrikeThrough = "\x1b[9m"

// Foreground colors
let public Black = "\x1b[30m"
let public Red = "\x1b[31m"
let public Green = "\x1b[32m"
let public Yellow = "\x1b[33m"
let public Blue = "\x1b[34m"
let public Magenta = "\x1b[35m"
let public Cyan = "\x1b[36m"
let public White = "\x1b[37m"

// Bright foreground colors
let public BrightBlack = "\x1b[90m"
let public BrightRed = "\x1b[91m"
let public BrightGreen = "\x1b[92m"
let public BrightYellow = "\x1b[93m"
let public BrightBlue = "\x1b[94m"
let public BrightMagenta = "\x1b[95m"
let public BrightCyan = "\x1b[96m"
let public BrightWhite = "\x1b[97m"

// Background colors
let public BgBlack = "\x1b[40m"
let public BgRed = "\x1b[41m"
let public BgGreen = "\x1b[42m"
let public BgYellow = "\x1b[43m"
let public BgBlue = "\x1b[44m"
let public BgMagenta = "\x1b[45m"
let public BgCyan = "\x1b[46m"
let public BgWhite = "\x1b[47m"

let public styled (styles: string list) (text: string) : string =
    let styleSeq = String.concat "" styles
    $"{styleSeq}{text}{Reset}"

let private ansiRegex =
    System.Text.RegularExpressions.Regex("\u001B\[[0-9;]*m", System.Text.RegularExpressions.RegexOptions.Compiled)

let public stripAnsi (text: string) : string = ansiRegex.Replace(text, "")

let public visualLength (text: string) : int = stripAnsi text |> String.length

let public centered (width: int) (s: string) : string =
    let visLength = visualLength s
    if visLength >= width then
        s
    else

    let left = max 0 ((width - visLength) / 2)
    let right = max 0 (width - visLength - left)
    let paddingLeft = String.replicate left " "
    let paddingRight = String.replicate right " "
    paddingLeft + s + paddingRight
