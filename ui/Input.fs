module public Input

open System

/// <summary>
/// A single user input: a regular key press, a mouse wheel / trackpad scroll
/// (with the terminal cell it happened over), or a left click.
/// </summary>
type public InputEvent =
    | Key of ConsoleKeyInfo
    | Wheel of Delta: int * Col: int * Row: int
    | Click of Col: int * Row: int

/// Enables xterm mouse reporting in SGR mode. Every terminal worth using
/// (iTerm2, Ghostty, Kitty, Terminal.app) understands these.
let public MouseOn = "\x1b[?1000h\x1b[?1006h"

/// Disables mouse reporting again. MUST be written before exiting or the
/// user's shell is left receiving mouse escape sequences.
let public MouseOff = "\x1b[?1006l\x1b[?1000l"

// One-event pushback buffer so wheel coalescing can stop cleanly when it
// pulls a non-wheel event off the queue
let mutable private pending: InputEvent option = None

// Reads the "button;col;row" + final byte tail of an SGR mouse report
// "\x1b[<button;col;rowM" after the caller consumed "\x1b[<". The final byte
// is 'M' on press/scroll and 'm' on release. Wheel up/down report as buttons
// 64/65.
let private ReadSgrMouseTail () : InputEvent option =
    let buffer = Text.StringBuilder()
    let mutable last = '\000'

    while last <> 'M' && last <> 'm' && buffer.Length < 16 do
        last <- (Console.ReadKey true).KeyChar

        if last <> 'M' && last <> 'm' then
            buffer.Append last |> ignore

    match (string buffer).Split ';' with
    | [| button; col; row |] ->
        match Int32.TryParse button, Int32.TryParse col, Int32.TryParse row with
        | (true, 64), (true, col), (true, row) -> Some(Wheel(-1, col, row))
        | (true, 65), (true, col), (true, row) -> Some(Wheel(1, col, row))
        | (true, 0), (true, col), (true, row) when last = 'M' -> Some(Click(col, row))
        | _ -> None
    | _ -> None

/// <summary>
/// Blocks for the next input event. Escape sequences that .NET does not
/// translate itself (the SGR mouse reports) arrive through ReadKey one
/// character at a time and are reassembled here.
/// </summary>
let rec public Read () : InputEvent =
    match pending with
    | Some event ->
        pending <- None
        event
    | None ->
        let key = Console.ReadKey true

        // A lone escape is just the escape key; a mouse report always has the
        // rest of its bytes already buffered right behind it
        if key.KeyChar <> '\x1b' || not Console.KeyAvailable then
            Key key
        else

        let second = Console.ReadKey true

        if second.KeyChar <> '[' || not Console.KeyAvailable then
            Key key
        else

        let third = Console.ReadKey true

        if third.KeyChar <> '<' then
            Key key
        else
            match ReadSgrMouseTail() with
            | Some event -> event
            | None -> Read()

/// <summary>
/// Like Read, but merges every wheel event already sitting in the input
/// buffer into one. Trackpads fire dozens of scroll events per second with
/// momentum; without coalescing the render falls behind the gesture.
/// </summary>
let public ReadCoalesced () : InputEvent =
    match Read() with
    | Wheel(delta, col, row) ->
        let mutable total = delta
        let mutable lastCol = col
        let mutable lastRow = row
        let mutable germane = true

        while germane && Console.KeyAvailable do
            match Read() with
            | Wheel(delta, col, row) ->
                total <- total + delta
                lastCol <- col
                lastRow <- row
            | other ->
                pending <- Some other
                germane <- false

        Wheel(total, lastCol, lastRow)
    | event -> event
