module Program

open Flip7

let private blockChar f =
    if f >= 7.0 / 8.0 then "█"
    elif f >= 6.0 / 8.0 then "▇"
    elif f >= 5.0 / 8.0 then "▆"
    elif f >= 4.0 / 8.0 then "▅"
    elif f >= 3.0 / 8.0 then "▄"
    elif f >= 2.0 / 8.0 then "▃"
    elif f >= 1.0 / 8.0 then "▂"
    elif f >= 0.0 / 8.0 then "▁"
    else " "

let private barCell (totalRows: int) (h: float) (r: int) =
    let scaled = h * float totalRows
    if scaled >= float r + 1.0 then "█"
    elif scaled > float r then blockChar (scaled - float r)
    else " "

let private sparkline (rows: int) (values: float list) : string list =
    List.map (fun r -> values |> List.map (fun v -> barCell rows v r + " ") |> String.concat "") [ rows - 1 .. -1 .. 0 ]

let printStats (deck: Deck) =
    let pdf3 =
        deck
        |> Deck.pdf
        |> Map.filter (fun card _ -> card.IsValueCard)
        |> Map.toList
        |> List.map snd
        |> sparkline 3

    let cdf3 =
        deck
        |> Deck.cdf
        |> Map.filter (fun card _ -> card.IsValueCard)
        |> Map.toList
        |> List.map snd
        |> sparkline 3

    let gap = String.replicate 4 " "
    let axisRow =
        [ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12" ]
        |> List.map (fun l -> l.PadRight 2)
        |> String.concat ""

    let ecl = (sprintf "ec:     %s" (Deck.ec deck |> string)).PadRight 20
    let evl = (sprintf "ev:     %.2f" (Deck.ev deck)).PadRight 20
    let var = (sprintf "var:    %.2f" (Deck.var deck)).PadRight 20
    let std = (sprintf "std:    %.2f" (Deck.std deck)).PadRight 20

    printfn "%s" (evl + pdf3[0] + gap + cdf3[0])
    printfn "%s" (ecl + pdf3[1] + gap + cdf3[1])
    printfn "%s" (var + pdf3[2] + gap + cdf3[2])
    printfn "%s" (std + axisRow + gap + axisRow)

[<EntryPoint>]
let main args =
    let deck = Deck.Random
    let discards = Deck.Empty

    let players: Simulation.Player list = [
        ("Alice", Strategy.Random, [ Card.ValueCard Card.Ten; Card.ModifierCard Card.Plus4 ])
        ("Bob", Strategy.Random, [ Card.ValueCard Card.Nine; Card.ValueCard Card.Three ])
        ("Charlie", Strategy.Random, [ Card.ValueCard Card.Eight; Card.ValueCard Card.Four ])
        ("Dave", Strategy.Random, [ Card.ValueCard Card.Seven; Card.ModifierCard Card.Plus10 ])
        ("Ethan", Strategy.Random, [ Card.ValueCard Card.Six; Card.ModifierCard Card.Double ])
    ]

    printfn "%s" (String.replicate 80 "─")
    printfn ""
    printStats deck
    printfn ""
    printfn "%s" (String.replicate 80 "─")

    for playerName, strategy, hand in players do
        let currentScore = 0
        let emojiStatus = ""
        let tentativeScore = Hand.Score hand
        let probabilityToBust = 0.0f
        let preamble =
            sprintf
                "%s %s (%dpts + %dpts?, %.2f%%): "
                playerName
                emojiStatus
                currentScore
                tentativeScore
                probabilityToBust

        hand
        |> List.fold
            (fun (lastTopRow, lastMidRow, lastBotRow) card ->
                let c = card.ToString().PadRight(2).PadLeft(3)
                let newTopRow = lastTopRow + $"┌───┐"
                let newMidRow = lastMidRow + $"│{c}│"
                let newBotRow = lastBotRow + $"└───┘"
                newTopRow, newMidRow, newBotRow
            )
            (String.replicate 40 " ", preamble.PadRight 40, String.replicate 40 " ")
        |> fun (top, middle, bottom) -> [ top; middle; bottom ]
        |> String.concat "\n"
        |> printfn "%s"

    0
