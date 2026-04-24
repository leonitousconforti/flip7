module Program

open Flip7

[<EntryPoint>]
let main args =
    let deck, discards = Deck.Full, Deck.Empty

    let players: Simulation.Player list = [
        ("Alice", Strategy.Random, [ Card.ValueCard Card.Ten; Card.ModifierCard Card.Plus4 ])
        ("Bob", Strategy.Random, [ Card.ValueCard Card.Nine; Card.ValueCard Card.Three ])
        ("Charlie", Strategy.Random, [ Card.ValueCard Card.Eight; Card.ValueCard Card.Four ])
        ("Dave", Strategy.Random, [ Card.ValueCard Card.Seven; Card.ModifierCard Card.Plus10 ])
        ("Ethan", Strategy.Random, [ Card.ValueCard Card.Six; Card.ModifierCard Card.Double ])
    ]

    printfn "%s" (String.replicate 80 "─")
    printfn "ec:     %s" (deck |> Deck.ec |> string)
    printfn "ev:     %.2f" (deck |> Deck.ev)
    printfn "var:    %.2f" (deck |> Deck.var)
    printfn "std:    %.2f" (deck |> Deck.std)
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
            (fun (lastTop, lastMiddle, lastBottom) card ->
                let label = card.ToString().PadRight(2).PadLeft(3)
                let newTop = lastTop + $"┌───┐"
                let newMiddle = lastMiddle + $"│{label}│"
                let newBottom = lastBottom + $"└───┘"
                newTop, newMiddle, newBottom
            )
            (String.replicate 40 " ", preamble.PadRight 40, String.replicate 40 " ")
        |> fun (top, middle, bottom) -> [ top; middle; bottom ]
        |> String.concat "\n"
        |> printfn "%s"

    0
