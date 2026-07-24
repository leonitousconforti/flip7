[<AutoOpen>]
module public Render

open Flip7

/// How scared a player should be of busting.
let public bustEmoji (probabilityToBust: float) : string =
    if probabilityToBust >= 50.0 then "😵"
    elif probabilityToBust >= 45.0 then "😵‍💫"
    elif probabilityToBust >= 40.0 then "🫪"
    elif probabilityToBust >= 35.0 then "🫣"
    elif probabilityToBust >= 30.0 then "😱"
    elif probabilityToBust >= 25.0 then "😰"
    elif probabilityToBust >= 20.0 then "😬"
    elif probabilityToBust >= 15.0 then "😐"
    elif probabilityToBust >= 10.0 then "🤔"
    elif probabilityToBust >= 5.0 then "🙂"
    else "😎"

/// Compact alternative to Card.ToString for the fixed-width card boxes: the
/// full action card names are too wide and would break the box layout.
let public cardLabel (card: Card) : string =
    match card with
    | ActionCard Card.Deal3 -> "D3"
    | ActionCard Card.Freeze -> "FZ"
    | ActionCard Card.SecondChance -> "SC"
    | _ -> card.ToString()

/// Renders a hand as three rows of card boxes, with the preamble padded to a
/// fixed width on the middle row so the boxes of all players line up.
let public handRows (padTo: int) (preamble: string) (hand: Hand) : string * string * string =
    hand
    |> List.fold
        (fun (topRow, midRow, botRow) card ->
            let c = (cardLabel card).PadRight(2).PadLeft(3)
            topRow + "┌───┐", midRow + $"│{c}│", botRow + "└───┘"
        )
        (String.replicate padTo " ", preamble.PadRight padTo, String.replicate padTo " ")
