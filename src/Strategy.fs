namespace Flip7

type public HitOrStand =
    | Hit
    | Stand

type public Strategy = uint -> Hand -> Hand list -> Deck -> HitOrStand

module Strategy =
    let public AlwaysHits: Strategy = fun _session _hand _otherHands _deck -> Hit
    let public AlwaysStands: Strategy = fun _session _hand _otherHands _deck -> Stand
    let public Random: Strategy =
        fun _session _hand _otherHands _deck -> if System.Random().NextSingle() > 0.5f then Hit else Stand
    let public HitUntil: uint -> Strategy =
        fun threshold _session hand _otherHands _deck -> if Hand.Score hand < threshold then Hit else Stand

    let rec public Prompt: Strategy =
        fun _session hand _otherHands _deck ->
            printfn "Your hand: %A (score: %d)" hand (Hand.Score hand)
            printfn "Do you want to hit or stand? (hit/stand)"
            let input = System.Console.ReadLine()
            match input with
            | "h" -> Hit
            | "hit" -> Hit
            | "s" -> Stand
            | "stand" -> Stand
            | _ ->
                printfn "Invalid input, please enter 'h' for hit or 's' for stand."
                Prompt _session hand _otherHands _deck
