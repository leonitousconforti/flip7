// Hunts the real deck for hands where one card of sight says stand and three
// say hit: MaximizesExpectedValue's answer is wrong and Lookahead's is right.
// Every hand of distinct value cards (with and without the x2) is priced
// against a fresh deck missing exactly those cards, and the sharpest
// disagreements are broken down draw by draw to show where the extra worth
// lives.
//
//   dotnet fsi benchmark/disagree.fsx

// Keep in compile order with src/Flip7.fsproj
#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
#load "../src/Lookahead.fs"

open Flip7

let values =
    [
        Card.Zero
        Card.One
        Card.Two
        Card.Three
        Card.Four
        Card.Five
        Card.Six
        Card.Seven
        Card.Eight
        Card.Nine
        Card.Ten
        Card.Eleven
        Card.Twelve
    ]

// Every hand of 2..5 distinct value cards, optionally doubled
let hands = seq {
    let rec choose (count: int) (from: Card.ValueCard list) : Card.ValueCard list seq = seq {
        match count, from with
        | 0, _ -> yield []
        | _, [] -> ()
        | count, head :: tail ->
            for rest in choose (count - 1) tail do
                yield head :: rest

            yield! choose count tail
    }

    for size in 2..5 do
        for combination in choose size values do
            let hand = combination |> List.map ValueCard
            yield hand
            yield ModifierCard Card.Double :: hand
}

let disagreements =
    hands
    |> Seq.choose (fun hand ->
        let deck = hand |> List.fold Deck.Decrement Deck.Full
        let oneCard = Simulation.expectedValueOfHit deck Deck.Empty hand
        let threeCards = Lookahead.GainFromHitting 3 deck hand

        if oneCard <= 0.0 && threeCards > 0.0 then
            Some(hand, oneCard, threeCards)
        else
            None
    )
    |> Seq.sortByDescending (fun (_, _, threeCards) -> threeCards)
    |> Seq.toList

printfn $"{List.length disagreements} hands where one card of sight stands and three hit\n"

for hand, oneCard, threeCards in disagreements |> List.truncate 12 do
    let shown = hand |> List.map string |> String.concat " "
    printfn $"  [{shown}] = {Hand.Score hand} points: one card %+.2f{oneCard}, three cards %+.2f{threeCards}"

// The sharpest disagreement, opened up draw by draw: for every card that
// could come, its chance, the hand it leaves, and what that hand is worth
// banked against played on (two cards of sight left)
match disagreements with
| [] -> printfn "no disagreements found"
| (hand, oneCard, threeCards) :: _ ->
    let deck = hand |> List.fold Deck.Decrement Deck.Full
    let shown = hand |> List.map string |> String.concat " "
    let banked = float (Hand.Score hand)

    printfn $"\nthe sharpest: [{shown}], {Hand.Score hand} points banked"
    printfn $"  one card of sight:    %+.3f{oneCard} (stand)"
    printfn $"  three cards of sight: %+.3f{threeCards} (hit)\n"

    let total = deck |> Map.values |> Seq.sumBy float

    deck
    |> Map.toList
    |> List.filter (fun (_, count) -> count > 0u)
    |> List.map (fun (card, count) ->
        let chance = float count / total
        let isBust, reduced, _ = Hand.Reduce(card :: hand)

        let bankedThere = if isBust then 0.0 else float (Hand.Score reduced)
        let playedOn = if isBust then 0.0 else Lookahead.Worth 2 (Deck.Decrement deck card) reduced

        card, chance, isBust, bankedThere, playedOn
    )
    |> List.sortByDescending (fun (_, chance, _, _, playedOn) -> playedOn * chance)
    |> List.iter (fun (card, chance, isBust, bankedThere, playedOn) ->
        let outcome =
            if isBust then
                "bust"
            elif playedOn > bankedThere then
                $"bank %.0f{bankedThere}, or %.2f{playedOn} played on"
            else
                $"bank %.0f{bankedThere}"

        printfn $"  draw %-12s{string card} chance %.3f{chance}: {outcome}"
    )

    let mev =
        deck
        |> Map.toList
        |> List.sumBy (fun (card, count) ->
            let chance = float count / total
            let isBust, reduced, _ = Hand.Reduce(card :: hand)

            match card with
            | ActionCard _ -> chance * banked
            | _ -> if isBust then 0.0 else chance * float (Hand.Score reduced)
        )

    let lookahead =
        deck
        |> Map.toList
        |> List.sumBy (fun (card, count) ->
            let chance = float count / total
            let isBust, reduced, _ = Hand.Reduce(card :: hand)

            if isBust then
                0.0
            else
                chance * Lookahead.Worth 2 (Deck.Decrement deck card) reduced
        )

    printfn $"\n  hit and bank whatever comes: %.3f{mev} vs {banked} banked now"
    printfn $"  hit and keep choosing:       %.3f{lookahead} vs {banked} banked now"
