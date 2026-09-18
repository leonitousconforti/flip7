// Hunts the real deck for hands where one card of sight says stand and more
// say hit: MaximizesExpectedValue's answer is wrong and Lookahead's is right.
// Every hand of distinct value cards (with and without the x2) is priced
// against a fresh deck missing exactly those cards, and the sharpest
// disagreements are broken down draw by draw to show where the extra worth
// lives.
//
//   dotnet fsi benchmark/disagree.fsx [depth]
//
// Beware the depth: the search has no memoization, so its cost multiplies by
// the near-twenty distinct cards of the deck for every extra card of sight.
// The default three prices every hand in seconds; seven does not finish even
// a single two-card hand in ten minutes.

// Keep in compile order with src/Flip7.fsproj
#load "../src/ScoreBuckets.fs"
#load "../src/Card.fs"
#load "../src/Hand.fs"
#load "../src/Deck.fs"
#load "../src/Simulation.fs"
#load "../src/Lookahead.fs"

open Flip7

let depth =
    fsi.CommandLineArgs
    |> Array.tryItem 1
    |> Option.map int
    |> Option.defaultValue 3

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

// Progress on stderr, so redirected stdout stays clean: the bar remembers how
// long the priced hands took and guesses the rest from it. The guess starts
// pessimistic, because the two-card hands come first and carry the deepest
// live trees
let all = hands |> Seq.toArray
let watch = System.Diagnostics.Stopwatch.StartNew()

let progress (finished: int) =
    let width = 40
    let filled = finished * width / all.Length
    let bar = String.replicate filled "#" + String.replicate (width - filled) "-"
    let elapsed = watch.Elapsed.TotalSeconds

    let remaining =
        if finished = 0 then
            ""
        else
            $", ~%.0f{elapsed / float finished * float (all.Length - finished)}s left"

    eprintf $"\r  [{bar}] {finished}/{all.Length} hands, %.0f{elapsed}s{remaining}"

    if finished = all.Length then
        eprintfn ""

let disagreements =
    all
    |> Array.mapi (fun index hand ->
        progress index
        let deck = hand |> List.fold Deck.Decrement Deck.Full
        let oneCard = Simulation.expectedValueOfHit deck Deck.Empty hand
        let deeper = Lookahead.GainFromHitting depth deck hand

        if oneCard <= 0.0 && deeper > 0.0 then
            Some(hand, oneCard, deeper)
        else
            None
    )
    |> Array.choose id
    |> Array.sortByDescending (fun (_, _, deeper) -> deeper)
    |> Array.toList

progress all.Length

printfn $"{List.length disagreements} hands where one card of sight stands and {depth} hit\n"

for hand, oneCard, deeper in disagreements |> List.truncate 12 do
    let shown = hand |> List.map string |> String.concat " "
    printfn $"  [{shown}] = {Hand.Score hand} points: one card %+.2f{oneCard}, {depth} cards %+.2f{deeper}"

// The sharpest disagreement, opened up draw by draw: for every card that
// could come, its chance, the hand it leaves, and what that hand is worth
// banked against played on (one card of sight spent)
match disagreements with
| [] -> printfn "no disagreements found"
| (hand, oneCard, deeper) :: _ ->
    let deck = hand |> List.fold Deck.Decrement Deck.Full
    let shown = hand |> List.map string |> String.concat " "
    let banked = float (Hand.Score hand)

    printfn $"\nthe sharpest: [{shown}], {Hand.Score hand} points banked"
    printfn $"  one card of sight:  %+.3f{oneCard} (stand)"
    printfn $"  {depth} cards of sight: %+.3f{deeper} (hit)\n"

    let total = deck |> Map.values |> Seq.sumBy float

    deck
    |> Map.toList
    |> List.filter (fun (_, count) -> count > 0u)
    |> List.map (fun (card, count) ->
        let chance = float count / total
        let isBust, reduced, _ = Hand.Reduce(card :: hand)

        let bankedThere = if isBust then 0.0 else float (Hand.Score reduced)
        let playedOn = if isBust then 0.0 else Lookahead.Worth (depth - 1) (Deck.Decrement deck card) reduced

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
                chance * Lookahead.Worth (depth - 1) (Deck.Decrement deck card) reduced
        )

    printfn $"\n  hit and bank whatever comes: %.3f{mev} vs {banked} banked now"
    printfn $"  hit and keep choosing:       %.3f{lookahead} vs {banked} banked now"
