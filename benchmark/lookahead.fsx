// Hunts the real deck for hands where one card of sight says stand and more say
// hit: MaximizesExpectedValue's answer is wrong and Lookahead's is right. Every
// legal hand of one to six cards is priced against a fresh deck missing exactly
// those cards, and the sharpest disagreements are broken down draw by draw to
// show where the extra worth lives.
//
//   dotnet fsi benchmark/lookahead.fsx [depth]
//
// The pricing lives in Lookahead: one searcher prices every hand from the
// moment it is constructed, and this script only asks it questions and prints
// what it hears.

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

// Every legal hand of 1..6 cards: a dup and the second chance covering it
// cancel the moment they meet, so a lasting hand holds each value at most once,
// each modifier at most once, and up to the deck's three of each action
let hands = seq {
    let limits =
        Deck.Full
        |> Map.toList
        |> List.map (fun (card, count) -> card, if card.IsValueCard then 1 else int count)

    let rec choose (size: int) (from: (Card * int) list) : Hand seq = seq {
        match size, from with
        | 0, _ -> yield []
        | _, [] -> ()
        | size, (card, limit) :: rest ->
            for copies in min size limit .. -1 .. 0 do
                for tail in choose (size - copies) rest do
                    yield List.replicate copies card @ tail
    }

    for size in 1..6 do
        yield! choose size limits
}

let all = hands |> Seq.toArray

let canceller = new System.Threading.CancellationTokenSource()
System.Console.CancelKeyPress.Add(fun press ->
    press.Cancel <- true
    canceller.Cancel()
)

// Progress on stderr, so redirected stdout stays clean
let searcher =
    new Lookahead(depth, all, progress = (fun line -> eprintfn $"  {line}"), cancellation = canceller.Token)

// Every hand here was given to the searcher, so an Error is a bug
let priced (result: Result<float, LookaheadError>) : float =
    match result with
    | Ok worth -> worth
    | Error error -> failwith $"%A{error}"

let gains =
    try
        all
        |> Array.map (fun hand -> async {
            let! gain = searcher.GainFromHitting hand
            return priced gain
        })
        |> Async.Parallel
        |> fun work -> Async.RunSynchronously(work, cancellationToken = canceller.Token)
    with _ when canceller.IsCancellationRequested ->
        eprintfn "  cancelled"
        exit 130

let disagreements =
    Array.zip all gains
    |> Array.Parallel.map (fun (hand, deeper) ->
        let deck = hand |> List.fold Deck.Decrement Deck.Full
        let oneCard = Simulation.expectedValueOfHit deck Deck.Empty hand

        if oneCard <= 0.0 && deeper > 0.0 then
            Some(hand, oneCard, deeper)
        else
            None
    )
    |> Array.choose id
    |> Array.sortByDescending (fun (_, _, deeper) -> deeper)
    |> Array.toList

printfn $"{List.length disagreements} hands where one card of sight stands and {depth} hit\n"

for hand, oneCard, deeper in disagreements |> List.truncate 12 do
    let shown = hand |> List.map string |> String.concat " "
    printfn $"  [{shown}] = {Hand.Score hand} points: one card %+.2f{oneCard}, {depth} cards %+.2f{deeper}"

// The sharpest disagreement, opened up draw by draw: for every card that could
// come, its chance, the hand it leaves, and what that hand is worth banked
// against played on (one card of sight spent)
match disagreements with
| [] -> printfn "no disagreements found"
| (hand, oneCard, deeper) :: _ ->
    let deck = hand |> List.fold Deck.Decrement Deck.Full
    let shown = hand |> List.map string |> String.concat " "
    let banked = float (Hand.Score hand)

    let worthAfter (card: Card) : float =
        searcher.WorthAfter(hand, card) |> Async.RunSynchronously |> priced

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
        let playedOn = if isBust then 0.0 else worthAfter card

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
            let isBust, _, _ = Hand.Reduce(card :: hand)

            if isBust then 0.0 else chance * worthAfter card
        )

    printfn $"\n  hit and bank whatever comes: %.3f{mev} vs {banked} banked now"
    printfn $"  hit and keep choosing:       %.3f{lookahead} vs {banked} banked now"
