namespace Flip7

open System.Collections.Generic

/// <summary>
/// The ways a question can miss a searcher's table: a hand no deck could ever
/// have dealt, a hand the searcher was not given at construction, or a card the
/// deck has no copies of left to turn up.
/// </summary>
type public LookaheadError =
    | ImpossibleHand of Hand
    | UnpricedHand of Hand
    | NoCopiesLeft of Card

// A state of the search is one word: what the full deck is missing. The missing
// cards name the hand they built - each value held once, the extra copies dups
// that each burned a second chance - and the deck they left behind, so nothing
// else needs storing. Each card's count gets just enough bits for its copies in
// the full deck, fifty bits in all. None of this depends on any particular
// search, only on the full deck itself.
module private Word =
    let cards = Deck.Full |> Map.toArray
    let fullCounts = cards |> Array.map (snd >> int)

    let widths =
        fullCounts
        |> Array.map (fun count ->
            let mutable bits = 0
            let mutable left = count

            while left > 0 do
                bits <- bits + 1
                left <- left / 2

            bits
        )

    let offsets = Array.scan (+) 0 widths |> Array.take cards.Length
    let masks = widths |> Array.map (fun width -> (1UL <<< width) - 1UL)

    do
        if Array.sum widths > 64 then
            failwith "the missing counts no longer fit in one word"

    let indexOf = cards |> Array.mapi (fun index (card, _) -> card, index) |> dict
    let isValue = cards |> Array.map (fun (card, _) -> card.IsValueCard)
    let valuePoints = cards |> Array.map (fun (card, _) -> int card.Value.ValuePoints)
    let modifierPoints =
        cards |> Array.map (fun (card, _) -> int card.Value.ModifierPoints)
    let doubleIndex = indexOf[ModifierCard Card.Double]
    let secondChanceIndex = indexOf[ActionCard Card.SecondChance]
    let deckSize = Array.sum fullCounts

    let missingOf (state: uint64) (index: int) : int =
        int ((state >>> offsets[index]) &&& masks[index])

    let drawn (state: uint64) (index: int) : uint64 = state + (1UL <<< offsets[index])

    // A hand fits a word only when the full deck holds every copy it claims
    let tryEncode (hand: Hand) : uint64 option =
        (Some 0UL, hand)
        ||> List.fold (fun state card ->
            let index = indexOf[card]

            state
            |> Option.filter (fun state -> missingOf state index < fullCounts[index])
            |> Option.map (fun state -> drawn state index)
        )

    let shown (hand: Hand) : string =
        hand |> List.map string |> String.concat " "

    // One pass over a state's word: what it banks (values times the x2,
    // modifiers and the flip7 bonus on top, exactly as Hand.Score counts), the
    // distinct values held, the second chances left after cancelling dups, and
    // how many cards the deck still holds
    let facts (state: uint64) : struct (float * int * int * int) =
        let mutable points = 0
        let mutable modifiers = 0
        let mutable distinct = 0
        let mutable dups = 0
        let mutable missing = 0

        for index in 0 .. cards.Length - 1 do
            let m = missingOf state index
            missing <- missing + m
            modifiers <- modifiers + m * modifierPoints[index]

            if m > 0 && isValue[index] then
                distinct <- distinct + 1
                dups <- dups + m - 1
                points <- points + valuePoints[index]

        let multiplier = if missingOf state doubleIndex > 0 then 2 else 1
        let bonus = if distinct >= 7 then 15 else 0
        let banked = float (points * multiplier + modifiers + bonus)
        let spare = missingOf state secondChanceIndex - dups
        struct (banked, distinct, spare, deckSize - missing)

    // The forward sweep: the states one draw deeper than a generation. A draw
    // is live when the card is new to the hand or a second chance remains to
    // cancel it, and a hand that has flipped seven stops drawing
    let expand (frontier: uint64[]) : uint64[] =
        let next = System.Collections.Concurrent.ConcurrentDictionary<uint64, byte>()

        frontier
        |> Array.Parallel.iter (fun state ->
            let struct (_, distinct, spare, _) = facts state

            if distinct < 7 then
                for index in 0 .. cards.Length - 1 do
                    let m = missingOf state index

                    if m < fullCounts[index] && (spare > 0 || not (isValue[index] && m > 0)) then
                        next.TryAdd(drawn state index, 0uy) |> ignore
        )

        let states = Array.zeroCreate next.Count
        next.Keys.CopyTo(states, 0)
        Array.sortInPlace states
        states

    // What playing on is worth against the next generation's worths: the
    // average over every card that could come, a bust worth nothing
    let afterHitting
        (nextStates: uint64[])
        (nextWorths: float[])
        (state: uint64)
        (spare: int)
        (remaining: int)
        : float =
        let mutable sum = 0.0

        for index in 0 .. cards.Length - 1 do
            let m = missingOf state index
            let count = fullCounts[index] - m

            if count > 0 && (spare > 0 || not (isValue[index] && m > 0)) then
                let child = System.Array.BinarySearch(nextStates, drawn state index)
                sum <- sum + float count / float remaining * nextWorths[child]

        sum

    // What a state is worth held: the better of banking now and playing on. A
    // hand that has flipped seven banks whatever the sight, because the round
    // ends there
    let worthOf (nextStates: uint64[]) (nextWorths: float[]) (state: uint64) : float =
        let struct (banked, distinct, spare, remaining) = facts state

        if distinct >= 7 then
            banked
        else
            max banked (afterHitting nextStates nextWorths state spare remaining)

    let bankedOf (state: uint64) : float =
        let struct (banked, _, _, _) = facts state
        banked

/// <summary>
/// What a hand is worth when it is played on as well as it can be, computed
/// rather than sampled: the deck is a known multiset, so the chance of every
/// card that could come next is known exactly and the average over them can be
/// summed instead of estimated. There is no sampling error in any of this, and
/// nothing here needs a threshold to decide whether it has seen enough.
///
/// A searcher prices every hand it is given at construction, depth cards of
/// sight each, against a fresh deck missing exactly that hand. The work starts
/// on the thread pool the moment the searcher is made and runs bottom up: a
/// forward sweep collects every state reachable within the depth, one
/// generation a draw deeper than the last, and a backward pass prices each
/// generation against the one after it, letting the deeper go as soon as it has
/// served, so no more than two generations of worths are ever held. The members
/// await the table, so they may be called before it is done.
///
/// The searcher answers only for the hands it was given: a state is named by
/// what the full deck is missing, and the missing cards name the hand they
/// built, so a hand it never swept from is a question it never priced.
/// </summary>
type public Lookahead(depth: int, hands: Hand seq, ?progress: string -> unit) =
    do
        if depth < 1 then
            invalidArg (nameof depth) "the search needs at least one card of sight"

    let report = defaultArg progress ignore

    let roots =
        hands
        |> Seq.map (fun hand ->
            if Hand.IsBust hand then
                invalidArg (nameof hands) $"[{Word.shown hand}] is already bust"

            match Word.tryEncode hand with
            | Some state -> state
            | None -> invalidArg (nameof hands) $"[{Word.shown hand}] holds more copies than the full deck"
        )
        |> Seq.toArray

    // The whole table prices once, starting now; everything below awaits it
    let table =
        async {
            let watch = System.Diagnostics.Stopwatch.StartNew()

            // frontiers[k] holds every state reachable in exactly k draws
            let frontiers: uint64[][] = Array.zeroCreate (depth + 1)
            frontiers[0] <- Array.distinct roots

            for level in 1..depth do
                frontiers[level] <- Word.expand frontiers[level - 1]
                report $"sweep {level}/{depth}: {frontiers[level].Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

            // The backward pass: the deepest generation banks - its sight is
            // spent - and each earlier one is priced against the one after it
            let mutable above =
                frontiers[depth], frontiers[depth] |> Array.Parallel.map Word.bankedOf

            report $"price {depth}/{depth}: {frontiers[depth].Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

            for level in depth - 1 .. -1 .. 1 do
                let nextStates, nextWorths = above
                let states = frontiers[level]
                let worths = states |> Array.Parallel.map (Word.worthOf nextStates nextWorths)
                frontiers[level + 1] <- [||]
                above <- states, worths
                report $"price {level}/{depth}: {states.Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

            // The generation one draw in stays alive: the hands are priced
            // against it, and the draw-by-draw questions read from it too
            let childStates, childWorths = above
            let gains = Dictionary<uint64, float>()

            for state in roots do
                let struct (banked, distinct, spare, remaining) = Word.facts state

                gains[state] <-
                    if distinct >= 7 then
                        0.0
                    else
                        Word.afterHitting childStates childWorths state spare remaining - banked

            return gains, childStates, childWorths
        }
        |> Async.StartAsTask

    /// <summary>
    /// What hitting is worth over standing, in points of expected round score,
    /// for one of the hands given at construction. Positive says hit. Errors
    /// when the hand is not one the searcher priced.
    /// </summary>
    member _.GainFromHitting(hand: Hand) : Async<Result<float, LookaheadError>> = async {
        let! gains, _, _ = Async.AwaitTask table

        match Word.tryEncode hand with
        | None -> return Error(ImpossibleHand hand)
        | Some state ->
            match gains.TryGetValue state with
            | true, gain -> return Ok gain
            | false, _ -> return Error(UnpricedHand hand)
    }

    /// <summary>
    /// What the hand a draw leaves is worth with the sight that remains: zero
    /// when the draw busts, and the round already over when it flips seven.
    /// Errors when the hand is not one the searcher priced, or when the deck
    /// has no copy of the card left to turn up.
    /// </summary>
    member _.WorthAfter(hand: Hand, card: Card) : Async<Result<float, LookaheadError>> = async {
        let! gains, childStates, childWorths = Async.AwaitTask table

        match Word.tryEncode hand with
        | None -> return Error(ImpossibleHand hand)
        | Some state when not (gains.ContainsKey state) -> return Error(UnpricedHand hand)
        | Some state ->
            let index = Word.indexOf[card]
            let m = Word.missingOf state index

            if m >= Word.fullCounts[index] then
                return Error(NoCopiesLeft card)
            else
                let struct (_, _, spare, _) = Word.facts state

                if Word.isValue[index] && m > 0 && spare <= 0 then
                    return Ok 0.0 // the draw busts, and a bust banks nothing
                else
                    let child = System.Array.BinarySearch(childStates, Word.drawn state index)
                    return Ok childWorths[child]
    }
