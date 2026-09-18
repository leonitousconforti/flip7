namespace Flip7

open System.Collections.Generic

/// <summary>
/// The ways a question can miss a searcher's table: a hand its deck could never
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
    let deltas = offsets |> Array.map (fun offset -> 1UL <<< offset)

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

    // The deck a search draws from: a count per card in word order, and the
    // total. Any deck within the full one packs the same way; one that outgrows
    // it does not fit the words
    type Universe = { Counts: int[]; Size: int }

    let universeOf (deck: Deck) : Universe option =
        let counts =
            cards
            |> Array.map (fun (card, _) -> deck |> Map.tryFind card |> Option.defaultValue 0u |> int)

        if Array.forall2 (<=) counts fullCounts then
            Some { Counts = counts; Size = Array.sum counts }
        else
            None

    let missingOf (state: uint64) (index: int) : int =
        int ((state >>> offsets[index]) &&& masks[index])

    let drawn (state: uint64) (index: int) : uint64 = state + deltas[index]

    // A hand fits a word only when its universe holds every copy it claims
    let tryEncode (universe: Universe) (hand: Hand) : uint64 option =
        (Some 0UL, hand)
        ||> List.fold (fun state card ->
            let index = indexOf[card]

            state
            |> Option.filter (fun state -> missingOf state index < universe.Counts[index])
            |> Option.map (fun state -> drawn state index)
        )

    let shown (hand: Hand) : string =
        hand |> List.map string |> String.concat " "

    // One pass over a state's word: what it banks (values times the x2,
    // modifiers and the flip7 bonus on top, exactly as Hand.Score counts), the
    // distinct values held, the second chances left after cancelling dups, and
    // how many cards the deck still holds
    let facts (universe: Universe) (state: uint64) : struct (float * int * int * int) =
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
        struct (banked, distinct, spare, universe.Size - missing)

    // Which draws a state lives through, one bit per card: a card with copies
    // left that is new to the hand or covered by a spare second chance. A hand
    // that has flipped seven has stopped drawing and lives through nothing
    let liveMask (universe: Universe) (state: uint64) : uint32 =
        let struct (_, distinct, spare, _) = facts universe state
        let mutable mask = 0u

        if distinct < 7 then
            for index in 0 .. cards.Length - 1 do
                let m = missingOf state index

                if m < universe.Counts[index] && (spare > 0 || not (isValue[index] && m > 0)) then
                    mask <- mask ||| (1u <<< index)

        mask

    // The forward sweep: the states one draw deeper than a generation. Adding a
    // card's delta to every state of a sorted generation keeps it sorted, so
    // the children arrive as twenty-one sorted streams and one merge with dedup
    // produces the next generation already sorted: no hashing, and no memory
    // beyond the output itself. The output key space is split at sampled
    // quantiles so the partitions can merge in parallel
    let expand (token: System.Threading.CancellationToken) (universe: Universe) (frontier: uint64[]) : uint64[] =
        let options = System.Threading.Tasks.ParallelOptions(CancellationToken = token)
        let live = Array.zeroCreate frontier.Length

        System.Threading.Tasks.Parallel.For(
            0,
            frontier.Length,
            options,
            (fun at -> live[at] <- liveMask universe frontier[at])
        )
        |> ignore

        // Sampled children, for boundaries that balance the partitions
        let sample = ResizeArray<uint64>()
        let stride = max 1 (frontier.Length / 1024)

        for at in 0..stride .. frontier.Length - 1 do
            for index in 0 .. cards.Length - 1 do
                if live[at] &&& (1u <<< index) <> 0u then
                    sample.Add(frontier[at] + deltas[index])

        sample.Sort()

        let partitions =
            if sample.Count < 64 then
                1
            else
                min (System.Environment.ProcessorCount * 4) (sample.Count / 16)

        let bounds =
            Array.init
                (partitions + 1)
                (fun partition ->
                    if partition = 0 then 0UL
                    elif partition = partitions then System.UInt64.MaxValue
                    else sample[partition * sample.Count / partitions]
                )

        let lowerBound (value: uint64) : int =
            let found = System.Array.BinarySearch(frontier, value)
            if found >= 0 then found else ~~~found

        let buffers = Array.init partitions (fun _ -> ResizeArray<uint64>())

        System.Threading.Tasks.Parallel.For(
            0,
            partitions,
            options,
            fun partition ->
                let low = bounds[partition]
                let high = bounds[partition + 1]

                // One cursor per card, walking the parents whose child for that
                // card lands inside this partition's slice of key space
                let position = Array.zeroCreate cards.Length
                let finish = Array.zeroCreate cards.Length
                let current = Array.create cards.Length System.UInt64.MaxValue

                for index in 0 .. cards.Length - 1 do
                    let delta = deltas[index]
                    let bit = 1u <<< index
                    let finishAt = if delta >= high then 0 else lowerBound (high - delta)

                    let mutable at =
                        if delta >= low then
                            0
                        else
                            min finishAt (lowerBound (low - delta))

                    while at < finishAt && live[at] &&& bit = 0u do
                        at <- at + 1

                    position[index] <- at
                    finish[index] <- finishAt

                    current[index] <-
                        if at < finishAt then
                            frontier[at] + delta
                        else
                            System.UInt64.MaxValue

                // The merge: emit the smallest child not yet emitted, advance
                // its cursor to the next parent that lives through the card
                let output = buffers[partition]
                let mutable emitted = System.UInt64.MaxValue
                let mutable draining = true

                while draining do
                    let mutable best = 0

                    for index in 1 .. cards.Length - 1 do
                        if current[index] < current[best] then
                            best <- index

                    if current[best] = System.UInt64.MaxValue then
                        draining <- false
                    else
                        if current[best] <> emitted then
                            emitted <- current[best]
                            output.Add emitted

                            // A partition can drain for a long while, and the
                            // options only guard between partitions
                            if output.Count &&& 0xFFFF = 0 then
                                token.ThrowIfCancellationRequested()

                        let bit = 1u <<< best
                        let finishAt = finish[best]
                        let mutable at = position[best] + 1

                        while at < finishAt && live[at] &&& bit = 0u do
                            at <- at + 1

                        position[best] <- at

                        current[best] <-
                            if at < finishAt then
                                frontier[at] + deltas[best]
                            else
                                System.UInt64.MaxValue
        )
        |> ignore

        let states = Array.zeroCreate (buffers |> Array.sumBy (fun buffer -> buffer.Count))
        let mutable at = 0

        for buffer in buffers do
            buffer.CopyTo(states, at)
            at <- at + buffer.Count

        states

    // What playing on is worth against the next generation's worths: the
    // average over every card that could come, a bust worth nothing
    let afterHitting
        (universe: Universe)
        (nextStates: uint64[])
        (nextWorths: float[])
        (state: uint64)
        (spare: int)
        (remaining: int)
        : float =
        let mutable sum = 0.0

        for index in 0 .. cards.Length - 1 do
            let m = missingOf state index
            let count = universe.Counts[index] - m

            if count > 0 && (spare > 0 || not (isValue[index] && m > 0)) then
                let child = System.Array.BinarySearch(nextStates, drawn state index)
                sum <- sum + float count / float remaining * nextWorths[child]

        sum

    // What a state is worth held: the better of banking now and playing on. A
    // hand that has flipped seven banks whatever the sight, because the round
    // ends there
    let worthOf (universe: Universe) (nextStates: uint64[]) (nextWorths: float[]) (state: uint64) : float =
        let struct (banked, distinct, spare, remaining) = facts universe state

        if distinct >= 7 then
            banked
        else
            max banked (afterHitting universe nextStates nextWorths state spare remaining)

    let bankedOf (universe: Universe) (state: uint64) : float =
        let struct (banked, _, _, _) = facts universe state
        banked

/// <summary>
/// What a hand is worth when it is played on as well as it can be, computed
/// rather than sampled: the deck is a known multiset, so the chance of every
/// card that could come next is known exactly and the average over them can be
/// summed instead of estimated. There is no sampling error in any of this, and
/// nothing here needs a threshold to decide whether it has seen enough.
///
/// A searcher prices every hand it is given at construction, depth cards of
/// sight each, against its deck missing exactly that hand - the full deck
/// unless one is given, so a mid-game hand is priced by handing over the
/// drawable cards plus the hand itself. The work starts on the thread pool the
/// moment the searcher is made and runs bottom up: a forward sweep collects
/// every state reachable within the depth, one generation a draw deeper than
/// the last, and a backward pass prices each generation against the one after
/// it. No generation is kept for later: the backward pass re-sweeps forward to
/// each level it descends to, keeping the one just before its target so every
/// other descent costs nothing, and never holds more than a rolling pair of
/// generations. The members await the table, so they may be called before it is
/// done.
///
/// The searcher answers only for the hands it was given: a state is named by
/// what the full deck is missing, and the missing cards name the hand they
/// built, so a hand it never swept from is a question it never priced.
///
/// Disposing the searcher cancels whatever work remains, as does the
/// cancellation token it may be given; members awaiting a cancelled table raise
/// the usual OperationCanceledException.
/// </summary>
type public Lookahead
    (
        depth: int,
        hands: Hand seq,
        ?deck: Deck,
        ?progress: string -> unit,
        ?cancellation: System.Threading.CancellationToken
    )
    =
    do
        if depth < 1 then
            invalidArg (nameof depth) "the search needs at least one card of sight"

    let universe =
        match Word.universeOf (defaultArg deck Deck.Full) with
        | Some universe -> universe
        | None -> invalidArg (nameof deck) "the deck holds more copies of a card than the full deck ever had"

    let report = defaultArg progress ignore

    let canceller =
        match cancellation with
        | Some token -> System.Threading.CancellationTokenSource.CreateLinkedTokenSource token
        | None -> new System.Threading.CancellationTokenSource()

    let token = canceller.Token
    let options = System.Threading.Tasks.ParallelOptions(CancellationToken = token)
    let mutable disposed = false

    let roots =
        hands
        |> Seq.map (fun hand ->
            if Hand.IsBust hand then
                invalidArg (nameof hands) $"[{Word.shown hand}] is already bust"

            match Word.tryEncode universe hand with
            | Some state -> state
            | None -> invalidArg (nameof hands) $"[{Word.shown hand}] holds more copies than its deck"
        )
        |> Seq.toArray

    // The whole table prices once, starting now; everything below awaits it
    let table =
        async {
            let watch = System.Diagnostics.Stopwatch.StartNew()

            // The sweep needs its generations sorted, and the hands arrive in
            // whatever order the caller grew them
            let start = Array.sort (Array.distinct roots)

            // Sweep from the hands to a generation, returning it and the one
            // just before it. Nothing else survives the sweep: the backward
            // pass re-sweeps as it descends, trading a little repeated sweeping
            // for never holding more than a rolling pair
            let sweepTo (level: int) : uint64[] * uint64[] =
                let mutable previous = [||]
                let mutable current = start

                for step in 1..level do
                    previous <- current
                    current <- Word.expand token universe current
                    report $"sweep {step}/{level}: {current.Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

                previous, current

            // Price a generation in parallel, under the cancellation token
            let priceBy (price: uint64 -> float) (states: uint64[]) : float[] =
                let worths = Array.zeroCreate states.Length

                System.Threading.Tasks.Parallel.For(
                    0,
                    states.Length,
                    options,
                    (fun at -> worths[at] <- price states[at])
                )
                |> ignore

                worths

            // The backward pass: the deepest generation banks - its sight is
            // spent - and each earlier one is priced against the one after it
            let penultimate, deepest = sweepTo depth
            let mutable held = Some penultimate
            let mutable aboveStates = deepest
            let mutable aboveWorths = deepest |> priceBy (Word.bankedOf universe)

            report $"price {depth}/{depth}: {deepest.Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

            for level in depth - 1 .. -1 .. 1 do
                let states =
                    match held with
                    | Some states ->
                        held <- None
                        states
                    | None ->
                        let previous, states = sweepTo level
                        held <- Some previous
                        states

                let nextStates = aboveStates
                let nextWorths = aboveWorths
                let worths = states |> priceBy (Word.worthOf universe nextStates nextWorths)
                aboveStates <- states
                aboveWorths <- worths
                report $"price {level}/{depth}: {states.Length} states, %.0f{watch.Elapsed.TotalSeconds}s"

            // The generation one draw in stays alive: the hands are priced
            // against it, and the draw-by-draw questions read from it too
            let childStates, childWorths = aboveStates, aboveWorths
            let gains = Dictionary<uint64, float>()

            for state in roots do
                let struct (banked, distinct, spare, remaining) = Word.facts universe state

                gains[state] <-
                    if distinct >= 7 then
                        0.0
                    else
                        Word.afterHitting universe childStates childWorths state spare remaining
                        - banked

            return gains, childStates, childWorths
        }
        |> fun work -> Async.StartAsTask(work, cancellationToken = token)

    /// <summary>
    /// What hitting is worth over standing, in points of expected round score,
    /// for one of the hands given at construction. Positive says hit. Errors
    /// when the hand is not one the searcher priced.
    /// </summary>
    member _.GainFromHitting(hand: Hand) : Async<Result<float, LookaheadError>> = async {
        let! gains, _, _ = Async.AwaitTask table

        match Word.tryEncode universe hand with
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

        match Word.tryEncode universe hand with
        | None -> return Error(ImpossibleHand hand)
        | Some state when not (gains.ContainsKey state) -> return Error(UnpricedHand hand)
        | Some state ->
            let index = Word.indexOf[card]
            let m = Word.missingOf state index

            if m >= universe.Counts[index] then
                return Error(NoCopiesLeft card)
            else
                let struct (_, _, spare, _) = Word.facts universe state

                if Word.isValue[index] && m > 0 && spare <= 0 then
                    return Ok 0.0 // the draw busts, and a bust banks nothing
                else
                    let child = System.Array.BinarySearch(childStates, Word.drawn state index)
                    return Ok childWorths[child]
    }

    interface System.IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                canceller.Cancel()
                canceller.Dispose()
