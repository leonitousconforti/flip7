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
// else needs storing but the second chances the hand passed on. Each card's
// count gets just enough bits for its copies in the full deck, fifty bits in
// all, and the count passed on gets as many bits as a second chance's. None of
// this depends on any particular search, only on the full deck itself.
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
    let masks = widths |> Array.map (fun width -> (1 <<< width) - 1)

    let indexOf = cards |> Array.mapi (fun index (card, _) -> card, index) |> dict
    let isValue = cards |> Array.map (fun (card, _) -> card.IsValueCard)
    let valuePoints = cards |> Array.map (fun (card, _) -> int card.Value.ValuePoints)
    let modifierPoints =
        cards |> Array.map (fun (card, _) -> int card.Value.ModifierPoints)
    let doubleIndex = indexOf[ModifierCard Card.Double]
    let secondChanceIndex = indexOf[ActionCard Card.SecondChance]

    // A hand holds one second chance at most: one drawn while it holds another
    // is passed on or discarded, as the engine does, so the hand stays as it
    // was and only the deck loses the card. Those cards are counted above all
    // the others, and drawing one is a move of its own beside each card's
    let passedOffset = Array.sum widths
    let passedMove = cards.Length
    let slots = Array.append offsets [| passedOffset |]

    // The word is a uint64 on .NET. Fable makes a uint64 a BigInt, which JS
    // computes with slowly, but every word fits in 52 bits, so JS keeps it in
    // a double instead: adding a count placed at an offset is exact there, and
    // the bits from a move's slot up are read by scaling them down to the bottom
#if FABLE_COMPILER
    type State = float
    let room = 53
    let top: State = infinity
    let place (count: int) (offset: int) : State = float count * 2.0 ** float offset
    let scales = slots |> Array.map (fun offset -> 0.5 ** float offset)
    let read (state: State) (move: int) : int = int (uint32 (state * scales[move]))
#else
    type State = uint64
    let room = 64
    let top: State = System.UInt64.MaxValue
    let place (count: int) (offset: int) : State = uint64 count <<< offset
    let read (state: State) (move: int) : int = int (state >>> slots[move])
#endif

    let moves = slots |> Array.map (place 1)

    do
        if passedOffset + widths[secondChanceIndex] > room then
            invalidOp "the missing counts no longer fit in one word"

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

    let missingOf (state: State) (index: int) : int = read state index &&& masks[index]

    let drawn (state: State) (move: int) : State = state + moves[move]

    let passedOf (state: State) : int =
        read state passedMove &&& masks[secondChanceIndex]

    let copiesLeft (universe: Universe) (state: State) (index: int) : int =
        let passed = if index = secondChanceIndex then passedOf state else 0
        universe.Counts[index] - missingOf state index - passed

    // The move a card makes: a second chance drawn while the hand has one spare
    // is passed on, and every other card joins the hand
    let moveOf (spare: int) (index: int) : int =
        if index = secondChanceIndex && spare > 0 then
            passedMove
        else
            index

    // A hand fits a word only when its universe holds every copy it claims. Of
    // the second chances its dups leave uncancelled it keeps one, and the rest
    // it passed on
    let tryEncode (universe: Universe) (hand: Hand) : State option =
        let values = hand |> List.filter (fun card -> card.IsValueCard)
        let dups = values.Length - (List.distinct values).Length
        let chances =
            hand |> List.filter ((=) (ActionCard Card.SecondChance)) |> List.length
        let passed = max 0 (chances - dups - 1)

        (Some(place 0 0), hand)
        ||> List.fold (fun state card ->
            let index = indexOf[card]

            state
            |> Option.filter (fun state -> missingOf state index < universe.Counts[index])
            |> Option.map (fun state -> drawn state index)
        )
        |> Option.map (fun state -> state - place passed offsets[secondChanceIndex] + place passed passedOffset)

    let shown (hand: Hand) : string =
        hand |> List.map string |> String.concat " "

    // One pass over a state's word: what it banks (values times the x2,
    // modifiers and the flip7 bonus on top, exactly as Hand.Score counts), the
    // distinct values held, the second chances left after cancelling dups, and
    // how many cards the deck still holds
    let facts (universe: Universe) (state: State) : struct (float * int * int * int) =
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
        struct (banked, distinct, spare, universe.Size - missing - passedOf state)

    // Which moves a state lives through, one bit per move: a card with copies
    // left that is new to the hand or covered by a spare second chance. A hand
    // that has flipped seven has stopped drawing and lives through nothing
    let liveMask (universe: Universe) (state: State) : uint32 =
        let struct (_, distinct, spare, _) = facts universe state
        let mutable mask = 0u

        if distinct < 7 then
            for index in 0 .. cards.Length - 1 do
                let m = missingOf state index
                let count = copiesLeft universe state index

                if count > 0 && (spare > 0 || not (isValue[index] && m > 0)) then
                    mask <- mask ||| (1u <<< moveOf spare index)

        mask

    // Every index below a count, in parallel under the token. JS has the one
    // thread, so Fable runs them in order
    let inline inParallel
        (token: System.Threading.CancellationToken)
        (count: int)
        ([<InlineIfLambda>] body: int -> unit)
        : unit =
#if FABLE_COMPILER
        token.ThrowIfCancellationRequested()

        for at in 0 .. count - 1 do
            body at
#else
        let options = System.Threading.Tasks.ParallelOptions(CancellationToken = token)
        System.Threading.Tasks.Parallel.For(0, count, options, (fun at -> body at))
        |> ignore
#endif

    // The forward sweep: the states one draw deeper than a generation. Adding a
    // move's delta to every state of a sorted generation keeps it sorted, so
    // the children arrive as one sorted stream per move and one merge with
    // dedup produces the next generation already sorted: no hashing, and no
    // memory beyond the output itself. The output key space is split at sampled
    // quantiles so the partitions can merge in parallel
    let expand (token: System.Threading.CancellationToken) (universe: Universe) (frontier: State[]) : State[] =
        let live = Array.zeroCreate frontier.Length
        inParallel token frontier.Length (fun at -> live[at] <- liveMask universe frontier[at])

        // Sampled children, for boundaries that balance the partitions
        let sample = ResizeArray<State>()
        let stride = max 1 (frontier.Length / 1024)

        for at in 0..stride .. frontier.Length - 1 do
            for index in 0 .. moves.Length - 1 do
                if live[at] &&& (1u <<< index) <> 0u then
                    sample.Add(frontier[at] + moves[index])

        sample.Sort()

        let partitions =
            if sample.Count < 64 then
                1
            else
#if FABLE_COMPILER
                1 // JS merges one partition after another anyway
#else
                min (System.Environment.ProcessorCount * 4) (sample.Count / 16)
#endif

        let bounds =
            Array.init
                (partitions + 1)
                (fun partition ->
                    if partition = 0 then place 0 0
                    elif partition = partitions then top
                    else sample[partition * sample.Count / partitions]
                )

        let lowerBound (value: State) : int =
            let found = System.Array.BinarySearch(frontier, value)
            if found >= 0 then found else ~~~found

        let buffers = Array.init partitions (fun _ -> ResizeArray<State>())

        inParallel
            token
            partitions
            (fun partition ->
                let low = bounds[partition]
                let high = bounds[partition + 1]

                // One cursor per move, walking the parents whose child for that
                // move lands inside this partition's slice of key space
                let position = Array.zeroCreate moves.Length
                let finish = Array.zeroCreate moves.Length
                let current = Array.create moves.Length top

                for index in 0 .. moves.Length - 1 do
                    let delta = moves[index]
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

                    current[index] <- if at < finishAt then frontier[at] + delta else top

                // The merge: emit the smallest child not yet emitted, advance
                // its cursor to the next parent that lives through the move
                let output = buffers[partition]
                let mutable emitted = top
                let mutable draining = true

                while draining do
                    let mutable best = 0

                    for index in 1 .. moves.Length - 1 do
                        if current[index] < current[best] then
                            best <- index

                    if current[best] = top then
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

                        current[best] <- if at < finishAt then frontier[at] + moves[best] else top
            )

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
        (nextStates: State[])
        (nextWorths: float[])
        (state: State)
        (spare: int)
        (remaining: int)
        : float =
        let mutable sum = 0.0

        for index in 0 .. cards.Length - 1 do
            let m = missingOf state index
            let count = copiesLeft universe state index

            if count > 0 && (spare > 0 || not (isValue[index] && m > 0)) then
                let child = System.Array.BinarySearch(nextStates, drawn state (moveOf spare index))
                sum <- sum + float count / float remaining * nextWorths[child]

        sum

    // What a state is worth held: the better of banking now and playing on. A
    // hand that has flipped seven banks whatever the sight, because the round
    // ends there
    let worthOf (universe: Universe) (nextStates: State[]) (nextWorths: float[]) (state: State) : float =
        let struct (banked, distinct, spare, remaining) = facts universe state

        if distinct >= 7 then
            banked
        else
            max banked (afterHitting universe nextStates nextWorths state spare remaining)

    let bankedOf (universe: Universe) (state: State) : float =
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
///
/// Under Fable, JS having the one thread, the work waits for the event loop's
/// next turn instead and then runs to the end without yielding, and its answers
/// are the same to the bit. A searcher disposed or cancelled before the work
/// starts cancels the members awaiting it, as Fable cancels an async, rather
/// than raising in them. The progress callback is all that runs meanwhile, and
/// a cancellation from there fails them with the plain Exception Fable raises
/// for a cancelled token, or, from its last report, cancels them where .NET
/// would still answer.
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

    // The whole table prices once, starting now; everything below awaits it. A
    // task rather than an async, since the pricing has nothing to await: a
    // cancellation thrown under the task's own token marks it cancelled rather
    // than faulted, so awaiters see an OperationCanceledException, and its
    // continuations run on the pool rather than in line on the thread that
    // finished it, so awaiters never queue up behind one another
    let table =
        fun () ->
            let started = System.DateTime.UtcNow // Fable has no Stopwatch
            let elapsed () =
                (System.DateTime.UtcNow - started).TotalSeconds

            // The sweep needs its generations sorted, and the hands arrive in
            // whatever order the caller grew them
            let start = Array.sort (Array.distinct roots)

            // Sweep from the hands to a generation, returning it and the one
            // just before it. Nothing else survives the sweep: the backward
            // pass re-sweeps as it descends, trading a little repeated sweeping
            // for never holding more than a rolling pair
            let sweepTo (level: int) : Word.State[] * Word.State[] =
                let mutable previous = [||]
                let mutable current = start

                for step in 1..level do
                    previous <- current
                    current <- Word.expand token universe current
                    report $"sweep {step}/{level}: {current.Length} states, %.0f{elapsed ()}s"

                previous, current

            // Price a generation in parallel, under the cancellation token
            let priceBy (price: Word.State -> float) (states: Word.State[]) : float[] =
                let worths = Array.zeroCreate states.Length
                Word.inParallel token states.Length (fun at -> worths[at] <- price states[at])
                worths

            // The backward pass: the deepest generation banks - its sight is
            // spent - and each earlier one is priced against the one after it
            let penultimate, deepest = sweepTo depth
            let mutable held = Some penultimate
            let mutable aboveStates = deepest
            let mutable aboveWorths = deepest |> priceBy (Word.bankedOf universe)

            report $"price {depth}/{depth}: {deepest.Length} states, %.0f{elapsed ()}s"

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
                report $"price {level}/{depth}: {states.Length} states, %.0f{elapsed ()}s"

            // The generation one draw in stays alive: the hands are priced
            // against it, and the draw-by-draw questions read from it too
            let childStates, childWorths = aboveStates, aboveWorths
            let gains = Dictionary<Word.State, float>()

            for state in roots do
                let struct (banked, distinct, spare, remaining) = Word.facts universe state

                gains[state] <-
                    if distinct >= 7 then
                        0.0
                    else
                        Word.afterHitting universe childStates childWorths state spare remaining
                        - banked

            gains, childStates, childWorths
        |> fun work ->
#if FABLE_COMPILER
            // JS has no thread pool: the work waits for the event loop's next
            // turn, so the constructor still returns first. Fable's
            // Async.StartAsTask imports a function its library lacks, so a
            // completion source stands in, and its runtime spells SetCanceled
            // SetCancelled, so a cancellation is passed on as the exception it
            // is, which awaiters treat as cancelled. JS reports a promise
            // rejected with nothing to handle it, so the table handles its
            // own, as fable-library's startChild does, and a searcher cancelled
            // before anything awaits it is harmless
            let source = System.Threading.Tasks.TaskCompletionSource<_>()
            Async.StartWithContinuations(Async.AwaitTask source.Task, ignore, ignore, ignore)

            Async.StartWithContinuations(
                async {
                    do! Async.Sleep 0
                    return work ()
                },
                source.SetResult,
                source.SetException,
                source.SetException,
                token
            )

            source.Task
#else
            System.Threading.Tasks.Task.Factory.StartNew(
                work,
                token,
                System.Threading.Tasks.TaskCreationOptions.RunContinuationsAsynchronously,
                System.Threading.Tasks.TaskScheduler.Default
            )
#endif

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

            if Word.copiesLeft universe state index <= 0 then
                return Error(NoCopiesLeft card)
            else
                let struct (_, _, spare, _) = Word.facts universe state

                if Word.isValue[index] && m > 0 && spare <= 0 then
                    return Ok 0.0 // the draw busts, and a bust banks nothing
                else
                    let child =
                        System.Array.BinarySearch(childStates, Word.drawn state (Word.moveOf spare index))
                    return Ok childWorths[child]
    }

    interface System.IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                canceller.Cancel()
                canceller.Dispose()
