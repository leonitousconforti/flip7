namespace Flip7

/// <summary>
/// Recovers the seed of a seeded System.Random from what its calls returned.
///
/// A seeded System.Random is .NET's legacy subtractive generator, and every
/// value it yields is affine in the seed modulo Int32.MaxValue, a Mersenne
/// prime, up to one of four corrections. So a call of Next(0, n) that returned
/// a known index confines the seed to a few arithmetic progressions, and a
/// handful of calls leave few enough seeds to list instead of 2^31 to try.
/// </summary>
module internal Seeded =
    // The modulus every value of the generator is reduced by
    [<Literal>]
    let private P = 2147483647L

    // Knuth's constant the generator subtracts the seed from
    [<Literal>]
    let private MSeed = 161803398

    /// <summary>
    /// The affine forms of a stretch of the stream: for a seed s from 0 to
    /// Int32.MaxValue of class c, the value at position k, counted from zero,
    /// is (Alpha[k] * s + Gamma[4 * k + c]) % Int32.MaxValue. A position counts
    /// every call on the Random, since Next(), Next(min, max) and NextDouble
    /// each take exactly one value.
    ///
    /// The class records which of two subtractions in the constructor's first
    /// mixing pass overflowed int32. Both read word 55 while it still holds
    /// Knuth's constant less the seed, unreduced and negative for every seed
    /// above it: bit 0 is set when word 24 less word 55 passes Int32.MaxValue,
    /// which leaves word 24 two short because 2^32 is 2 modulo Int32.MaxValue,
    /// and bit 1 when word 55 less word 31 falls below Int32.MinValue, which
    /// leaves word 55 two over. Every seed up to Knuth's constant is of class
    /// 0. At some positions the corrections cancel out and the classes'
    /// constants agree. That these forms give every value of every seed, with
    /// no exceptions, was proved seed by seed against the real System.Random.
    /// </summary>
    type internal Affine = { Alpha: int64 array; Gamma: int64 array }

    /// <summary>
    /// The affine forms of the stream's first count values, from a run of the
    /// generator on the forms themselves.
    /// </summary>
    let internal AffineForms (count: int) : Affine =
        // Every word is held as five lanes modulo Int32.MaxValue: its
        // coefficient of the seed, then its constant in each class. The
        // generator only ever subtracts one word from another, which keeps them
        // affine, and its reductions only add multiples of the modulus
        let lanes = 5
        let words = Array.zeroCreate<int64>(56 * lanes)
        let mj = Array.zeroCreate<int64> lanes
        let mk = Array.zeroCreate<int64> lanes

        let subtract (target: int) (source: int) =
            for lane in 0 .. lanes - 1 do
                words[target * lanes + lane] <- (words[target * lanes + lane] - words[source * lanes + lane] + P) % P

        let shift (word: int) (amount: int64) (classes: int list) =
            for c in classes do
                words[word * lanes + 1 + c] <- (words[word * lanes + 1 + c] + amount + P) % P

        // Word 55 and mj start as Knuth's constant less the seed
        mj[0] <- P - 1L

        for lane in 1..4 do
            mj[lane] <- int64 MSeed
            mk[lane] <- 1L

        Array.blit mj 0 words (55 * lanes) lanes
        let mutable ii = 0

        for _ in 1..54 do
            ii <- ii + 21

            if ii >= 55 then
                ii <- ii - 55

            Array.blit mk 0 words (ii * lanes) lanes

            for lane in 0 .. lanes - 1 do
                mk[lane] <- (mj[lane] - mk[lane] + P) % P

            Array.blit words (ii * lanes) mj 0 lanes

        for pass in 1..4 do
            for i in 1..55 do
                subtract i (if i <= 24 then i + 31 else i - 24)

                if pass = 1 && i = 24 then
                    shift 24 -2L [ 1; 3 ]

                if pass = 1 && i = 55 then
                    shift 55 2L [ 2; 3 ]

        let alpha = Array.zeroCreate<int64> count
        let gamma = Array.zeroCreate<int64>(4 * count)
        let mutable next = 0
        let mutable nextp = 21

        for k in 0 .. count - 1 do
            next <- if next = 55 then 1 else next + 1
            nextp <- if nextp = 55 then 1 else nextp + 1
            subtract next nextp
            alpha[k] <- words[next * lanes]

            for c in 0..3 do
                gamma[4 * k + c] <- words[next * lanes + 1 + c]

        { Alpha = alpha; Gamma = gamma }

    /// <summary>
    /// The class of a seed from 0 to Int32.MaxValue, as Affine describes it:
    /// the constructor's first loop, which never overflows, then the two
    /// subtractions of its first mixing pass that can, computed wide enough to
    /// see whether they do. The words are scratch space for the loop, 56 of
    /// them, so that classifying seed after seed allocates nothing. This is the
    /// very computation the proof checked against the class of every seed.
    /// </summary>
    let internal ClassOf (words: int array) (seed: int) : int =
        let word55 = MSeed - seed
        let mutable mj = word55
        let mutable mk = 1
        let mutable ii = 0

        // Where the constructor adds Int32.MaxValue to a negative difference,
        // this adds the difference's sign, shifted down to all ones or all
        // zeros, masked to Int32.MaxValue: the same int, with no branch to
        // guess, and that branch goes either way about half the time
        let reduce (difference: int) =
            difference + ((difference >>> 31) &&& System.Int32.MaxValue)

        for _ in 1..54 do
            ii <- ii + 21

            if ii >= 55 then
                ii <- ii - 55

            words[ii] <- mk
            mk <- reduce (mj - mk)
            mj <- words[ii]

        // The first pass takes word 31 from word 55 only after taking word 7
        // from word 31 and word 38 from word 7, none of which overflows
        let word31 = reduce (words[31] - reduce (words[7] - words[38]))
        let above = int64 words[24] - int64 word55 > int64 System.Int32.MaxValue
        let below = int64 word55 - int64 word31 < int64 System.Int32.MinValue
        (if above then 1 else 0) ||| (if below then 2 else 0)

    /// <summary>
    /// The values for which Next(0, count) returns an index from low to high,
    /// as the first and last of them.
    /// </summary>
    let internal Interval (count: int) (low: int) (high: int) : struct (int * int) =
        if count < 1 || low < 0 || high < low || high >= count then
            let culprit =
                if count < 1 then nameof count
                elif low < 0 || high < low then nameof low
                else nameof high

            invalidArg culprit $"Next(0, {count}) cannot return every index from {low} to {high}"

        // Next scales the value by 1.0 / Int32.MaxValue, then by the count, in
        // double precision and truncates. Rounding to the nearest double never
        // reverses an order and neither does truncation, so the index never
        // falls as the value rises, the values of each index form a run, and a
        // binary search with the very same arithmetic finds where each run
        // starts exactly. An index nothing reaches starts at Int32.MaxValue,
        // one past the largest value
        let first (index: int) =
            let mutable below = 0
            let mutable above = System.Int32.MaxValue

            while below < above do
                let middle = below + (above - below) / 2

                if int (float middle * (1.0 / float System.Int32.MaxValue) * float count) >= index then
                    above <- middle
                else
                    below <- middle + 1

            below

        struct (first low, first (high + 1) - 1)

    /// <summary>
    /// A call of Next(0, Count) at a position of a seeded stream, counted from
    /// zero, that returned an index from Low to High: a card drawn from a deck
    /// of Count cards whose copies sit at those indices of the shuffled array.
    /// </summary>
    [<Struct>]
    type internal Constraint = { Position: int; Count: int; Low: int; High: int }

    // The values the constraints allow at each position they name, in order of
    // position, with the constraints on one call intersected. None when two of
    // them contradict each other
    let private windows (constraints: Constraint seq) : struct (int * int * int) array option =
        let merged =
            constraints
            |> Seq.map (fun constrained ->
                if constrained.Position < 0 then
                    invalidArg (nameof constraints) $"Position {constrained.Position} is before the stream starts"

                constrained.Position, Interval constrained.Count constrained.Low constrained.High
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (position, intervals) ->
                struct (position,
                        intervals |> Seq.map (fun (_, struct (low, _)) -> low) |> Seq.max,
                        intervals |> Seq.map (fun (_, struct (_, high)) -> high) |> Seq.min)
            )
            |> Seq.sortBy (fun struct (position, _, _) -> position)
            |> Array.ofSeq

        if merged |> Array.exists (fun struct (_, low, high) -> low > high) then
            None
        else
            Some merged

    /// <summary>
    /// How many seeds from 0 to Int32.MaxValue are expected to be consistent
    /// with the constraints, taking the values at different positions to be
    /// independent: 2^31 times the fraction of values each constrained position
    /// allows. It foretells how many seeds Enumerate lists, and so its memory
    /// and the cost of filtering what it lists, though not its time. It is also
    /// how many seeds are expected to agree by chance with calls that no seed
    /// made, such as those of a Random used before they were counted.
    /// </summary>
    let internal Expected (constraints: Constraint seq) : float =
        match windows constraints with
        | None -> 0.0
        | Some windows ->
            windows
            |> Array.fold
                (fun expected struct (_, low, high) -> expected * float (high - low + 1) / float P)
                2147483648.0

    /// <summary>
    /// The seed of a survivor, as Enumerate and Filter give them.
    /// </summary>
    let internal SeedOf (survivor: int64) : int = int (survivor >>> 2)

    /// <summary>
    /// Every seed from 0 to Int32.MaxValue whose System.Random makes all of the
    /// calls the constraints describe, and no other seed: each shifted up two
    /// bits with its class in the two below, so that sorting them sorts the
    /// seeds, and in ascending order. The forms must reach every constrained
    /// position.
    ///
    /// It walks the values the tightest constraint allows, once for each group
    /// of classes whose forms agree at every constrained position. Under the
    /// group a value pins the seed modulo Int32.MaxValue, the other constraints
    /// are checked against their affine forms, and a seed they let through is
    /// kept only if its own class is in the group: the forms are exact, so its
    /// class is all a seed needs, and it costs the same however far along the
    /// stream the constraints are. The residue 0 stands for seeds 0 and
    /// Int32.MaxValue, which are of different classes, so both are classified.
    /// Every seed is in exactly one group, so none is found twice. It takes
    /// time in proportion to the tightest constraint's values times the groups,
    /// across every core, which a single draw from a nearly full deck already
    /// narrows to tens of millions of values whatever Expected is, and memory
    /// in proportion to the seeds it returns, some 40 bytes a seed, which
    /// Expected foretells. Constraints that every seed meets cannot be listed,
    /// and are refused with an ArgumentException: Expected is 2^31 for them, so
    /// a caller that enumerates only once Expected is small never meets one.
    /// </summary>
    let internal Enumerate (forms: Affine) (constraints: Constraint seq) : int64 array =
        match windows constraints with
        | None -> [||]
        | Some windows ->
            // A position that allows every value says nothing about the seed
            let windows =
                windows
                |> Array.filter (fun struct (_, low, high) -> int64 high - int64 low + 1L < P)

            if Array.isEmpty windows then
                invalidArg (nameof constraints) "Every seed is consistent with constraints that allow every value"

            let width (struct (_, low, high)) = int64 high - int64 low + 1L

            // A value whose coefficient is 0 does not depend on the seed, so it
            // can only filter. None is below position 200000
            let walked =
                windows
                |> Array.filter (fun struct (position, _, _) -> forms.Alpha[position] <> 0L)
                |> Array.sortBy width
                |> Array.tryHead
                |> Option.defaultWith (fun () ->
                    invalidArg (nameof constraints) "No constrained value depends on the seed"
                )

            let filters = windows |> Array.filter ((<>) walked) |> Array.sortBy width
            let struct (walkedAt, walkedLow, walkedHigh) = walked
            let alpha = forms.Alpha[walkedAt]

            // Int32.MaxValue is prime, so alpha to the power Int32.MaxValue - 2
            // is alpha's inverse
            let inverse =
                let mutable inverse = 1L
                let mutable square = alpha
                let mutable exponent = P - 2L

                while exponent > 0L do
                    if exponent &&& 1L = 1L then
                        inverse <- inverse * square % P

                    square <- square * square % P
                    exponent <- exponent >>> 1

                inverse

            let filterAlpha =
                filters |> Array.map (fun struct (position, _, _) -> forms.Alpha[position])

            let filterLow = filters |> Array.map (fun struct (_, low, _) -> int64 low)
            let filterHigh = filters |> Array.map (fun struct (_, _, high) -> int64 high)

            let filterGamma =
                filters
                |> Array.collect (fun struct (position, _, _) -> forms.Gamma[4 * position .. 4 * position + 3])

            // As the walked value rises by one the seed rises by the inverse,
            // and the first filter's value by a fixed step, so the walk keeps
            // both with an addition and reaches for a multiplication only when
            // the first filter passes, which it rarely does
            let step =
                if Array.isEmpty filters then
                    0L
                else
                    filterAlpha[0] * inverse % P

            // Classes whose forms agree at every constrained position walk the
            // same seeds, so each group is walked once, under its first class,
            // with the classes in it as a mask. The corrections cancel out at
            // some positions, among them position 0, where all four agree
            let groups =
                [| 0..3 |]
                |> Array.groupBy (fun c ->
                    windows
                    |> Array.map (fun struct (position, _, _) -> forms.Gamma[4 * position + c])
                )
                |> Array.map (fun (_, members) -> members[0], members |> Array.sumBy (fun c -> 1 <<< c))

            let chunk = 1L <<< 20
            let chunks = (width walked + chunk - 1L) / chunk

            let walk (job: int) : int64 array =
                let c, members = groups[job % groups.Length]
                let first = int64 walkedLow + int64 (job / groups.Length) * chunk
                let last = min (first + chunk - 1L) (int64 walkedHigh)
                let gamma = forms.Gamma[4 * walkedAt + c]
                let words = Array.zeroCreate<int> 56
                let found = ResizeArray()

                let keep (seed: int) =
                    let own = ClassOf words seed

                    if (members >>> own) &&& 1 = 1 then
                        found.Add((int64 seed <<< 2) ||| int64 own)

                let mutable seed = inverse * ((first - gamma + P) % P) % P

                let mutable firstFilter =
                    if Array.isEmpty filters then
                        0L
                    else
                        (filterAlpha[0] * seed + filterGamma[c]) % P

                let mutable value = first

                while value <= last do
                    if
                        Array.isEmpty filters
                        || (firstFilter >= filterLow[0] && firstFilter <= filterHigh[0])
                    then
                        let mutable consistent = true
                        let mutable filter = 1

                        while consistent && filter < filters.Length do
                            let filtered = (filterAlpha[filter] * seed + filterGamma[4 * filter + c]) % P
                            consistent <- filtered >= filterLow[filter] && filtered <= filterHigh[filter]
                            filter <- filter + 1

                        if consistent then
                            if seed = 0L then
                                keep 0
                                keep System.Int32.MaxValue
                            else
                                keep (int seed)

                    seed <- seed + inverse

                    if seed >= P then
                        seed <- seed - P

                    firstFilter <- firstFilter + step

                    if firstFilter >= P then
                        firstFilter <- firstFilter - P

                    value <- value + 1L

                found.ToArray()

            let found = Array.Parallel.collect walk [| 0 .. groups.Length * int chunks - 1 |]
            Array.sortInPlace found
            found

    /// <summary>
    /// The survivors, as Enumerate gives them, whose System.Random also makes
    /// the calls the constraints describe, in the same order. A survivor's
    /// class is known, so each constraint costs it one multiplication. The
    /// forms must reach every constrained position.
    /// </summary>
    let internal Filter (forms: Affine) (constraints: Constraint seq) (survivors: int64 array) : int64 array =
        match windows constraints with
        | None -> [||]
        | Some windows ->
            survivors
            |> Array.filter (fun survivor ->
                let residue = (survivor >>> 2) % P
                let c = int (survivor &&& 3L)

                windows
                |> Array.forall (fun struct (position, low, high) ->
                    let value = (forms.Alpha[position] * residue + forms.Gamma[4 * position + c]) % P
                    value >= int64 low && value <= int64 high
                )
            )

/// <summary>
/// A call a game makes on its Random. A question, whether to hit or stand or
/// who to aim an action card at, costs one value, which seeds the Random the
/// question is asked with. Drawing a card from Count cards, two or more,
/// shuffles them and takes the head, which costs Count - 1 values, the first of
/// which picks the card: the drawn card's copies sit from Low to High in the
/// deck laid out in Map order, copies of a card side by side. A card drawn from
/// one card costs nothing.
/// </summary>
[<RequireQualifiedAccess>]
type internal Call =
    | Asked
    | Aimed
    | Drawn of Count: int * Low: int * High: int

    /// <summary>
    /// How many values the call takes from the Random.
    /// </summary>
    member self.Values: int =
        match self with
        | Asked
        | Aimed -> 1
        | Drawn(count, _, _) -> count - 1

/// <summary>
/// Works out from a timeline the calls the engine made on the game's Random to
/// produce it, instant by instant, mirroring Timeline's GoonSession,
/// ResolveDraw and ResolveDeal3.
/// </summary>
module internal Interpreter =
    /// <summary>
    /// Where the reading of a timeline stands: the decision scan up to the last
    /// instant read, that instant, which the next one starts from, and how many
    /// values the game's Random has drawn by then.
    /// </summary>
    type internal Reading = { Scan: Observation.Scan; Before: Instant; Position: int }

    /// <summary>
    /// The reading of a game from its TableSeated instant, before which its
    /// Random has drawn nothing.
    /// </summary>
    let internal Start (seated: Instant) : Reading = {
        Scan = fst (Observation.Observe Observation.Start seated)
        Before = seated
        Position = 0
    }

    // Draws a card the way Deck.DrawWith does, shuffling the discards in as the
    // deck once it is empty, which costs nothing. Returns the call the draw
    // makes, if any, and the decks after it, or None when the deck has no copy
    // of the card to draw
    let private Draw ((deck, discards): Deck * Deck) (card: Card) : (Call list * (Deck * Deck)) option =
        let deck, discards =
            if Deck.IsEmpty deck then
                discards, Deck.Empty
            else
                deck, discards

        let below, copies, count =
            deck
            |> Map.fold
                (fun (below, copies, count) other held ->
                    let held = int held

                    if other < card then below + held, copies, count + held
                    elif other = card then below, held, count + held
                    else below, copies, count + held
                )
                (0, 0, 0)

        if copies = 0 then
            None
        else
            let calls =
                if count >= 2 then
                    [ Call.Drawn(count, below, below + copies - 1) ]
                else
                    []

            Some(calls, (Deck.Decrement deck card, discards))

    // The flips of a deal3, card by card as ResolveDeal3 makes them: each a
    // draw from the deck as it stands, number and modifier cards reduced into
    // the target's hand with whatever the reduction removes discarded, a second
    // chance kept, passed on with an aim while somebody can take one, or
    // discarded, and freezes and deal3s set aside in the hand. A passed second
    // chance is silent in the timeline, but whoever takes it, one taker fewer
    // is left. Parked freezes and deal3s neither score, bust nor count toward a
    // flip7, so the hand never needs them taken back out. The flips stop after
    // the third card, on a bust, or on a flip7, and must have used every card
    // the deal3 records. Returns the calls in order and the decks after the
    // flips
    let rec private Flip
        (remaining: int)
        (hand: Hand)
        (takers: int)
        (decks: Deck * Deck)
        (cards: Card list)
        (calls: Call list)
        : (Call list * (Deck * Deck)) option =
        let stopped = remaining = 0 || Hand.HasFlip7Bonus hand

        match cards with
        | [] -> if stopped then Some(calls, decks) else None
        | _ when stopped -> None
        | card :: rest ->
            Draw decks card
            |> Option.bind (fun (drawn, (deck, discards)) ->
                let calls = calls @ drawn

                match card with
                | ActionCard Card.Freeze
                | ActionCard Card.Deal3 -> Flip (remaining - 1) (card :: hand) takers (deck, discards) rest calls
                | ActionCard Card.SecondChance when not (List.contains card hand) ->
                    Flip (remaining - 1) (card :: hand) takers (deck, discards) rest calls
                | ActionCard Card.SecondChance when takers > 0 ->
                    Flip (remaining - 1) hand (takers - 1) (deck, discards) rest (calls @ [ Call.Aimed ])
                | ActionCard Card.SecondChance ->
                    Flip (remaining - 1) hand takers (deck, Deck.Increment discards card) rest calls
                | ValueCard _
                | ModifierCard _ ->
                    let isBust, reduced, removed = Hand.Reduce(card :: hand)
                    let decks = deck, List.fold Deck.Increment discards removed

                    if not isBust then
                        Flip (remaining - 1) reduced takers decks rest calls
                    elif List.isEmpty rest then
                        Some(calls, decks)
                    else
                        None
            )

    /// <summary>
    /// The calls the engine made on the game's Random to produce an instant, in
    /// order, and the reading after it, or None when the engine could not have
    /// produced the instant from the reading. RoundEnded and Flip7Achieved make
    /// no calls, and a later TableSeated is refused: Start reads the one a game
    /// is seated with. The play editor makes an Edited instant in place of
    /// answering a hit-or-stand question, whose call the game's Random had made
    /// by then and cannot take back, so the edit costs that call, and the game
    /// goes on from whatever the edit left, asking the question again with a
    /// call of its own. A hit-or-stand question costs a call before the draw it
    /// leads to, except on a player's first turn of a round, which is a forced
    /// hit and asks nothing; turns are counted from the timeline as Observation
    /// counts them. A drawn second chance passed on, a drawn freeze and a drawn
    /// deal3 are aimed after the draw, and a freeze or deal3 set aside during a
    /// deal3 is aimed with no question and no draw when it resolves. The
    /// set-aside deal3 card goes to the discards before it is aimed, as a drawn
    /// deal3 does after its draw, and then the deal3's flips draw from the deck
    /// as it stands at each one. The deck the instant shows must be the one its
    /// draws leave, and for a deal3 the discards too, which keeps a timeline
    /// the engine did not play from being read into calls it never made.
    /// </summary>
    let internal Read (reading: Reading) (instant: Instant) : (Reading * Call list) option =
        let scan = reading.Scan
        let before = reading.Before
        let decks = before.Deck, before.Discards

        let asked (name: string) : Call list =
            match Observation.TurnsTaken scan |> Map.tryFind name with
            | Some taken when taken > 0u -> [ Call.Asked ]
            | _ -> []

        let hit (name: string) (card: Card) : (Call list * (Deck * Deck)) option =
            Draw decks card |> Option.map (fun (drawn, decks) -> asked name @ drawn, decks)

        let aimed (calls: (Call list * (Deck * Deck)) option) =
            calls |> Option.map (fun (calls, decks) -> calls @ [ Call.Aimed ], decks)

        let read =
            match instant.Event with
            | TableSeated -> None
            | Edited _ -> Some([ Call.Asked ], (instant.Deck, instant.Discards))
            | Flip7Achieved _
            | RoundEnded _ -> Some([], decks)
            // A forced first turn always hits
            | Stood name ->
                match asked name with
                | [] -> None
                | calls -> Some(calls, decks)
            | Drew(name, card)
            | Busted(name, card) -> hit name card
            | SecondChanceDiscarded name -> hit name (ActionCard Card.SecondChance)
            | SecondChancePassed(source, _) -> hit source (ActionCard Card.SecondChance) |> aimed
            | Froze(source, _) when Observation.Owes scan source = 0 -> hit source (ActionCard Card.Freeze) |> aimed
            | Froze _ -> Some([ Call.Aimed ], decks)
            | Dealt3(source, target, cards) ->
                let started =
                    if Observation.Owes scan source = 0 then
                        hit source (ActionCard Card.Deal3)
                    else
                        Some([], decks)

                let dealtTo = before.Players |> List.tryFind (fun player -> player.Name = target)

                // Who could be passed a second chance the target flips but
                // cannot keep: everyone else still in the round without one
                let takers =
                    before.Players
                    |> List.where (fun player ->
                        player.Name <> target
                        && Observation.InRound scan player
                        && not (List.contains (ActionCard Card.SecondChance) player.Hand)
                    )
                    |> List.length

                match started, dealtTo with
                | Some(calls, (deck, discards)), Some dealtTo ->
                    let decks = deck, Deck.Increment discards (ActionCard Card.Deal3)
                    Flip 3 dealtTo.Hand takers decks cards (calls @ [ Call.Aimed ])
                | _ -> None

        read
        |> Option.filter (fun (_, (deck, discards)) ->
            deck = instant.Deck
            && (not instant.Event.IsDealt3 || discards = instant.Discards)
        )
        |> Option.map (fun (calls, _) ->
            let reading' = {
                Scan = fst (Observation.Observe scan instant)
                Before = instant
                Position = reading.Position + List.sumBy (fun (call: Call) -> call.Values) calls
            }

            reading', calls
        )

    /// <summary>
    /// What the draws among calls made from a position on say about the seed.
    /// </summary>
    let internal Constraints (position: int) (calls: Call list) : Seeded.Constraint list =
        calls
        |> List.mapFold
            (fun position (call: Call) ->
                let constrained =
                    match call with
                    | Call.Drawn(count, low, high) ->
                        Some(
                            {
                                Position = position
                                Count = count
                                Low = low
                                High = high
                            }
                            : Seeded.Constraint
                        )
                    | Call.Asked
                    | Call.Aimed -> None

                constrained, position + call.Values
            )
            position
        |> fst
        |> List.choose id

/// <summary>
/// What an oracle has worked out about the seed of the System.Random that dealt
/// a game. Seeds run from 0 to Int32.MaxValue: Random(s) deals the same game as
/// Random(-s), and Random(Int32.MinValue) the same as Random(Int32.MaxValue),
/// so these are every game a seeded Random can deal.
/// </summary>
[<RequireQualifiedAccess>]
type public Seeds =
    /// Too many seeds still agree with the game to list them: about Expected
    | Unlisted of Expected: float
    /// Every seed that still agrees with the game, in ascending order: two or
    /// more, or a lone one while so many seeds could agree by chance that a
    /// game no seed dealt might still show one
    | Agreeing of Seeds: int array
    /// The seed that dealt the game: the only one that agrees, once so few
    /// seeds could agree by chance that no more than one game in a thousand
    /// that no seed dealt ever shows a seed as pinned
    | Pinned of Seed: int
    /// No seed agrees with the game: it was not dealt by a System.Random fresh
    /// from its seed at its TableSeated instant, or the timeline does not start
    /// at one, or the engine could not have played it
    | NoneAgree

// What the oracle knows of the seed: before the seeds are listed, the
// constraints so far; after, every seed left, as Seeded.Enumerate packs them,
// and the affine forms as far as the constraints reach. Either way, how many
// seeds the constraints so far are expected to leave, which once they are
// listed is how many would agree by chance with a game no seed dealt
[<NoComparison; NoEquality>]
type internal Knowledge =
    | Gathering of Constraints: Seeded.Constraint list * Expected: float
    | Listed of Survivors: int64 array * Expected: float * Forms: Seeded.Affine

/// <summary>
/// Seed recovery partway through a timeline: everything an oracle has learnt
/// from the instants it has observed, opaque to callers. It is immutable, so a
/// caller handed the game so far at every question can keep the latest one and
/// observe only the instants since.
/// </summary>
[<NoComparison; NoEquality>]
type public Oracle =
    private
    | Unseen
    | Following of Interpreter.Reading * Knowledge
    | Lost

/// <summary>
/// Recovers the seed of the System.Random a game was dealt with from its
/// timeline alone, instant by instant: neither the lineup's strategies nor
/// their answers are needed, since every question costs the game's Random one
/// call whatever is answered. Once it has pinned the seed, a Random wound to
/// where the game's stands plays the rest of the game again. Like Lookahead, it
/// is .NET only, since it relies on how .NET's seeded System.Random draws.
/// </summary>
module public Oracle =
    // The expected count of agreeing seeds at which they are listed. Listing
    // walks the values the tightest draw allows, which one draw from a nearly
    // full deck already narrows to tens of millions, so it takes much the same
    // time whatever the count: over sixty games, a median of 0.03 to 0.06 s and
    // at most 0.17 s on ten cores for any count from 1e3 to 1e6. What grows
    // with the count is memory, some 40 bytes a seed listed, and the cost of
    // checking the seeds against every later draw. At 1e4 a listing takes 0.2
    // MB at the median and 0.5 MB at most, and the next draw checks its seeds
    // in 0.02 ms, while waiting for 1e3 saves only some 15 ms and lists a draw
    // later. A median game gets there in five draws
    [<Literal>]
    let private Threshold = 1e4

    // How few seeds must be expected to agree by chance for the one seed left
    // to be taken as the game's. A game no seed dealt, on an unseeded Random or
    // on one used before the game, leaves only seeds that agree by chance, and
    // over 600 such games nearly a third showed the last of them alone for a
    // few instants before a draw ruled it out. The chance that any is left once
    // the expected count falls to the margin is at most the margin, and seeds
    // only drop out from there, so at most one such game in a thousand ever
    // shows a seed as pinned, and none of the 600 did. Over 300 seeded games it
    // costs two cards: the true seed is pinned with 11 cards seen at the median
    // and 15 at most, rather than 9 and 13. A margin of 1e-2 would pin a card
    // sooner, and 1e-4 a card later
    [<Literal>]
    let private Margin = 1e-3

    /// <summary>
    /// The oracle before any instant: every seed still agrees.
    /// </summary>
    let public Start: Oracle = Unseen

    // Takes what the draws of an instant say about the seed into account
    let private Learn (knowledge: Knowledge) (constraints: Seeded.Constraint list) : Knowledge =
        let reach (constraints: Seeded.Constraint list) =
            constraints
            |> List.fold (fun reach (constrained: Seeded.Constraint) -> max reach (constrained.Position + 1)) 0

        match knowledge, constraints with
        | _, []
        | Listed([||], _, _), _ -> knowledge
        | Gathering(gathered, _), constraints ->
            let gathered = constraints @ gathered
            let expected = Seeded.Expected gathered

            if expected <= Threshold then
                let forms = Seeded.AffineForms(2 * reach gathered)
                Listed(Seeded.Enumerate forms gathered, expected, forms)
            else
                Gathering(gathered, expected)
        // The affine forms are recomputed at twice the length whenever they
        // fall short, so a whole game's cost no more than twice the last. A
        // later instant's draws are at positions no earlier draw took, so they
        // scale the expected count on their own
        | Listed(survivors, expected, forms), constraints ->
            let reach = reach constraints

            let forms =
                if reach <= forms.Alpha.Length then
                    forms
                else
                    Seeded.AffineForms(max reach (2 * forms.Alpha.Length))

            Listed(
                Seeded.Filter forms constraints survivors,
                expected * Seeded.Expected constraints / 2147483648.0,
                forms
            )

    /// <summary>
    /// Advances the oracle by one instant. The instant and those before it tell
    /// exactly which calls the engine made on the game's Random to produce it,
    /// and every card drawn from two or more says which values the call that
    /// picked it could have returned, which only some seeds give. Until few
    /// enough seeds are expected to agree for them to be listed, the draws are
    /// only gathered, and the step costs a few microseconds; the step that
    /// lists them takes some tens of milliseconds across every core, and each
    /// step after it checks every seed left against the new draws, at a few
    /// nanoseconds a seed. Every draw is checked even once one seed is left,
    /// and a seed that fails one is dropped, which leaves none. The first
    /// instant must be a TableSeated, from which the game's Random is taken to
    /// be fresh from its seed, and an instant the engine could not have
    /// produced from the ones before it leaves no seed.
    /// </summary>
    let public Observe (oracle: Oracle) (instant: Instant) : Oracle =
        match oracle with
        | Unseen when instant.Event.IsTableSeated -> Following(Interpreter.Start instant, Gathering([], 2147483648.0))
        | Unseen
        | Lost -> Lost
        | Following(reading, knowledge) ->
            match Interpreter.Read reading instant with
            | None -> Lost
            | Some(reading', calls) ->
                Following(reading', Learn knowledge (Interpreter.Constraints reading.Position calls))

    /// <summary>
    /// The oracle after observing every one of the instants, from Start.
    /// </summary>
    let public FromInstants (instants: Instant seq) : Oracle = Seq.fold Observe Start instants

    /// <summary>
    /// What the oracle knows of the seed.
    /// </summary>
    let public SeedsOf (oracle: Oracle) : Seeds =
        match oracle with
        | Unseen -> Seeds.Unlisted 2147483648.0
        | Lost -> Seeds.NoneAgree
        | Following(_, Gathering(_, expected)) -> Seeds.Unlisted expected
        | Following(_, Listed(survivors, expected, _)) ->
            match survivors with
            | [||] -> Seeds.NoneAgree
            | [| survivor |] when expected <= Margin -> Seeds.Pinned(Seeded.SeedOf survivor)
            | survivors -> Seeds.Agreeing(Array.map Seeded.SeedOf survivors)

    /// <summary>
    /// How many calls the game's Random has made by the last instant observed,
    /// whatever its seed, or None when the oracle could not follow the timeline
    /// from a TableSeated instant.
    /// </summary>
    let public CallsOf (oracle: Oracle) : int option =
        match oracle with
        | Following(reading, _) -> Some reading.Position
        | Unseen
        | Lost -> None

    /// <summary>
    /// Once the seed is pinned, a System.Random fresh from it and wound on by
    /// every call the game's Random had made by the last instant observed,
    /// which therefore draws what the game's draws next; a new one each time it
    /// is asked for. Given a hit-or-stand question's game so far, that is one
    /// call short of the game's own Random when the question was asked, which
    /// had already drawn the question's seed: Timeline.ContinueWith from the
    /// question, given that history, this Random and the same decider, asks the
    /// question again with the very same draw and replays the rest of the game
    /// exactly. An aim made while a card is still being resolved is no place to
    /// continue from, since the calls of the resolution so far have no instants
    /// yet.
    /// </summary>
    let public RandomOf (oracle: Oracle) : System.Random option =
        match oracle, SeedsOf oracle with
        | Following(reading, _), Seeds.Pinned seed ->
            let random = System.Random seed

            for _ in 1 .. reading.Position do
                random.Next() |> ignore

            Some random
        | _ -> None
