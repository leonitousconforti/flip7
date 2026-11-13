module OracleTests

open System
open System.Numerics
open FSharp.Control
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Flip7.Oracle
open Support
open Games

// Seed recovery rests on facts about .NET's seeded System.Random that were
// proved over every seed by scripts far too slow for a test suite. These
// check a fast sample of them against the real System.Random, then hold the
// interpreter to the engine call by call, then run the oracle over whole
// games.

[<Literal>]
let private P = 2147483647L

// Knuth's constant the generator subtracts the seed from: every seed up to it
// is of class Neither, and the classes vary above it
[<Literal>]
let private MSeed = 161803398

// Every seed from 0 to Int32.MaxValue, 2^31 of them, all of which agree with
// a game before its first draw
let private everySeed = float (1L <<< 31)

// The cards of a full deck, which a round's first draw picks from
let private fullDeck = int (Deck.Count Deck.Full)

// Random(s) deals what Random(-s) does, and Random(Int32.MinValue) what
// Random(Int32.MaxValue) does, so the oracle reports the seed from 0 up
let private canonical (seed: int) : int =
    if seed = Int32.MinValue then Int32.MaxValue else abs seed

let private classOf (seed: int) : KnuthSubtractiveGenerator.Class =
    KnuthSubtractiveGenerator.ClassOf(canonical seed)

// A deck as its cards, in Map order with copies side by side, as the engine
// lays a deck out to shuffle it
let private cardsOf (deck: Deck) : Card list = [
    for card, count in Map.toList deck do
        for _ in 1u .. count -> card
]

// ---- Seeded against the real System.Random ----

// A stretch of a game as the engine deals it: draws from the full deck by the
// real Deck, the first at position 0 as a forced first hit is, with up to
// three single-call questions before each draw after it, and what each draw
// says about the seed, until the draws are expected to leave at most the given
// count of seeds. Also what the next draw would say. The question counts come
// from a Random of their own, seeded by the seed mixed with MurmurHash's
// multiplier, so they vary from seed to seed without being drawn from the
// stream under test
let private dealt
    (seed: int)
    (expected: float)
    : KnuthSubtractiveGenerator.Constraint list * KnuthSubtractiveGenerator.Constraint =
    let random = Recording seed
    let questions = Random(seed ^^^ 0x5bd1e995)
    let constraints = ResizeArray<KnuthSubtractiveGenerator.Constraint>()
    let mutable decks = Deck.Full, Deck.Empty

    let draw () : KnuthSubtractiveGenerator.Constraint =
        if constraints.Count > 0 then
            for _ in 1 .. questions.Next 4 do
                random.Next() |> ignore

        let position = random.Calls
        let layout = cardsOf (fst decks) |> Array.ofList
        let decks', card = Deck.Draw1With random decks
        decks <- decks'

        let struct (low, high) =
            KnuthSubtractiveGenerator.Interval
                layout.Length
                (Array.IndexOf(layout, card))
                (Array.LastIndexOf(layout, card))

        { Position = position; Low = low; High = high }

    while constraints.Count = 0
          || KnuthSubtractiveGenerator.Expected constraints > expected do
        constraints.Add(draw ())

    List.ofSeq constraints, draw ()

// Whether the real System.Random of a seed produces every value the
// constraints describe
let private makes (constraints: KnuthSubtractiveGenerator.Constraint list) (seed: int) : bool =
    let real = Random seed

    let rec check (position: int) (constraints: KnuthSubtractiveGenerator.Constraint list) =
        match constraints with
        | [] -> true
        | constrained :: _ when position < constrained.Position ->
            real.Next() |> ignore
            check (position + 1) constraints
        | constrained :: rest ->
            let value = real.Next()
            value >= constrained.Low
            && value <= constrained.High
            && check (position + 1) rest

    check 0 (constraints |> List.sortBy (fun constrained -> constrained.Position))

// Whether the seeds rise strictly, so that none is listed twice
let private strictlyAscending (seeds: int array) : bool =
    seeds |> Array.pairwise |> Array.forall (fun (seed, next) -> seed < next)

[<Fact>]
let ``every value a seeded Random draws is the affine form of its seed's class`` () =
    let forms = KnuthSubtractiveGenerator.AffineForms 1000
    let random = Random 20261016

    // Seeds either side of a change of class, which comes every few seeds. The
    // search finds the change with classOf itself, so a wrong class still
    // shows: the seeds around it then disagree with the real Random
    let boundaries = [
        for _ in 1..20 do
            let start = random.Next(MSeed + 1, Int32.MaxValue - 1000000)
            let mutable seed = start

            while classOf (seed + 1) = classOf start do
                seed <- seed + 1

            yield! [ seed - 1; seed; seed + 1; seed + 2 ]
    ]

    // The negative seeds and Int32.MinValue are held to the forms of the seed
    // they deal as, which checks that they draw what canonical says they do
    let seeds =
        [
            0
            1
            2
            Int32.MaxValue
            Int32.MaxValue - 1
            Int32.MinValue
            Int32.MinValue + 1
            -1
            -2
            -MSeed
            -(MSeed + 1)
        ]
        @ [ for delta in -3 .. 3 -> MSeed + delta ]
        // The seeds whose overflowing subtraction in the constructor's first
        // mixing pass lands on the edge of [0, Int32.MaxValue), so leaves a
        // word out of range until a later pass brings it back
        @ ([ 614279151; 1235228408; 1235545220; 1333158113 ]
           |> List.collect (fun seed -> [ seed - 1; seed; seed + 1 ]))
        @ boundaries
        @ [ for i in 1..200 -> if i % 4 = 0 then -random.Next() else random.Next() ]

    let disagreeing =
        seeds
        |> List.where (fun seed ->
            let residue = int64 (canonical seed) % P
            let c = classOf seed
            let real = Random seed

            forms
            |> Seq.exists (fun form -> int64 (real.Next()) <> (form.Alpha * residue + form.Gamma c) % P)
        )

    Assert.Empty disagreeing

    // Every class was among the seeds checked
    Assert.Equal(4, seeds |> List.map classOf |> List.distinct |> List.length)

// Enumerate classifies seeds with FastClassOf, which keeps only the words of
// ClassOf's array that the overflowing subtractions read, so the two must agree.
// A script checked them against each other on every seed; this checks a sample
[<Fact>]
let ``FastClassOf finds the class ClassOf does`` () =
    let random = Random 20261112

    let seeds =
        [ 0; 1; Int32.MaxValue - 1; Int32.MaxValue ]
        @ [ for delta in -3 .. 3 -> MSeed + delta ]
        @ ([ 614279151; 1235228408; 1235545220; 1333158113 ]
           |> List.collect (fun seed -> [ seed - 1; seed; seed + 1 ]))
        @ [ for _ in 1..100000 -> random.Next() ]

    Assert.Empty(
        seeds
        |> List.where (fun seed ->
            KnuthSubtractiveGenerator.FastClassOf seed
            <> KnuthSubtractiveGenerator.ClassOf seed
        )
    )

    // Every class was among the seeds checked
    Assert.Equal(
        4,
        seeds
        |> List.map KnuthSubtractiveGenerator.ClassOf
        |> List.distinct
        |> List.length
    )

// The oracle searches only the 2^31 seeds from 0 to Int32.MaxValue, because
// System.Random deals a negative seed as its absolute value and Int32.MinValue
// as Int32.MaxValue. That is .NET's behaviour, not ours, so this holds it to
// it: a runtime that changed it would leave half of all games unsolvable
[<Fact>]
let ``a seed and its negation draw alike, as do Int32.MinValue and Int32.MaxValue`` () =
    let random = Random 7

    let pairs =
        (Int32.MinValue, Int32.MaxValue)
        :: [ for _ in 1..50 -> let seed = random.Next() in seed, -seed ]

    for seed, twin in pairs do
        let a, b = Random seed, Random twin
        Assert.True(
            Seq.init 500 id |> Seq.forall (fun _ -> a.Next() = b.Next()),
            $"Random {seed} and Random {twin} differ"
        )

[<Fact>]
let ``Next(0, n) changes index exactly where Interval says, for every n up to the full deck`` () =
    let forms = KnuthSubtractiveGenerator.AffineForms 64

    // Int32.MaxValue is prime, so alpha to the power Int32.MaxValue - 2 is
    // alpha's inverse
    let inverses =
        forms
        |> Array.map (fun form -> int64 (BigInteger.ModPow(bigint form.Alpha, bigint (P - 2L), bigint P)))

    // A seed whose real System.Random draws the value at some position, solved
    // from the affine forms and confirmed by the real System.Random
    let drawing (value: int) : int * int =
        Seq.init 64 id
        |> Seq.pick (fun k ->
            List.ofArray KnuthSubtractiveGenerator.Class.All
            |> List.collect (fun c ->
                let residue = inverses[k] * ((int64 value - forms[k].Gamma c + P) % P) % P
                if residue = 0L then
                    [ 0; Int32.MaxValue ]
                else
                    [ int residue ]
            )
            |> List.tryFind (fun seed ->
                let real = Random seed

                for _ in 1..k do
                    real.Next() |> ignore

                real.Next() = value
            )
            |> Option.map (fun seed -> seed, k)
        )

    let indexAt (seed: int, k: int) (n: int) : int =
        let real = Random seed

        for _ in 1..k do
            real.Next() |> ignore

        real.Next(0, n)

    // Every n's indices together take every value Next() returns
    Assert.Empty [
        for n in 1..fullDeck do
            if KnuthSubtractiveGenerator.Interval n 0 (n - 1) <> struct (0, Int32.MaxValue - 1) then
                n
    ]

    // Each index's values follow straight on from the index below's, and the
    // real Random turns the first of them into the index and the value before
    // it into the index below
    let disagreeing = [
        for n in 1..fullDeck do
            for index in 1 .. n - 1 do
                let struct (low, _) = KnuthSubtractiveGenerator.Interval n index index
                let struct (_, high) = KnuthSubtractiveGenerator.Interval n (index - 1) (index - 1)

                if
                    low - 1 <> high
                    || indexAt (drawing low) n <> index
                    || indexAt (drawing (low - 1)) n <> index - 1
                then
                    n, index
    ]

    Assert.Empty disagreeing

[<Fact>]
let ``Enumerate lists exactly the seeds whose Random makes the calls`` () =
    // The draws stop once they are expected to leave at most 5e5 seeds, which
    // is enough for every window below to hold some
    let seed = 1040319434
    let constraints, _ = dealt seed 5e5
    let forms = KnuthSubtractiveGenerator.AffineForms 1000
    let found =
        KnuthSubtractiveGenerator.Enumerate forms constraints
        |> Array.map (fun struct (seed, _) -> seed)

    // Windows of seeds checked by brute force: at the bottom of the range,
    // where every seed is of class Neither, at the top, and around the seed,
    // where the draws leave seeds of every class. Each is narrow enough to try
    // every seed in it quickly
    let window = 1 <<< 18

    let survivors = [
        for first in [ 0; Int32.MaxValue - window + 1; seed - window / 2 ] do
            let last = first + (window - 1)

            let brute =
                [| first..last |]
                |> Array.Parallel.choose (fun seed -> if makes constraints seed then Some seed else None)

            Assert.NotEmpty brute
            Assert.Equal<int array>(brute, found |> Array.where (fun seed -> seed >= first && seed <= last))
            yield! brute
    ]

    Assert.True(
        (survivors |> List.map classOf |> List.distinct |> List.length) = 4,
        "the windows no longer hold a seed of every class: pick a new seed"
    )

[<Fact>]
let ``Enumerate keeps the true seed, which Filter keeps through the next draw`` () =
    let seeds = [ 0; Int32.MaxValue; 1235545220; 1040319434; 1000000000 ]

    // One true seed of every class, and the two that share residue 0
    Assert.Equal<KnuthSubtractiveGenerator.Class list>(
        [
            KnuthSubtractiveGenerator.Neither
            KnuthSubtractiveGenerator.Both
            KnuthSubtractiveGenerator.Both
            KnuthSubtractiveGenerator.Word24Short
            KnuthSubtractiveGenerator.Word55Over
        ],
        seeds |> List.map classOf
    )

    let forms = KnuthSubtractiveGenerator.AffineForms 1000

    for seed in seeds do
        let constraints, next = dealt seed 1e4
        let listed = KnuthSubtractiveGenerator.Enumerate forms constraints
        let found = listed |> Array.map (fun struct (seed, _) -> seed)

        Assert.Contains(canonical seed, found)
        Assert.True(
            found |> Array.forall (makes constraints),
            $"seed {seed} listed a seed that does not make the calls"
        )
        Assert.True(strictlyAscending found, $"seed {seed} listed a seed twice or out of order")

        let filtered =
            KnuthSubtractiveGenerator.Filter forms [ next ] listed
            |> Array.map (fun struct (seed, _) -> seed)
        Assert.Equal<int array>(found |> Array.where (makes (next :: constraints)), filtered)

// At some positions the corrections cancel out and the classes' constants
// agree, all four of them at some. A game's draws seldom all fall there, but
// when they do, Enumerate walks a group of classes under the first class in
// it and must still keep the seeds of the others, each with its own class,
// by which later draws, where the constants differ, judge it
[<Fact>]
let ``Enumerate keeps every class of a group whose forms agree, each with its own class`` () =
    let forms = KnuthSubtractiveGenerator.AffineForms 3000

    let distinctConstants (k: int) =
        [ for c in KnuthSubtractiveGenerator.Class.All -> forms[k].Gamma c ]
        |> List.distinct
        |> List.length

    // Positions where classes agree come early in the stream, so the first
    // forty hold every kind needed: four at which all four classes agree, one
    // at which they split in two and one at which all differ
    let agreeing =
        [ 0..40 ] |> List.where (fun k -> distinctConstants k = 1) |> List.take 4

    let split = [ 0..40 ] |> List.find (fun k -> distinctConstants k = 2)
    let differing = [ 0..40 ] |> List.find (fun k -> distinctConstants k = 4)

    // The later draws are taken far along the forms as well, as a game's later
    // draws are, where every class has a constant of its own
    let later = [ differing; 2000; 2500 ]
    Assert.True(later |> List.forall (fun k -> distinctConstants k = 4), "a later draw sits where classes agree")

    // Exact draws of one card from a full deck by a seed's Random at the
    // positions. Four of them leave about 2^31 / 94^4 seeds, some 27, so the
    // true seed is listed among a few others
    let drawn (seed: int) (positions: int list) : KnuthSubtractiveGenerator.Constraint list = [
        let real = Random seed

        for k in 0 .. List.max positions do
            if List.contains k positions then
                let index = real.Next(0, fullDeck)
                let struct (low, high) = KnuthSubtractiveGenerator.Interval fullDeck index index
                yield { Position = k; Low = low; High = high }
            else
                real.Next() |> ignore
    ]

    // Two seeds of class Both, Int32.MaxValue among them, which shares its
    // residue with 0, and one of class Word24Short
    let seeds = [ Int32.MaxValue; 1235545220; 1040319434 ]

    Assert.Equal<KnuthSubtractiveGenerator.Class list>(
        [
            KnuthSubtractiveGenerator.Both
            KnuthSubtractiveGenerator.Both
            KnuthSubtractiveGenerator.Word24Short
        ],
        seeds |> List.map classOf
    )

    for seed in seeds do
        for positions in [ agreeing; split :: agreeing ] do
            let constraints = drawn seed positions

            // The walk really merges classes
            let groups =
                KnuthSubtractiveGenerator.Class.All
                |> Array.distinctBy (fun c -> positions |> List.map (fun k -> forms[k].Gamma c))

            Assert.True(groups.Length < 4, $"no classes agree at every one of positions %A{positions}")

            let listed = KnuthSubtractiveGenerator.Enumerate forms constraints
            let found = listed |> Array.map (fun struct (seed, _) -> seed)

            Assert.Contains(seed, found)

            Assert.True(
                listed |> Array.forall (fun struct (seed, class') -> class' = classOf seed),
                "a seed was listed with the class of the group it was walked under rather than its own"
            )

            Assert.True(found |> Array.forall (makes constraints), "a seed was listed that does not make the calls")

            let laterDrawn = drawn seed later
            let kept =
                KnuthSubtractiveGenerator.Filter forms laterDrawn listed
                |> Array.map (fun struct (seed, _) -> seed)
            Assert.Contains(seed, kept)
            Assert.Equal<int array>(found |> Array.where (makes (laterDrawn @ constraints)), kept)

// Enumerate walks the values the tightest draw allows a chunk at a time,
// across every core. A draw of index 0 from 1500 cards, far more than a deck
// holds, allows about 2^31 / 1500 values, more than one chunk holds but few
// enough to list quickly. It is at position 0, where all four classes agree,
// so they are walked as one group and each value once
[<Fact>]
let ``Enumerate walks every value of a window wider than a chunk exactly once`` () =
    let forms = KnuthSubtractiveGenerator.AffineForms 1
    let struct (low, high) = KnuthSubtractiveGenerator.Interval 1500 0 0
    let first: KnuthSubtractiveGenerator.Constraint = { Position = 0; Low = low; High = high }

    let found =
        KnuthSubtractiveGenerator.Enumerate forms [ first ]
        |> Array.map (fun struct (seed, _) -> seed)

    // Each value pins one residue, and so one seed, since the residue 0, which
    // stands for both 0 and Int32.MaxValue, takes none of them
    Assert.False(makes [ first ] 0)

    Assert.Equal(high - low + 1, found.Length)
    Assert.True(strictlyAscending found, "a seed was listed twice or out of order")

[<Fact>]
let ``contradictory constraints leave no seed, and ones every seed meets cannot be listed`` () =
    let forms = KnuthSubtractiveGenerator.AffineForms 10
    let small: KnuthSubtractiveGenerator.Constraint = { Position = 0; Low = 0; High = 999 }
    let contradicted = [ small; { small with Low = 1000; High = 1999 } ]

    Assert.Empty(KnuthSubtractiveGenerator.Enumerate forms contradicted)
    Assert.Equal(0.0, KnuthSubtractiveGenerator.Expected contradicted)

    let everything = [ { small with High = Int32.MaxValue - 1 } ]
    Assert.Equal(everySeed, KnuthSubtractiveGenerator.Expected everything)
    Assert.Throws<ArgumentException>(fun () -> KnuthSubtractiveGenerator.Enumerate forms everything |> ignore)
    |> ignore

// The stream's values run from 0 to Int32.MaxValue - 1, so a constraint
// reaching past them is a mistake, refused rather than read as allowing fewer
[<Fact>]
let ``constraints on values the stream never produces are refused`` () =
    let refused: KnuthSubtractiveGenerator.Constraint list = [
        { Position = 0; Low = -1; High = 10 }
        { Position = 0; Low = 0; High = Int32.MaxValue }
        { Position = -1; Low = 0; High = 10 }
    ]

    for constrained in refused do
        Assert.Throws<ArgumentException>(fun () -> KnuthSubtractiveGenerator.Expected [ constrained ] |> ignore)
        |> ignore

// ---- the interpreter against the engine ----

// What the interpreter, fed only the timeline, says each instant cost the
// game's Random: per instant, each step the engine took, with the position of
// the first value it took. Error with the first instant it could not read
let private interpreted (timeline: Instant list) : Result<(int * Observation.Step) list list, int> =
    let rec read (scan: Observation.Scan) (position: int) (index: int) (instants: Instant list) acc =
        match instants with
        | [] -> Ok(List.rev acc)
        | instant :: rest ->
            let scan' = fst (Observation.Observe scan instant)

            match Observation.ReplayOf scan' with
            | Some replay when replay.Possible ->
                let positioned, position' =
                    replay.Steps
                    |> List.mapFold (fun position step -> (position, step), position + Interpreter.Values step) position

                read scan' position' (index + 1) rest (positioned :: acc)
            | _ -> Error index

    match timeline with
    | [] -> Ok []
    | seated :: rest -> read (fst (Observation.Observe Observation.Start seated)) 0 1 rest [ [] ]

// The calls a Recording logged, a whole draw at a time: a shuffle of n cards is
// Next(i, n) for i from 0 to n - 2, all for the same instant, and the first
// one's value picks the card. Error with the position of the first call that
// does not fit
let private logged (made: Made list) : Result<(int * int * Made) list, int> =
    let made = Array.ofList made

    let rec parse (position: int) acc =
        if position = made.Length then
            Ok(List.rev acc)
        else
            match made[position] with
            | Questioned instant
            | AimedAt instant as question -> parse (position + 1) ((instant, position, question) :: acc)
            | Shuffled(instant, 0, count, _) as first when
                position + count - 1 <= made.Length
                && Seq.init (count - 2) ((+) 1)
                   |> Seq.forall (fun from ->
                       match made[position + from] with
                       | Shuffled(instant', from', count', _) -> instant' = instant && from' = from && count' = count
                       | _ -> false
                   )
                ->
                parse (position + count - 1) ((instant, position, first) :: acc)
            | _ -> Error position

    parse 0 []

// A step as a failure shows it, without the observation or the pile it
// carries
let private described (step: Observation.Step) : string =
    match step with
    | Observation.Step.Asked _ -> "a question"
    | Observation.Step.Interrupted _ -> "a question an edit was made at"
    | Observation.Step.Aimed _ -> "an aim"
    | Observation.Step.Drawn(card, pile) ->
        let count = pile |> Map.fold (fun count _ held -> count + int held) 0
        $"{card} drawn from {count} cards"

// Where the interpreter, fed only the timeline, disagrees with the calls the
// Recording logged, if anywhere. Every step but a card drawn from a single
// card makes a call, and they agree when they are the same calls at the same
// positions for the same instants, each question of the same kind, and each
// draw a shuffle of the pile it came from, laid out in Map order, whose index
// lands on a copy of the card drawn
let private disagreement (timeline: Instant list) (made: Made list) : string option =
    match interpreted timeline, logged made with
    | Error index, _ -> Some $"the interpreter could not read instant {index}: {timeline[index].Event}"
    | _, Error position -> Some $"the Recording logged a stray call at {position}: {made[position]}"
    | Ok perInstant, Ok observed ->
        let predicted =
            perInstant
            |> List.indexed
            |> List.collect (fun (index, steps) ->
                steps
                |> List.where (fun (_, step) -> Interpreter.Values step > 0)
                |> List.map (fun (position, step) -> index, position, step)
            )

        let agree (index, position, step) (index', position', made) =
            index = index'
            && position = position'
            && match step, made with
               | Observation.Step.Asked _, Questioned _
               | Observation.Step.Interrupted _, Questioned _
               | Observation.Step.Aimed _, AimedAt _ -> true
               | Observation.Step.Drawn(card, pile), Shuffled(_, _, count, value) ->
                   let laidOut =
                       pile
                       |> Map.toArray
                       |> Array.collect (fun (card, held) -> Array.replicate (int held) card)

                   laidOut.Length = count && laidOut[value] = card
               | _ -> false

        match
            Seq.zip predicted observed
            |> Seq.tryFind (fun (predicted, observed) -> not (agree predicted observed))
        with
        | Some((index, position, step), observed) ->
            Some
                $"the interpreter predicted {described step} at {position} for instant {index} where the engine made %A{observed}"
        | None when predicted.Length <> observed.Length ->
            Some $"the interpreter predicted {predicted.Length} calls where the engine made {observed.Length}"
        | None -> None

// How many times a game went down the rarer roads through a deal3, as the
// interpreter reads them off the timeline
let private roadsOf (timeline: Instant list) : Roads =
    match interpreted timeline with
    | Error _ -> noRoads
    | Ok perInstant ->
        let count (condition: bool) = if condition then 1 else 0

        List.zip3 (List.take (timeline.Length - 1) timeline) (List.tail timeline) (List.tail perInstant)
        |> List.fold
            (fun roads (before, instant, steps) ->
                let steps = List.map snd steps

                match instant.Event, steps with
                | Froze _, [ Observation.Step.Aimed _ ] -> { roads with SetAsideFreezes = roads.SetAsideFreezes + 1 }
                | Dealt3(_, target, cards, _), first :: _ ->
                    let setAside = first.IsAimed

                    // A drawn deal3 that finds the deck empty reshuffles
                    // before its flips, a set-aside one at its first flip
                    let reshuffled =
                        (setAside || not (Deck.IsEmpty before.Deck))
                        && Deck.Count instant.Deck + bigint cards.Length > Deck.Count before.Deck

                    // The second chances flipped to a target already holding
                    // one, and how many were aimed at somebody else
                    let unkeepable, _ =
                        cards
                        |> List.fold
                            (fun (unkeepable, hand) card ->
                                match card with
                                | ActionCard Card.SecondChance when List.contains card hand -> unkeepable + 1, hand
                                | ActionCard _ -> unkeepable, card :: hand
                                | _ ->
                                    let _, reduced, _ = Hand.Reduce(card :: hand)
                                    unkeepable, reduced
                            )
                            (0, (before.Players |> List.find (fun player -> player.Name = target)).Hand)

                    let passed = (steps |> List.where _.IsAimed |> List.length) - 1

                    let flipped7 =
                        cards.Length < 3
                        && instant.Players
                           |> List.exists (fun player -> player.Name = target && Hand.HasFlip7Bonus player.Hand)

                    {
                        roads with
                            SetAsideDeal3s = roads.SetAsideDeal3s + count setAside
                            PassedInFlips = roads.PassedInFlips + count (passed > 0)
                            PassedTwiceInFlips = roads.PassedTwiceInFlips + count (passed > 1)
                            CutShortByFlip7 = roads.CutShortByFlip7 + count flipped7
                            ReshuffledInFlips = roads.ReshuffledInFlips + count reshuffled
                            ReshuffledInSetAside = roads.ReshuffledInSetAside + count (setAside && reshuffled)
                            DiscardedInFlips = roads.DiscardedInFlips + count (passed < unkeepable)
                            RanOutOfTakers = roads.RanOutOfTakers + count (passed > 0 && passed < unkeepable)
                    }
                | _ -> roads
            )
            noRoads

[<Fact>]
let ``the interpreter predicts every call the engine makes on the game's Random`` () =
    let roads = ResizeArray<Roads>()
    let mutable edits = 0
    let mutable continuations = 0

    let property =
        Prop.forAll
            (Arb.fromGen genGameWithFork)
            (fun (seed, lineup, fork) ->
                let timeline, made, forked = playForked id seed lineup (Option.map editing fork)

                roads.Add(roadsOf timeline)

                if forked && fork |> Option.exists (fun fork -> fork.Edit.IsSome) then
                    edits <- edits + 1
                elif forked then
                    continuations <- continuations + 1

                match disagreement timeline made with
                | None -> Prop.ofTestable true
                | Some reason -> Prop.label reason false
            )

    // A hundred games take each common road below many times over and still
    // play out quickly
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

    // The games went down the common roads and continued after edits and
    // without them. The test below takes the rarer roads every time
    let taken (road: Roads -> int) = roads |> Seq.sumBy road

    Assert.True(taken _.SetAsideFreezes > 0, "no freeze was set aside during a deal3's flips")
    Assert.True(taken _.SetAsideDeal3s > 0, "no deal3 was set aside during a deal3's flips")
    Assert.True(taken _.CutShortByFlip7 > 0, "no deal3's flips were cut short by a flip7")
    Assert.True(taken _.ReshuffledInFlips > 0, "no deck ran out during a deal3's flips")
    Assert.True(edits > 0, "no game was continued after an edit")
    Assert.True(continuations > 0, "no game was continued without an edit")

// Seed 22 runs the deck out during the flips of a set-aside deal3, whose card
// the discards hold by then, seed 17 flips a second chance that nobody can
// take, seed 8164 flips two in one deal3 with one taker for them, passing the
// first on, and seed 1266 passes two on during one deal3's flips, each aim
// between the flips around it. Only a few games in a hundred go the first two
// ways or pass a second chance on during the flips at all, and one in
// thousands the last two, so the property above can miss them in a run, and
// these seeds take them every time
[<Fact>]
let ``the interpreter follows a deal3 down its rarest roads`` () =
    let followed (seed: int) =
        let timeline, made, _ = playForked id seed rarestRoadsLineup None
        Assert.Equal(None, disagreement timeline made)
        roadsOf timeline

    // A change to how the game draws can move a seed off its road
    Assert.True(
        (followed 22).ReshuffledInSetAside > 0,
        "seed 22 no longer runs the deck out during a set-aside deal3's flips: pick a new seed"
    )

    Assert.True(
        (followed 17).DiscardedInFlips > 0,
        "seed 17 no longer flips a second chance nobody can take: pick a new seed"
    )

    let ranOut = followed 8164

    Assert.True(
        ranOut.PassedInFlips > 0 && ranOut.RanOutOfTakers > 0,
        "seed 8164 no longer passes a second chance on to the last taker for another: pick a new seed"
    )

    Assert.True(
        (followed 1266).PassedTwiceInFlips > 0,
        "seed 1266 no longer passes two second chances on during one deal3's flips: pick a new seed"
    )

// The play editor can take the busting card back out of a busted player's
// hand, and the engine still counts them finished, so they cannot be passed a
// second chance. Seed 3 plays two players who always hit until one busts and
// the other is asked whether to hit. An edit there un-busts the one, gives the
// other a second chance and leaves the deck one deal3 and one second chance,
// so a deal3 soon flips a second chance that nobody can take
[<Fact>]
let ``the interpreter passes no second chance to a player an edit un-busted`` () =
    let seed = 3

    let lineup = [
        "Alice", Strategy.AlwaysHits, Targeting.ChoosesRandomly
        "Bob", Strategy.AlwaysHits, Targeting.ChoosesRandomly
    ]

    // The players still in the round besides the one asked, and those finished,
    // at each hit-or-stand question the game asks
    let questions = ResizeArray<Player list * Player list>()

    let noting =
        own
        |> beforeEachQuestion (fun _ _ question ->
            match question with
            | HitOrStandQuestion(Others = others; Finished = finished) -> questions.Add(others, finished)
            | TargetQuestion _ -> ()
        )

    Timeline.SimulateWithDecider (Random seed) noting lineup |> played |> ignore

    let question =
        questions
        |> Seq.findIndex (fun (others, finished) ->
            List.isEmpty others
            && finished |> List.exists (fun player -> Hand.IsBust player.Hand)
        )

    let secondChance = ActionCard Card.SecondChance
    let deal3 = ActionCard Card.Deal3

    let unbusting ((active, finished, (deck, discards)): Table) : Table =
        let busted = finished |> List.find (fun player -> Hand.IsBust player.Hand)
        let asked = List.head active

        let given =
            if List.contains secondChance asked.Hand then
                []
            else
                [ secondChance ]

        // The busting card and the deck go to the discards, and the second
        // chance given and the new deck come out of them
        let pool =
            List.head busted.Hand :: cardsOf deck |> List.fold Deck.Increment discards

        Assert.True(
            Map.find deal3 pool >= 1u
            && Map.find secondChance pool >= 1u + uint given.Length,
            "the cards out of play lack the deal3 or the second chances the edit deals: pick a new seed"
        )

        { asked with Hand = given @ asked.Hand } :: List.tail active,
        finished
        |> List.map (fun player ->
            if player.Name = busted.Name then
                { player with Hand = List.tail player.Hand }
            else
                player
        ),
        (deckOf [ deal3; secondChance ], deal3 :: secondChance :: given |> List.fold Deck.Decrement pool)

    let timeline, made, forked =
        playForked id seed lineup (Some { AtQuestion = question; Edit = Some unbusting })

    Assert.True(forked, "the game ended before the question it was to be edited at")
    Assert.Equal(None, disagreement timeline made)

    // A change to how the game draws can move the seed off its road
    let edit = timeline |> List.findIndex (fun instant -> instant.Event.IsEdited)

    Assert.True(
        (roadsOf timeline).DiscardedInFlips > (roadsOf timeline[..edit]).DiscardedInFlips,
        "no second chance was discarded during a deal3's flips after the edit: pick a new seed"
    )

// ---- the oracle end to end ----

// Alice plays like a human and aims spitefully, Bob maximizes his expected
// value and aims greedily, and Carol answers every question with randomness of
// her own
let private mixedTable: Lineup = [
    "Alice", Strategy.PlaysLikeAHuman 20u, Targeting.PlaysSpitefully
    "Bob", Strategy.MaximizesExpectedValue, Targeting.PlaysGreedily(0.8, 0.2)
    "Carol", Strategy.Custom "Own", Targeting.ChoosesExternally "Own"
]

let private twoSeats: Lineup = [
    "Alice", Strategy.RandomWithProbability 0.6, Targeting.ChoosesRandomly
    "Bob", Strategy.HitUntilScore 25u, Targeting.ChoosesExternally "Own"
]

let private sixHitters: Lineup = [
    for name in List.take 6 seatNames -> name, Strategy.AlwaysHits, Targeting.ChoosesRandomly
]

// Every third seat answers with randomness of its own, and the rest hit up to
// rising scores and aim spitefully
let private twelveSeats: Lineup = [
    for index, name in List.indexed seatNames ->
        if index % 3 = 0 then
            name, Strategy.Custom "Own", Targeting.ChoosesExternally "Own"
        else
            name, Strategy.HitUntilScore(uint (15 + index)), Targeting.PlaysSpitefully
]

let private lineups: Map<string, Lineup> =
    Map [
        "mixed", mixedTable
        "two seats", twoSeats
        "six hitters", sixHitters
        "twelve seats", twelveSeats
    ]

// Where a game stood at a hit-or-stand question: how much of the game the
// question was shown, the round, the players still in it with the one asked
// first, those finished, and the decks
type private Paused = {
    Shown: int
    Round: uint
    Active: Player list
    Finished: Player list
    Decks: Deck * Deck
}

[<Theory>]
// A date, as people pick seeds
[<InlineData(20261016, "mixed")>]
// A negative seed, reported as its negation
[<InlineData(-12345, "two seats")>]
// Int32.MinValue, reported as Int32.MaxValue, which shares its residue with 0
[<InlineData(Int32.MinValue, "six hitters")>]
// A seed of class Word55Over, which no other row's is
[<InlineData(1000000000, "twelve seats")>]
let ``the oracle pins the true seed and its Random plays the rest of the game again`` (seed: int, lineup: string) =
    let game = Recording seed
    let paused = ResizeArray<Paused>()

    let noting =
        own
        |> beforeEachQuestion (fun _ history question ->
            match question with
            | HitOrStandQuestion(_, round, _, player, others, finished, decks) ->
                paused.Add {
                    Shown = Seq.length history
                    Round = round
                    Active = player :: others
                    Finished = finished
                    Decks = decks
                }
            | TargetQuestion _ -> ()
        )

    let timeline =
        Timeline.SimulateWithDecider game noting lineups[lineup]
        |> AsyncSeq.toArrayAsync
        |> Async.RunSynchronously

    let oracles = timeline |> Array.scan Oracle.Observe Oracle.Start |> Array.tail

    // The true seed agrees after every instant, and is all that is left by the
    // end, with every call of the game accounted for. Seeds are listed only
    // once few enough are expected to agree, so a listing never comes near the
    // tens of millions one at the first draw would hold, and the bound leaves
    // room to retune when listing starts
    for oracle in oracles do
        match Oracle.SeedsOf oracle with
        | Seeds.Unlisted _ -> ()
        | Seeds.Agreeing seeds ->
            Assert.Contains(canonical seed, seeds)
            Assert.True(seeds.Length < 10_000_000, $"{seeds.Length} seeds listed")
        | Seeds.Pinned pinned -> Assert.Equal(canonical seed, pinned)
        | Seeds.NoneAgree -> Assert.Fail "the true seed was dropped"

    Assert.Equal(Seeds.Pinned(canonical seed), Oracle.SeedsOf(Array.last oracles))
    Assert.Equal(Some game.Calls, Oracle.CallsOf(Array.last oracles))

    // From the first hit-or-stand question once the seed is pinned, the game
    // plays out again exactly, given what led to the question
    let pinned =
        oracles |> Array.findIndex (fun oracle -> (Oracle.SeedsOf oracle).IsPinned)

    let at = paused |> Seq.find (fun at -> at.Shown > pinned)
    let history = List.ofArray timeline[.. at.Shown - 1]
    let random = Oracle.RandomOf oracles[at.Shown - 1]
    Assert.True(random.IsSome, "the pinned oracle handed out no Random")

    let replayed =
        Timeline.ContinueWith
            random.Value
            own
            history
            (timeline[0].Players |> List.map (fun player -> player.Name))
            at.Round
            (Observation.TurnsTaken(scanOf history))
            at.Active
            at.Finished
            at.Decks
        |> played

    Assert.Equal<Instant list>(List.ofArray timeline[at.Shown ..], replayed)

// A game no seed dealt leaves only seeds that agree with it by chance, and the
// last of them is often alone for a few instants before a draw rules it out,
// as it is in these games on Randoms used once before the game
[<Fact>]
let ``a lone seed agreeing by chance is never pinned nor handed out as a Random`` () =
    let usedOnce (seed: int) =
        let random = Random seed
        random.Next() |> ignore
        random

    let mutable alone = 0

    for seed in 10..12 do
        let oracles =
            Timeline.SimulateWithDecider (usedOnce seed) own mixedTable
            |> played
            |> List.scan Oracle.Observe Oracle.Start

        for oracle in oracles do
            match Oracle.SeedsOf oracle with
            | Seeds.Pinned pinned -> Assert.Fail $"seed {pinned} was pinned by chance"
            | Seeds.Agreeing [| _ |] -> alone <- alone + 1
            | _ -> ()

            Assert.True((Oracle.RandomOf oracle).IsNone, "an oracle that had not pinned the seed handed out a Random")

        let oracle = List.last oracles
        Assert.Equal(Seeds.NoneAgree, Oracle.SeedsOf oracle)
        Assert.True((Oracle.CallsOf oracle).IsSome, "the oracle stopped following a game the engine played")

    Assert.True(alone > 0, "no seed was ever left alone by chance in these games: pick a new seed")

// The interpreter reads an instant only where its decks are what the draws it
// predicts leave and a deal3 records just the passes its flips make, so a
// timeline the engine did not play is read only up to the instant that shows
// otherwise, and leaves no seed from there: here one Drew is shown with a card
// missing from its deck, one Dealt3 with a card too many in its discards,
// though each shows the card that was drawn, and the same Dealt3 recording a
// second chance passed on to its source on top of whatever its flips passed on
[<Fact>]
let ``a timeline whose decks the draws could not have left, or whose deal3 records a pass it did not make, is not read past them``
    ()
    =
    let timeline = Timeline.SimulateWithDecider (Random 12345) own mixedTable |> played

    let anyCardIn (deck: Deck) =
        deck |> Map.findKey (fun _ count -> count > 0u)

    let drew = timeline |> List.findIndex (fun instant -> instant.Event.IsDrew)
    let dealt3 = timeline |> List.findIndex (fun instant -> instant.Event.IsDealt3)

    let alterations = [
        drew,
        {
            timeline[drew] with
                Deck = Deck.Decrement timeline[drew].Deck (anyCardIn timeline[drew].Deck)
        }
        dealt3,
        {
            timeline[dealt3] with
                Discards = Deck.Increment timeline[dealt3].Discards (anyCardIn timeline[dealt3].Deck)
        }
        dealt3,
        {
            timeline[dealt3] with
                Event =
                    match timeline[dealt3].Event with
                    | Dealt3(source, target, cards, passed) -> Dealt3(source, target, cards, passed @ [ source ])
                    | event -> event
        }
    ]

    Assert.True((interpreted timeline).IsOk, "the interpreter could not read the game as the engine played it")

    for index, altered in alterations do
        let tampered = List.updateAt index altered timeline
        Assert.Equal(Error index, interpreted tampered |> Result.map ignore)

        let oracle = Oracle.FromInstants(List.take (index + 1) tampered)
        Assert.Equal(Seeds.NoneAgree, Oracle.SeedsOf oracle)
        Assert.True((Oracle.CallsOf oracle).IsNone, "the oracle still follows a timeline it could not read")

// The engine seats every player under a name of their own, and the replay
// finds them by name, so a table that seats two under one is not read: one
// seated so leaves no seed from the start, and an instant that shows one is
// not read past, though its draws leave just the decks it shows
[<Fact>]
let ``a table that seats two players under one name is not read`` () =
    let timeline = Timeline.SimulateWithDecider (Random 1) own twoSeats |> played

    let twins (instant: Instant) = {
        instant with
            Players = instant.Players |> List.map (fun player -> { player with Name = "Twin" })
    }

    let seated = Oracle.FromInstants [ twins (List.head timeline) ]
    Assert.Equal(Seeds.NoneAgree, Oracle.SeedsOf seated)
    Assert.True((Oracle.CallsOf seated).IsNone, "the oracle followed a table seating two players under one name")

    let drew = timeline |> List.findIndex (fun instant -> instant.Event.IsDrew)
    Assert.True((interpreted timeline).IsOk, "the interpreter could not read the game as the engine played it")
    Assert.Equal(
        Error drew,
        interpreted (List.updateAt drew (twins timeline[drew]) timeline)
        |> Result.map ignore
    )

[<Fact>]
let ``before any instant every seed agrees, seating draws nothing, and a timeline that skips its TableSeated is not followed``
    ()
    =
    Assert.Equal(Seeds.Unlisted everySeed, Oracle.SeedsOf Oracle.Start)
    Assert.True((Oracle.CallsOf Oracle.Start).IsNone, "the oracle counted calls before any instant")

    let timeline = Timeline.SimulateWithDecider (Random 1) own twoSeats |> played

    let seated = Oracle.FromInstants [ List.head timeline ]
    Assert.Equal(Seeds.Unlisted everySeed, Oracle.SeedsOf seated)
    Assert.Equal(Some 0, Oracle.CallsOf seated)

    // The oracle takes the game's Random to be fresh from its seed at the
    // TableSeated, so without one it cannot tell how many calls came before
    let unseated = Oracle.FromInstants(List.tail timeline)
    Assert.Equal(Seeds.NoneAgree, Oracle.SeedsOf unseated)
    Assert.True((Oracle.CallsOf unseated).IsNone, "the oracle followed a timeline that skips its TableSeated")
