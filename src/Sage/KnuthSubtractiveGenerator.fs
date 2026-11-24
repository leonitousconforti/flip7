/// <summary>
/// Recovers the seed of a seeded System.Random from what its calls returned.
///
/// A seeded System.Random is .NET's legacy subtractive generator, and every
/// value it yields is affine in the seed modulo Int32.MaxValue, a Mersenne
/// prime, up to one of four corrections. So a call of Next(0, n) that returned
/// a known index confines the seed to a few arithmetic progressions, and a
/// handful of calls leave few enough seeds to list instead of 2^31 to try.
/// </summary>
module internal Flip7.KnuthSubtractiveGenerator

// The modulus every value of the generator is reduced by
[<Literal>]
let private P = 2147483647L

// Knuth's constant the generator subtracts the seed from
[<Literal>]
let private MSeed = 161803398

/// <summary>
/// Which of two subtractions in the constructor's first mixing pass overflowed
/// int32 for a seed. Both read word 55 while it still holds Knuth's constant
/// less the seed, unreduced and negative for every seed above it, and an int32
/// that overflows wraps by 2^32, which is 2 modulo Int32.MaxValue.
/// </summary>
[<Struct>]
type public Class =
    /// Neither overflowed, as for every seed up to Knuth's constant
    | Neither
    /// Word 24 less word 55 passed Int32.MaxValue, which left word 24 two short
    | Word24Short
    /// Word 55 less word 31 fell below Int32.MinValue, which left word 55 two
    /// over
    | Word55Over
    /// Both overflowed
    | Both

    /// <summary>
    /// Every class, in the order of a form's constants.
    /// </summary>
    static member All: Class array = [| Neither; Word24Short; Word55Over; Both |]

/// <summary>
/// The affine form of the value at a position of the stream, counted from zero:
/// for a seed s from 0 to Int32.MaxValue of class c, the value is (Alpha * s +
/// Gamma c) % Int32.MaxValue. A position counts every call on the Random, since
/// Next(), Next(min, max) and NextDouble each take exactly one value. At some
/// positions the corrections cancel out and the classes' constants agree. That
/// these forms give every value of every seed, with no exceptions, was proved
/// seed by seed against the real System.Random.
/// </summary>
[<Struct>]
type public Form = {
    Alpha: int64
    Gammas: struct (int64 * int64 * int64 * int64)
} with

    /// <summary>
    /// The constant of a class.
    /// </summary>
    member self.Gamma(class': Class) : int64 =
        let struct (neither, word24Short, word55Over, both) = self.Gammas

        match class' with
        | Neither -> neither
        | Word24Short -> word24Short
        | Word55Over -> word55Over
        | Both -> both

    /// <summary>
    /// The form of a value that is the same for every seed.
    /// </summary>
    static member public Constant(value: int) : Form =
        let value' = int64 value

        {
            Alpha = 0L
            Gammas = struct (value', value', value', value')
        }

    /// <summary>
    /// Add two forms lane by lane modulo Int32.MaxValue.
    /// </summary>
    static member public (+)(a: Form, b: Form) : Form =
        let addition (x: int64) (y: int64) = (x + y) % P
        let struct (a0, a1, a2, a3) = a.Gammas
        let struct (b0, b1, b2, b3) = b.Gammas

        {
            Alpha = addition a.Alpha b.Alpha
            Gammas = struct (addition a0 b0, addition a1 b1, addition a2 b2, addition a3 b3)
        }

    /// <summary>
    /// Subtract two forms lane by lane modulo Int32.MaxValue.
    /// </summary>
    static member public (-)(a: Form, b: Form) : Form =
        let subtraction (x: int64) (y: int64) = (x - y + P) % P
        let struct (a0, a1, a2, a3) = a.Gammas
        let struct (b0, b1, b2, b3) = b.Gammas

        {
            Alpha = subtraction a.Alpha b.Alpha
            Gammas = struct (subtraction a0 b0, subtraction a1 b1, subtraction a2 b2, subtraction a3 b3)
        }

/// <summary>
/// .NET's seeded System.Random, as
/// https://github.com/microsoft/referencesource/blob/ec9fa9ae770d522a5b5f0607898044b7478574a3/mscorlib/system/random.cs
/// writes it, run on forms in place of ints, so that Next gives the form of
/// each value in turn. Random only ever subtracts one word from another, so
/// every word stays a form. Where Random adds Int32.MaxValue to a negative
/// word, the forms add nothing, since that is 0 modulo Int32.MaxValue, and
/// where an int32 overflows, they shift the constants of the classes it
/// overflows in instead.
/// </summary>
type private AffineRandom() =
    // Since F# only finds an operator member on a public type
    let (-) (a: Form) (b: Form) : Form = Form.op_Subtraction (a, b)

    let mutable inext = 0
    let mutable inextp = 21
    let seedArray = Array.zeroCreate<Form> 56

    do
        // The seed is from 0 to Int32.MaxValue, so it is already the absolute
        // value Random takes
        let subtraction = { Alpha = 1L; Gammas = struct (0L, 0L, 0L, 0L) }

        let mutable mj = Form.Constant MSeed - subtraction
        seedArray[55] <- mj

        let mutable mk = Form.Constant 1
        for i in 1..54 do
            let ii = (21 * i) % 55
            seedArray[ii] <- mk
            mk <- mj - mk
            mj <- seedArray[ii]

        for pass in 1..4 do
            for i in 1..55 do
                seedArray[i] <- seedArray[i] - seedArray[1 + (i + 30) % 55]

                // An int32 that overflows wraps by 2^32, which is 2 modulo
                // Int32.MaxValue. Word 24 less word 55 passes Int32.MaxValue in
                // classes Word24Short and Both, leaving word 24 two short, and
                // word 55 less word 31 falls below Int32.MinValue in classes
                // Word55Over and Both, leaving word 55 two over
                if pass = 1 && i = 24 then
                    seedArray[i] <- seedArray[i] - { Alpha = 0L; Gammas = struct (0L, 2L, 0L, 2L) }

                if pass = 1 && i = 55 then
                    seedArray[i] <- seedArray[i] - { Alpha = 0L; Gammas = struct (0L, 0L, -2L, -2L) }

    // Random also turns a difference of Int32.MaxValue into one less, which no
    // form could do, but the forms give every value of every seed, so it never
    // does
    member _.Next() : Form =
        let locINext = inext % 55 + 1
        let locINextp = inextp % 55 + 1
        let retVal = seedArray[locINext] - seedArray[locINextp]
        seedArray[locINext] <- retVal
        inext <- locINext
        inextp <- locINextp
        retVal

/// <summary>
/// The affine forms of the stream's first count values.
/// </summary>
let public AffineForms (count: int) : Form array =
    let random = AffineRandom()
    Array.init count (fun _ -> random.Next())

/// <summary>
/// The class of a seed from 0 to Int32.MaxValue, from .NET's seeded
/// System.Random as
/// https://github.com/microsoft/referencesource/blob/ec9fa9ae770d522a5b5f0607898044b7478574a3/mscorlib/system/random.cs
/// writes its constructor: the first loop, then the first mixing pass, with
/// each of the pass's subtractions also taken in int64 to see whether it
/// overflows int32. Word 55 is the only word the loop leaves negative, so the
/// only subtractions that can overflow are the two that read it: word 24 less
/// word 55, which can pass Int32.MaxValue, and word 55 less word 31, which can
/// fall below Int32.MinValue. FastClassOf finds the same class without the
/// array or the branches.
/// </summary>
let public ClassOf (seed: int) : Class =
    let seedArray = Array.zeroCreate<int> 56

    let mutable mj = MSeed - seed
    seedArray[55] <- mj

    let mutable mk = 1
    for i in 1..54 do
        let ii = (21 * i) % 55
        seedArray[ii] <- mk
        mk <- mj - mk + (if mk > mj then int P else 0)
        mj <- seedArray[ii]

    let mutable above = false
    let mutable below = false

    for i in 1..55 do
        let wide = int64 seedArray[i] - int64 seedArray[1 + (i + 30) % 55]

        if wide > int64 System.Int32.MaxValue then
            above <- true
        if wide < int64 System.Int32.MinValue then
            below <- true

        seedArray[i] <- seedArray[i] - seedArray.[1 + (i + 30) % 55]
        if seedArray[i] < 0 then
            seedArray[i] <- seedArray[i] + System.Int32.MaxValue

    match above, below with
    | false, false -> Neither
    | true, false -> Word24Short
    | false, true -> Word55Over
    | true, true -> Both

/// <summary>
/// ClassOf without its array or its branches, since Enumerate classifies seed
/// after seed: the two subtractions that can overflow read only words 7, 24,
/// 31, 38 and 55, so it keeps just those of the words the first loop writes. It
/// agrees with ClassOf on every seed from 0 to Int32.MaxValue, which a script
/// checked one by one.
/// </summary>
let public FastClassOf (seed: int) : Class =
    // Where the constructor adds Int32.MaxValue to a negative difference, this
    // adds the difference's sign, shifted down to all ones or all zeros, masked
    // to Int32.MaxValue: the same int, with no branch to guess, and that branch
    // goes either way about half the time
    let reduce (difference: int) =
        difference + ((difference >>> 31) &&& System.Int32.MaxValue)

    // The first loop's mj and mk some steps further on
    let rec advance (steps: int) (mj: int) (mk: int) : struct (int * int) =
        if steps = 0 then
            struct (mj, mk)
        else
            advance (steps - 1) mk (reduce (mj - mk))

    // Step i of the first loop writes its mk to word (21 * i) % 55, and 21 is
    // its own inverse modulo 55, so word w is written at step (21 * w) % 55:
    // word 24 at step 9, word 38 at step 28, word 7 at step 37 and word 31 at
    // step 46
    let word55 = MSeed - seed
    let struct (mj, word24) = advance 8 word55 1
    let struct (mj, word38) = advance 19 mj word24
    let struct (mj, word7) = advance 9 mj word38
    let struct (_, word31) = advance 9 mj word7

    // The first pass takes word 31 from word 55 only after taking word 7 from
    // word 31 and word 38 from word 7, none of which overflows
    let word31 = reduce (word31 - reduce (word7 - word38))
    let above = int64 word24 - int64 word55 > int64 System.Int32.MaxValue
    let below = int64 word55 - int64 word31 < int64 System.Int32.MinValue

    match above, below with
    | false, false -> Neither
    | true, false -> Word24Short
    | false, true -> Word55Over
    | true, true -> Both

/// <summary>
/// The values for which Next(0, count) returns an index from low to high, as
/// the first and last of them.
/// </summary>
let public Interval (count: int) (low: int) (high: int) : struct (int * int) =
    if count < 1 || low < 0 || high < low || high >= count then
        let culprit =
            if count < 1 then nameof count
            elif low < 0 || high < low then nameof low
            else nameof high

        invalidArg culprit $"Next(0, {count}) cannot return every index from {low} to {high}"

    // Next scales the value by 1.0 / Int32.MaxValue, then by the count, in
    // double precision and truncates. Rounding to the nearest double never
    // reverses an order and neither does truncation, so the index never falls
    // as the value rises, the values of each index form a run, and a binary
    // search with the very same arithmetic finds where each run starts exactly.
    // An index nothing reaches starts at Int32.MaxValue, one past the largest
    // value
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
/// What a seeded stream produced at a position, counted from zero, as far as it
/// is known: a value from Low to High, where the stream's values run from 0 to
/// Int32.MaxValue - 1. Next() returns the value itself, and Interval gives the
/// values for which Next(0, count) returns an index. Low above High allows no
/// value at all.
/// </summary>
[<Struct>]
type public Constraint = {
    /// <summary>
    /// The position of the value in the stream, counted from zero.
    /// </summary>
    Position: int
    /// <summary>
    /// The least the value can be.
    /// </summary>
    Low: int
    /// <summary>
    /// The greatest the value can be.
    /// </summary>
    High: int
}

// The values the constraints allow at each position they name, in order of
// position, with the constraints on one position intersected. None when they
// leave some position no value
let public windows (constraints: Constraint seq) : struct (int * int * int) array option =
    let merged =
        constraints
        |> Seq.map (fun constrained ->
            if constrained.Position < 0 then
                invalidArg (nameof constraints) $"Position {constrained.Position} is before the stream starts"

            if constrained.Low < 0 || constrained.High > System.Int32.MaxValue - 1 then
                invalidArg
                    (nameof constraints)
                    $"The stream produces no values below 0 or above Int32.MaxValue - 1, so none from {constrained.Low} to {constrained.High}"

            constrained
        )
        |> Seq.groupBy (fun constrained -> constrained.Position)
        |> Seq.map (fun (position, constrained) ->
            struct (position,
                    constrained |> Seq.map (fun constrained -> constrained.Low) |> Seq.max,
                    constrained |> Seq.map (fun constrained -> constrained.High) |> Seq.min)
        )
        |> Seq.sortBy (fun struct (position, _, _) -> position)
        |> Array.ofSeq

    if merged |> Array.exists (fun struct (_, low, high) -> low > high) then
        None
    else
        Some merged

/// <summary>
/// How many seeds from 0 to Int32.MaxValue are expected to be consistent with
/// the constraints, taking the values at different positions to be independent:
/// 2^31 times the fraction of values each constrained position allows. It
/// foretells how many seeds Enumerate lists, and so its memory and the cost of
/// filtering what it lists, though not its time. It is also how many seeds are
/// expected to agree by chance with calls that no seed made, such as those of a
/// Random used before they were counted.
/// </summary>
let public Expected (constraints: Constraint seq) : float =
    match windows constraints with
    | None -> 0.0
    | Some windows ->
        windows
        |> Array.fold (fun expected struct (_, low, high) -> expected * float (high - low + 1) / float P) 2147483648.0

/// <summary>
/// Every seed from 0 to Int32.MaxValue whose System.Random makes all of the
/// calls the constraints describe, and no other seed, each paired with its
/// class, which Filter needs to judge it by later constraints, in ascending
/// order of seed. The forms must reach every constrained position.
///
/// It walks the values the tightest constraint allows, once for each group of
/// classes whose forms agree at every constrained position. Under the group a
/// value pins the seed modulo Int32.MaxValue, the other constraints are checked
/// against their affine forms, and a seed they let through is kept only if its
/// own class is in the group: the forms are exact, so its class is all a seed
/// needs, and it costs the same however far along the stream the constraints
/// are. The residue 0 stands for seeds 0 and Int32.MaxValue, which are of
/// different classes, so both are classified. Every seed is in exactly one
/// group, so none is found twice. It takes time in proportion to the tightest
/// constraint's values times the groups, across every core, which a single draw
/// from a nearly full deck already narrows to tens of millions of values
/// whatever Expected is, and memory in proportion to the seeds it returns, some
/// 40 bytes a seed, which Expected foretells. Constraints that every seed meets
/// cannot be listed, and are refused with an ArgumentException: Expected is
/// 2^31 for them, so a caller that enumerates only once Expected is small never
/// meets one.
/// </summary>
let public Enumerate (forms: Form array) (constraints: Constraint seq) : struct (int * Class) array =
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

        // A value whose coefficient is 0 does not depend on the seed, so it can
        // only filter. None is below position 200000
        let walked =
            windows
            |> Array.filter (fun struct (position, _, _) -> forms[position].Alpha <> 0L)
            |> Array.sortBy width
            |> Array.tryHead
            |> Option.defaultWith (fun () -> invalidArg (nameof constraints) "No constrained value depends on the seed")

        let filters = windows |> Array.filter ((<>) walked) |> Array.sortBy width
        let struct (walkedAt, walkedLow, walkedHigh) = walked
        let alpha = forms[walkedAt].Alpha

        // Int32.MaxValue is prime, so alpha to the power Int32.MaxValue - 2 is
        // alpha's inverse
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

        let filterForms =
            filters |> Array.map (fun struct (position, _, _) -> forms[position])

        let filterLow = filters |> Array.map (fun struct (_, low, _) -> int64 low)
        let filterHigh = filters |> Array.map (fun struct (_, _, high) -> int64 high)

        // As the walked value rises by one the seed rises by the inverse, and
        // the first filter's value by a fixed step, so the walk keeps both with
        // an addition and reaches for a multiplication only when the first
        // filter passes, which it rarely does
        let step =
            if Array.isEmpty filters then
                0L
            else
                filterForms[0].Alpha * inverse % P

        // Classes whose forms agree at every constrained position walk the same
        // seeds, so each group is walked once, under its first class, with the
        // classes in it alongside. The corrections cancel out at some
        // positions, among them position 0, where all four agree
        let groups =
            Class.All
            |> Array.groupBy (fun c -> windows |> Array.map (fun struct (position, _, _) -> forms[position].Gamma c))
            |> Array.map (fun (_, members) -> members[0], members)

        let chunk = 1L <<< 20
        let chunks = (width walked + chunk - 1L) / chunk

        let walk (job: int) : struct (int * Class) array =
            let c, members = groups[job % groups.Length]
            let first = int64 walkedLow + int64 (job / groups.Length) * chunk
            let last = min (first + chunk - 1L) (int64 walkedHigh)
            let gamma = forms[walkedAt].Gamma c
            let found = ResizeArray()

            let keep (seed: int) =
                let own = FastClassOf seed

                if Array.contains own members then
                    found.Add(struct (seed, own))

            let mutable seed = inverse * ((first - gamma + P) % P) % P

            let mutable firstFilter =
                if Array.isEmpty filters then
                    0L
                else
                    (filterForms[0].Alpha * seed + filterForms[0].Gamma c) % P

            let mutable value = first

            while value <= last do
                if
                    Array.isEmpty filters
                    || (firstFilter >= filterLow[0] && firstFilter <= filterHigh[0])
                then
                    let mutable consistent = true
                    let mutable filter = 1

                    while consistent && filter < filters.Length do
                        let filtered = (filterForms[filter].Alpha * seed + filterForms[filter].Gamma c) % P
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
        Array.sortInPlaceBy (fun struct (seed, _) -> seed) found
        found

/// <summary>
/// The survivors, as Enumerate gives them, whose System.Random also makes the
/// calls the constraints describe, in the same order. A survivor's class is
/// known, so each constraint costs it one multiplication. The forms must reach
/// every constrained position.
/// </summary>
let public Filter
    (forms: Form array)
    (constraints: Constraint seq)
    (survivors: struct (int * Class) array)
    : struct (int * Class) array =
    match windows constraints with
    | None -> [||]
    | Some windows ->
        survivors
        |> Array.filter (fun struct (seed, class') ->
            let residue = int64 seed % P

            windows
            |> Array.forall (fun struct (position, low, high) ->
                let value = (forms[position].Alpha * residue + forms[position].Gamma class') % P
                value >= int64 low && value <= int64 high
            )
        )
