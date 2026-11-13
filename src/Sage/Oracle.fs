/// <summary>
/// Recovers the seed of the System.Random a game was dealt with from its
/// timeline alone, instant by instant: neither the lineup's strategies nor
/// their answers are needed, since every question costs the game's Random one
/// call whatever is answered. Once it has pinned the seed, a Random wound to
/// where the game's stands plays the rest of the game again. Like Lookahead, it
/// is .NET only, since it relies on how .NET's seeded System.Random draws.
/// </summary>
module public Flip7.Oracle

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

/// <summary>
/// Seed recovery partway through a timeline: everything an oracle has learnt
/// from the instants it has observed, opaque to callers. It is immutable, so a
/// caller handed the game so far at every question can keep the latest one and
/// observe only the instants since.
/// </summary>
[<NoComparison; NoEquality>]
type public Oracle =
    private
    // Either way, the scan of every instant observed, how many values the
    // game's Random has drawn by the last of them, and how many seeds the draws
    // so far are expected to leave, which once they are listed is how many
    // would agree by chance with a game no seed dealt. Until few enough are
    // expected to list them, the draws themselves; after, every seed left with
    // its class, and the affine forms as far as the draws reach. A timeline the
    // oracle cannot follow lists no seed
    | Gathering of
        Scan: Observation.Scan *
        Calls: int *
        Expected: float *
        Draws: KnuthSubtractiveGenerator.Constraint list
    | Listed of
        Scan: Observation.Scan *
        Calls: int *
        Expected: float *
        Survivors: struct (int * KnuthSubtractiveGenerator.Class) array *
        Forms: KnuthSubtractiveGenerator.Form array

// The expected count of agreeing seeds at which they are listed. Listing walks
// the values the tightest draw allows, which one draw from a nearly full deck
// already narrows to tens of millions, so it takes much the same time whatever
// the count: over sixty games, a median of 0.03 to 0.06 s and at most 0.17 s on
// ten cores for any count from 1e3 to 1e6. What grows with the count is memory,
// some 40 bytes a seed listed, and the cost of checking the seeds against every
// later draw. At 1e4 a listing takes 0.2 MB at the median and 0.5 MB at most,
// and the next draw checks its seeds in 0.02 ms, while waiting for 1e3 saves
// only some 15 ms and lists a draw later. A median game gets there in five
// draws
[<Literal>]
let private Threshold = 1e4

// How few seeds must be expected to agree by chance for the one seed left to be
// taken as the game's. A game no seed dealt, on an unseeded Random or on one
// used before the game, leaves only seeds that agree by chance, and over 600
// such games nearly a third showed the last of them alone for a few instants
// before a draw ruled it out. The chance that any is left once the expected
// count falls to the margin is at most the margin, and seeds only drop out from
// there, so at most one such game in a thousand ever shows a seed as pinned,
// and none of the 600 did. Over 300 seeded games it costs two cards: the true
// seed is pinned with 11 cards seen at the median and 15 at most, rather than 9
// and 13. A margin of 1e-2 would pin a card sooner, and 1e-4 a card later
[<Literal>]
let private Margin = 1e-3

/// <summary>
/// The oracle before any instant: every seed still agrees.
/// </summary>
let public Start: Oracle = Gathering(Observation.Start, 0, 2147483648.0, [])

/// <summary>
/// Advances the oracle by the instant a scan took in last, given the scan:
/// what Observe does, for a caller that scans the game anyway, so that each
/// instant is scanned once. The scan must have taken in, from
/// Observation.Start and in order, every instant the oracle has observed and
/// then this one, and the oracle keeps it to go on from.
/// </summary>
let internal Follow (oracle: Oracle) (scan: Observation.Scan) : Oracle =
    let reach (draws: KnuthSubtractiveGenerator.Constraint list) =
        draws
        |> List.fold (fun reach (drawn: KnuthSubtractiveGenerator.Constraint) -> max reach (drawn.Position + 1)) 0

    match oracle, Observation.ReplayOf scan with
    | Listed(_, calls, expected, [||], forms), _ -> Listed(scan, calls, expected, [||], forms)
    | (Gathering(_, calls, _, _) | Listed(_, calls, _, _, _)), Some replay when replay.Possible ->
        let draws = Interpreter.Constraints calls replay.Steps
        let calls = calls + List.sumBy Interpreter.Values replay.Steps

        match oracle, draws with
        | Gathering(_, _, expected, gathered), [] -> Gathering(scan, calls, expected, gathered)
        | Listed(_, _, expected, survivors, forms), [] -> Listed(scan, calls, expected, survivors, forms)
        | Gathering(_, _, _, gathered), draws ->
            let gathered = draws @ gathered
            let expected = KnuthSubtractiveGenerator.Expected gathered

            if expected > Threshold then
                Gathering(scan, calls, expected, gathered)
            else
                let forms = KnuthSubtractiveGenerator.AffineForms(2 * reach gathered)
                Listed(scan, calls, expected, KnuthSubtractiveGenerator.Enumerate forms gathered, forms)
        // A later instant's draws are at positions no earlier draw took, so
        // they scale the expected count on their own, and the affine forms are
        // recomputed at twice the length whenever they fall short, so a whole
        // game's cost no more than twice the last
        | Listed(_, _, expected, survivors, forms), draws ->
            let forms =
                if reach draws <= forms.Length then
                    forms
                else
                    KnuthSubtractiveGenerator.AffineForms(max (reach draws) (2 * forms.Length))

            Listed(
                scan,
                calls,
                expected * KnuthSubtractiveGenerator.Expected draws / 2147483648.0,
                KnuthSubtractiveGenerator.Filter forms draws survivors,
                forms
            )
    | _ -> Listed(scan, 0, 0.0, [||], [||])

/// <summary>
/// Advances the oracle by one instant. The instant and those before it tell
/// exactly which calls the engine made on the game's Random to produce it, and
/// every card drawn from two or more says which values the call that picked it
/// could have returned, which only some seeds give. Until few enough seeds are
/// expected to agree for them to be listed, the draws are only gathered, and
/// the step costs a few microseconds; the step that lists them takes some tens
/// of milliseconds across every core, and each step after it checks every seed
/// left against the new draws, at a few nanoseconds a seed. Every draw is
/// checked even once one seed is left, and a seed that fails one is dropped,
/// which leaves none. The first instant must be a TableSeated, from which the
/// game's Random is taken to be fresh from its seed, and an instant the engine
/// could not have produced from the ones before it leaves no seed, after which
/// later instants are not even scanned.
/// </summary>
let public Observe (oracle: Oracle) (instant: Instant) : Oracle =
    match oracle with
    | Listed(_, _, _, [||], _) -> oracle
    | Gathering(scan, _, _, _)
    | Listed(scan, _, _, _, _) -> Follow oracle (fst (Observation.Observe scan instant))

/// <summary>
/// What the oracle knows of the seed.
/// </summary>
let public SeedsOf (oracle: Oracle) : Seeds =
    match oracle with
    | Gathering(_, _, expected, _) -> Seeds.Unlisted expected
    | Listed(_, _, _, [||], _) -> Seeds.NoneAgree
    | Listed(_, _, expected, [| struct (seed, _) |], _) when expected <= Margin -> Seeds.Pinned seed
    | Listed(_, _, _, survivors, _) -> Seeds.Agreeing(survivors |> Array.map (fun struct (seed, _) -> seed))

/// <summary>
/// Once the seed is pinned, a System.Random fresh from it and wound on by every
/// call the game's Random had made by the last instant observed, which
/// therefore draws what the game's draws next; a new one each time it is asked
/// for. Given a hit or stand question's game so far, that is one call short of
/// the game's own Random when the question was asked, which had already drawn
/// the question's seed: Timeline.ContinueWith from the question, given that
/// history, this Random and the same decider, asks the question again with the
/// very same draw and replays the rest of the game exactly. An aim made while a
/// card is still being resolved is no place to continue from, since the calls
/// of the resolution so far have no instants yet.
/// </summary>
let public RandomOf (oracle: Oracle) : System.Random option =
    match oracle, SeedsOf oracle with
    | Listed(_, calls, _, _, _), Seeds.Pinned seed ->
        let random = System.Random seed

        for _ in 1..calls do
            random.Next() |> ignore

        Some random
    | _ -> None
