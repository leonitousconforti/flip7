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

// What the oracle knows of the seed: before the seeds are listed, the
// constraints so far; after, every seed left with its class, and the affine
// forms as far as the constraints reach. Either way, how many seeds the
// constraints so far are expected to leave, which once they are listed is how
// many would agree by chance with a game no seed dealt
[<NoComparison; NoEquality>]
type internal Knowledge =
    | Gathering of Constraints: KnuthSubtractiveGenerator.Constraint list * Expected: float
    | Listed of
        Survivors: struct (int * KnuthSubtractiveGenerator.Class) array *
        Expected: float *
        Forms: KnuthSubtractiveGenerator.Form array

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
    // While it follows a game, the scan of the instants so far, which Observe
    // advances and a caller that scans the game itself hands it through
    // Follow, how many values the game's Random has drawn by then, and what it
    // knows of the seed
    | Following of Scan: Observation.Scan * Position: int * Knowledge
    | Lost

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
let public Start: Oracle = Unseen

// Takes what the draws of an instant say about the seed into account
let private Learn (knowledge: Knowledge) (constraints: KnuthSubtractiveGenerator.Constraint list) : Knowledge =
    let reach (constraints: KnuthSubtractiveGenerator.Constraint list) =
        constraints
        |> List.fold
            (fun reach (constrained: KnuthSubtractiveGenerator.Constraint) -> max reach (constrained.Position + 1))
            0

    match knowledge, constraints with
    | _, []
    | Listed([||], _, _), _ -> knowledge
    | Gathering(gathered, _), constraints ->
        let gathered = constraints @ gathered
        let expected = KnuthSubtractiveGenerator.Expected gathered

        if expected <= Threshold then
            let forms = KnuthSubtractiveGenerator.AffineForms(2 * reach gathered)
            Listed(KnuthSubtractiveGenerator.Enumerate forms gathered, expected, forms)
        else
            Gathering(gathered, expected)
    // The affine forms are recomputed at twice the length whenever they fall
    // short, so a whole game's cost no more than twice the last. A later
    // instant's draws are at positions no earlier draw took, so they scale the
    // expected count on their own
    | Listed(survivors, expected, forms), constraints ->
        let reach = reach constraints

        let forms =
            if reach <= forms.Length then
                forms
            else
                KnuthSubtractiveGenerator.AffineForms(max reach (2 * forms.Length))

        Listed(
            KnuthSubtractiveGenerator.Filter forms constraints survivors,
            expected * KnuthSubtractiveGenerator.Expected constraints / 2147483648.0,
            forms
        )

/// <summary>
/// Advances the oracle by the instant a scan took in last, given the scan:
/// what Observe does, for a caller that scans the game anyway, so that each
/// instant is scanned once. The scan must have taken in, from
/// Observation.Start and in order, every instant the oracle has observed and
/// then this one, and the oracle keeps it to go on from. An instant the
/// engine could not have produced from the ones before it, as the scan's
/// replay of it tells, leaves no seed.
/// </summary>
let internal Follow (oracle: Oracle) (scan: Observation.Scan) : Oracle =
    match oracle, Observation.ReplayOf scan with
    | Unseen, Some replay when replay.Instant.Event.IsTableSeated && replay.Possible ->
        Following(scan, 0, Gathering([], 2147483648.0))
    | Following(_, position, knowledge), Some replay when replay.Possible ->
        Following(
            scan,
            position + List.sumBy Interpreter.Values replay.Steps,
            Learn knowledge (Interpreter.Constraints position replay.Steps)
        )
    | Unseen, _
    | Following _, _
    | Lost, _ -> Lost

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
/// could not have produced from the ones before it leaves no seed.
/// </summary>
let public Observe (oracle: Oracle) (instant: Instant) : Oracle =
    match oracle with
    | Unseen -> Follow oracle (fst (Observation.Observe Observation.Start instant))
    | Following(scan, _, _) -> Follow oracle (fst (Observation.Observe scan instant))
    | Lost -> Lost

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
    | Following(_, _, Gathering(_, expected)) -> Seeds.Unlisted expected
    | Following(_, _, Listed(survivors, expected, _)) ->
        match survivors with
        | [||] -> Seeds.NoneAgree
        | [| struct (seed, _) |] when expected <= Margin -> Seeds.Pinned seed
        | survivors -> Seeds.Agreeing(survivors |> Array.map (fun struct (seed, _) -> seed))

/// <summary>
/// How many calls the game's Random has made by the last instant observed,
/// whatever its seed, or None when the oracle could not follow the timeline
/// from a TableSeated instant.
/// </summary>
let public CallsOf (oracle: Oracle) : int option =
    match oracle with
    | Following(_, position, _) -> Some position
    | Unseen
    | Lost -> None

/// <summary>
/// Once the seed is pinned, a System.Random fresh from it and wound on by every
/// call the game's Random had made by the last instant observed, which
/// therefore draws what the game's draws next; a new one each time it is asked
/// for. Given a hit-or-stand question's game so far, that is one call short of
/// the game's own Random when the question was asked, which had already drawn
/// the question's seed: Timeline.ContinueWith from the question, given that
/// history, this Random and the same decider, asks the question again with the
/// very same draw and replays the rest of the game exactly. An aim made while a
/// card is still being resolved is no place to continue from, since the calls
/// of the resolution so far have no instants yet.
/// </summary>
let public RandomOf (oracle: Oracle) : System.Random option =
    match oracle, SeedsOf oracle with
    | Following(_, position, _), Seeds.Pinned seed ->
        let random = System.Random seed

        for _ in 1..position do
            random.Next() |> ignore

        Some random
    | _ -> None
