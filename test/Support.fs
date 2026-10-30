module Support

open System
open FSharp.Control
open FsCheck
open FsCheck.FSharp

open Flip7

// What the test files share: decks and hands built from their cards, a
// timeline played out and the questions it shows, how Inference models a
// player it never saw decide or aim and what it makes of the observations of
// those it did, the field that studies and Sage are tested against, studied
// as it plays and taking sides, a decider that notes every question before
// passing it on, a Random that logs every call made on it, and the generators
// the properties draw from.

// A table to seat: each player's name, strategy and targeting
type Lineup = (string * Strategy * Targeting) list

// The four seats whose games the digests pin, and at which the seeds of the
// deepest questions inside a deal3 were found
let fourSeats: Lineup = [
    "Alice", Strategy.Random, Targeting.ChoosesRandomly
    "Bob", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
    "Carol", Strategy.AlwaysHits, Targeting.ChoosesRandomly
    "Dave", Strategy.HitUntilNumCards 4u, Targeting.ChoosesRandomly
]

// A deck of exactly the given cards that still holds every card, most at
// zero, so that looking any card up is safe
let deckOf (cards: Card list) : Deck =
    cards |> List.fold Deck.Increment Deck.Empty

// A hand of the given value cards, newest first as listed
let values (cards: Card.ValueCard list) : Hand = cards |> List.map ValueCard

// Plays a timeline, or any stream, out to its end
let played (stream: AsyncSeq<'T>) : 'T list =
    stream |> AsyncSeq.toListAsync |> Async.RunSynchronously

// Every question a timeline shows a player was asked, in the order the engine
// asked them
let observationsOf (timeline: Instant list) : Observation list =
    timeline |> AsyncSeq.ofSeq |> Observation.FromTimeline |> played

// How Inference models a player never seen decide: by the prior, even over
// every strategy of the default grid
let strategyPrior: (Strategy * float) list =
    Inference.DefaultStrategyCandidates
    |> List.map (fun candidate -> candidate, 1.0 / float Inference.DefaultStrategyCandidates.Length)

// How Inference models a player never seen aim at two or more players: by the
// prior, which over nobody to take sides with is even over every policy of the
// default grid, none of which takes sides
let aimPrior: (Targeting * float) list =
    Inference.DefaultTargetingCandidates
    |> List.map (fun targeting -> targeting, 1.0 / float Inference.DefaultTargetingCandidates.Length)

// The evidence a player's observations carry at the default rate of
// out-of-character choices, over the default grids and the names their aims
// were among, as though they were of one game
let evidenceOf (name: string) (observations: Observation list) : PlayerEvidence =
    Inference.Evidence 0.1 Inference.DefaultStrategyCandidates Inference.DefaultTargetingCandidates [] name observations

// The model of each player some observations show deciding or aiming at one
// of two or more players, from their own observations alone
let modelsOf (observations: Observation list) : PlayerModel list =
    observations
    |> List.groupBy (fun observation -> observation.Name)
    |> List.map (fun (name, theirs) -> evidenceOf name theirs)
    |> List.where (fun evidence -> evidence.Observations > 0 || evidence.Aims > 0)
    |> List.map Inference.ModelFrom

// The probability a targeting gives the target of an aim: the aim's
// likelihood under a player who never aims out of character, which no
// strategy changes
let probabilityOfTarget (targeting: Targeting) (aim: Observation) : float =
    Inference.Likelihood 0.0 Strategy.AlwaysHits targeting aim

// Opponents whose strategies the default candidate grid holds and which ten
// games tell apart from every other candidate in it, so that a study of them
// names each one exactly and a replay predicts every answer they give: their
// targeting is an engine policy, which a replay keeps. Fewer games left Bob's
// HitUntilScore 24 tied with ChasesFlip7 (25, 6), which only the order of the
// grid broke
let bob = "Bob", Strategy.HitUntilScore 24u, Targeting.PlaysSpitefully
let carol = "Carol", Strategy.HitUntilNumCards 4u, Targeting.ChoosesRandomly
let dave =
    "Dave", Strategy.MaximizesExpectedValue, Targeting.PlaysGreedily(0.3, 0.2)
let erin = "Erin", Strategy.StandsAfterTurn 3u, Targeting.ChoosesRandomly
let field: Lineup = [ bob; carol; dave; erin ]

// Who sits in the seat named Sage in the games of the field that are studied,
// which have no Sage
let standIn = "Sage", Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully

// Ten games of the field with the stand-in in Sage's seat, studied once for
// every test that needs them
let fieldStudy =
    lazy
        ([
            for seed in 1..10 -> Timeline.SimulateWith (Random(100 + seed)) (standIn :: field)
         ]
         |> Study.OfTimelines
         |> Async.RunSynchronously)

// Bob and Carol play as the field's players of the same names do, but under
// ChoosesExternally and taking sides, as the engine plays it: for Dave and
// against Sage, and otherwise spitefully. A study of twenty games of the table
// fits how they aim, and Sage replays them as it fitted them, where it used to
// replay anyone who declared ChoosesExternally as spiteful
let partisan =
    Targeting.TakesSides(Some "Dave", Some "Sage", Targeting.PlaysSpitefully)

let partisans: Strategy.Decider =
    Strategy.Decider.Default
    |> Strategy.addTargetingStrategy "Partisan" (fun random -> Strategy.Decider.Default.Target random partisan)

let partisanField: Lineup =
    let takingSides (name, strategy, _) =
        name, strategy, Targeting.ChoosesExternally "Partisan"

    [ takingSides bob; takingSides carol; dave; erin ]

let partisanStudy =
    lazy
        ([
            for seed in 1..20 -> Timeline.SimulateWithDecider (Random(200 + seed)) partisans (standIn :: partisanField)
         ]
         |> Study.OfTimelines
         |> Async.RunSynchronously)

// How likely a study makes it that a player aims as a targeting
let aimsAs (targeting: Targeting) (study: Study) (name: string) : float =
    (Study.ModelOf study name).Value.TargetingPosterior
    |> List.find (fst >> (=) targeting)
    |> snd

// A question the engine puts to a decider, with everything it shows the
// decider besides the question's Random and the game so far
type Question =
    | HitOrStandQuestion of
        Strategy: Strategy *
        Round: uint *
        Turn: uint *
        Player: Player *
        Others: Player list *
        Finished: Player list *
        Decks: (Deck * Deck)
    | TargetQuestion of
        Targeting: Targeting *
        Ask: Strategy.Ask *
        Chooser: Player *
        Candidates: Player list *
        Finished: Player list *
        Decks: (Deck * Deck)

    // The player the question is put to
    member self.Asked: Player =
        match self with
        | HitOrStandQuestion(Player = player) -> player
        | TargetQuestion(Chooser = chooser) -> chooser

// Shows every question to onQuestion, with its Random and the game so far, as
// it is asked, and then passes it on to the inner decider for its answer
let beforeEachQuestion
    (onQuestion: Random -> Instant seq -> Question -> unit)
    (inner: Strategy.Decider)
    : Strategy.Decider = {
    HitOrStand =
        fun random strategy history round turn player others finished decks ->
            onQuestion random history (HitOrStandQuestion(strategy, round, turn, player, others, finished, decks))
            inner.HitOrStand random strategy history round turn player others finished decks
    Target =
        fun random targeting history ask chooser candidates finished decks ->
            onQuestion random history (TargetQuestion(targeting, ask, chooser, candidates, finished, decks))
            inner.Target random targeting history ask chooser candidates finished decks
}

// A call made on a Recording. A parameterless Next is logged with what it
// drew: those are the draws that seed a question's Random. A decider asked
// straight after it may name it for the instant it was made for, counted from
// the game's TableSeated, which the decider's game so far tells: for an aim
// made while a card is being resolved, that runs past what the timeline has
// yielded. A Next(min, max) is a step of the shuffle that deals a card, made
// for the instant after everything yielded so far, or for the one the last
// question named, when that is later. The engine never calls Next(max) or
// NextDouble, but a Recording logs them too
type Made =
    | Unnamed of Value: int
    | Questioned of Instant: int
    | AimedAt of Instant: int
    | Shuffled of Instant: int * From: int * Count: int * Value: int
    | Below of Max: int * Value: int
    | Fraction of Value: float

#if !FABLE_COMPILER
// Passes every call to a Random of the same seed and logs each one. Each call
// the engine makes draws a single sample, so a fresh Random of the seed wound
// on by the count of calls stands exactly where this one does. Every overload
// it does not log raises, so no call it cannot account for goes unseen
type Recording(seed: int) =
    inherit Random()
    let inner = Random seed
    let made = ResizeArray<Made>()

    let never (call: string) =
        raise (InvalidOperationException $"the engine called {call}")

    // Every call made so far, oldest first
    member _.Made: Collections.Generic.IReadOnlyList<Made> = made :> _

    // How many calls have been made so far
    member _.Calls: int = made.Count

    // How many instants the game has yielded, and the instant the last
    // question was named for, which a harness keeps up to date so that a
    // shuffle is logged for the instant it deals
    member val Yielded = 0 with get, set
    member val Named = 0 with get, set

    // Makes the calls of an earlier game, so a continuation picks up its
    // stream where it left off
    member _.Wind(calls: Made seq) =
        for call in calls do
            inner.Next() |> ignore
            made.Add call

    // Names the last call, which has to be a parameterless Next nobody has
    // named yet, as the draw of a question asked at the given instant
    member self.Name (question: int -> Made) (instant: int) =
        if made.Count = 0 || not made[made.Count - 1].IsUnnamed then
            raise (InvalidOperationException "a question was asked without a draw of its own")

        made[made.Count - 1] <- question instant
        self.Named <- instant

    override _.Next() =
        let value = inner.Next()
        made.Add(Unnamed value)
        value

    override self.Next(minValue: int, maxValue: int) =
        let value = inner.Next(minValue, maxValue)
        made.Add(Shuffled(max self.Yielded self.Named, minValue, maxValue, value))
        value

    override _.Next(maxValue: int) =
        let value = inner.Next maxValue
        made.Add(Below(maxValue, value))
        value

    override _.NextDouble() =
        let value = inner.NextDouble()
        made.Add(Fraction value)
        value

    override _.Sample() = never "Sample"
    override _.NextSingle() = never "NextSingle"
    override _.NextInt64() = never "NextInt64"
    override _.NextInt64(_: int64) : int64 = never "NextInt64"
    override _.NextInt64(_: int64, _: int64) : int64 = never "NextInt64"
    override _.NextBytes(_: byte array) : unit = never "NextBytes"
    override _.NextBytes(_: Span<byte>) : unit = never "NextBytes"
#endif

// The generators stay inside the domain's own invariants: decks are sub-decks
// of the full 94 cards, strategy numbers use the tame magnitudes real games
// use (keeping floats finite, so round-trips compare equal), and names draw
// from an alphabet that includes the separators the serialization formats
// have to survive, spaces and colons. Two go outside them on purpose:
// genHand is any list of cards, so Hand's own functions are tried on hands
// no game deals as well as on those it does, and genEvent makes events no
// game would (a Dealt3 of any number of cards, names nobody is seated
// under), since an event's saved form and a continuation's history have to
// take any event.

let genCard: Gen<Card> = Deck.Empty |> Map.toList |> List.map fst |> Gen.elements

let genHand: Gen<Hand> = Gen.listOf genCard

let genSeed: Gen<int> = Gen.choose (0, 1000000)

let genName: Gen<string> =
    [ 'a' .. 'z' ] @ [ ' '; ':'; '+' ]
    |> Gen.elements
    |> Gen.nonEmptyListOf
    |> Gen.map (List.toArray >> String)

let genUint: Gen<uint> = Gen.choose (0, 300) |> Gen.map uint

let genProbability: Gen<float> =
    Gen.choose (0, 100) |> Gen.map (fun percent -> float percent / 100.0)

// Every strategy DecideWith can evaluate: Custom is decided by an injected
// decider and raises there, so it stays out of engine draws
let genEngineStrategy: Gen<Strategy> =
    Gen.oneof [
        Gen.constant Strategy.AlwaysHits
        Gen.constant Strategy.AlwaysStands
        Gen.map Strategy.RandomWithProbability genProbability
        Gen.map Strategy.HitUntilScore genUint
        Gen.map Strategy.HitUntilNumCards genUint
        Gen.map Strategy.HitUntilBustProbability genProbability
        Gen.map Strategy.HitUntilNaiveBustProbability genProbability
        Gen.map2
            (fun threshold temperature -> Strategy.SoftHitUntilScore(threshold, temperature))
            genUint
            genProbability
        Gen.map Strategy.HitUntilTotal genUint
        Gen.map Strategy.HitUntilUniqueValues genUint
        Gen.map2 (fun score uniques -> Strategy.ChasesFlip7(score, uniques)) genUint genUint
        Gen.map Strategy.EmboldenedBySecondChance genUint
        Gen.map Strategy.HitWhileBehindLeader genUint
        Gen.map Strategy.StandsAfterTurn genUint
        Gen.map Strategy.PlaysLikeAHuman genUint
        Gen.constant Strategy.MaximizesExpectedValue
    ]

let genStrategy: Gen<Strategy> =
    Gen.frequency [ 15, genEngineStrategy; 1, Gen.map Strategy.Custom genName ]

let genDeck: Gen<Deck> = gen {
    let cards = Deck.Full |> Map.toList
    let! counts = Gen.listOfLength cards.Length (Gen.choose (0, 12))

    return
        List.zip cards counts
        |> List.map (fun ((card, full), count) -> card, uint (min count (int full)))
        |> Map.ofList
}

let genEvent: Gen<Event> =
    Gen.oneof [
        Gen.constant TableSeated
        Gen.map2 (fun name card -> Drew(name, card)) genName genCard
        Gen.map Stood genName
        Gen.map2 (fun name card -> Busted(name, card)) genName genCard
        Gen.map2 (fun source target -> Froze(source, target)) genName genName
        Gen.map2 (fun source target -> SecondChancePassed(source, target)) genName genName
        Gen.map SecondChanceDiscarded genName
        Gen.map3 (fun source target cards -> Dealt3(source, target, cards)) genName genName (Gen.listOf genCard)
        Gen.map Flip7Achieved genName
        Gen.map Edited genName
        Gen.map2 (fun name score -> name, score) genName genUint
        |> Gen.listOf
        |> Gen.map (Map.ofList >> RoundEnded)
    ]

// Taking sides one to Targeting.SidesLimit deep, each depth as likely as the
// next, each side one of the names or nobody, and at the end of the chain
// otherwise aiming by the policy
let genSides (names: Gen<string>) (policy: Gen<Targeting>) : Gen<Targeting> =
    let side = Gen.frequency [ 1, Gen.constant None; 2, Gen.map Some names ]

    let takingSides (otherwise: Targeting) =
        Gen.map2 (fun favours opposes -> Targeting.TakesSides(favours, opposes, otherwise)) side side

    let rec chain (depth: int) (otherwise: Targeting) : Gen<Targeting> =
        if depth = 0 then
            Gen.constant otherwise
        else
            takingSides otherwise |> Gen.bind (chain (depth - 1))

    gen {
        let! depth = Gen.choose (1, Targeting.SidesLimit)
        let! otherwise = policy
        return! chain depth otherwise
    }

// The policies that decide for themselves or pick at random, taking no sides
let private genEnginePolicy: Gen<Targeting> =
    Gen.oneof [
        Gen.constant Targeting.ChoosesRandomly
        Gen.constant Targeting.PlaysSpitefully
        Gen.map2
            (fun banksAbove deal3sBelow -> Targeting.PlaysGreedily(banksAbove, deal3sBelow))
            genProbability
            genProbability
    ]

// Every targeting policy DecideTargetWith can evaluate, taking sides among the
// names the lineups seat and a stranger's among them. ChoosesExternally is
// decided by an injected decider and raises there, alone or as what one who
// takes sides otherwise does, so it stays out of engine draws
let genEngineTargeting: Gen<Targeting> =
    Gen.frequency [
        3, genEnginePolicy
        1, genSides (Gen.elements [ "Alice"; "Bob"; "Carol"; "Dave"; "Eve"; "Stranger" ]) genEnginePolicy
    ]

// Names that are awkward to write between quotes: empty, all spaces, holding
// quotes and backslashes, the words a side is written with, or characters
// beyond ASCII, one of them a surrogate pair
let private genAwkwardName: Gen<string> =
    Gen.frequency [
        2,
        Gen.elements [
            ""
            " "
            "\""
            "\\"
            "\\\""
            "Carol \\"
            "a \"b\" c"
            "for"
            "against"
            "otherwise"
            " otherwise "
            "\" otherwise PlaysSpitefully"
            "for \"Bob\" against"
            "Zo\u00EB \u6771\u4EAC \U0001F600"
        ]
        1, genName
    ]

// Every targeting, taking sides over awkward names and plain ones alike
let genTargeting: Gen<Targeting> =
    let policy =
        Gen.frequency [ 3, genEnginePolicy; 1, Gen.map Targeting.ChoosesExternally genName ]

    Gen.frequency [ 3, policy; 1, genSides genAwkwardName policy ]

// One to five uniquely named players on any engine strategies and policies
let genLineup: Gen<Lineup> = gen {
    let! count = Gen.choose (1, 5)
    let! strategies = Gen.listOfLength count genEngineStrategy
    let! targetings = Gen.listOfLength count genEngineTargeting

    return
        List.take count [ "Alice"; "Bob"; "Carol"; "Dave"; "Eve" ]
        |> List.mapi (fun i name -> name, List.item i strategies, List.item i targetings)
}

// A seed and a lineup the engine decides for alone
let genEngineGame: Gen<int * Lineup> = Gen.zip genSeed genLineup

// Deals a hand card by card out of the given deck, keeping at most one
// SecondChance per hand and, unless busting is allowed, skipping duplicate
// value cards, so an active player can never start busted. Action cards can
// land in hands: the play editor really produces such splices.
let rec private dealHand (allowBust: bool) (remaining: int) (hand: Hand) (deck: Deck) : Gen<Hand * Deck> = gen {
    if remaining = 0 then
        return hand, deck
    else
        let candidates =
            deck
            |> Map.toList
            |> List.filter (fun (card, count) ->
                count > 0u
                && (card <> ActionCard Card.SecondChance
                    || not (List.contains (ActionCard Card.SecondChance) hand))
                && (allowBust
                    || match card with
                       | ValueCard _ -> not (List.contains card hand)
                       | _ -> true)
            )
            |> List.map fst

        match candidates with
        | [] -> return hand, deck
        | _ ->
            let! card = Gen.elements candidates
            return! dealHand allowBust (remaining - 1) (card :: hand) (Deck.Decrement deck card)
}

// A mid-round state to continue a game from: the round, the turns taken by
// everyone still in it, the players still in it, those finished, and the deck
// and the discards
type Splice = {
    Round: uint
    TurnsTaken: Map<string, uint>
    Active: Player list
    Finished: Player list
    Decks: Deck * Deck
}

// A legal mid-round state for ContinueWith: hands dealt out of the full deck
// so the 94-card invariant holds by construction, no active player busted,
// what remains split between the deck and the discards, and a turn counted
// for anyone already holding cards
let genSplice: Gen<Splice> = gen {
    let! activeCount = Gen.choose (1, 3)
    let! finishedCount = Gen.choose (0, 2)
    let count = activeCount + finishedCount
    let names = List.take count [ "Alice"; "Bob"; "Carol"; "Dave"; "Eve" ]

    let! strategies = Gen.listOfLength count genEngineStrategy
    let! targetings = Gen.listOfLength count genEngineTargeting
    let! firmScores = Gen.listOfLength count (Gen.map uint (Gen.choose (0, 190)))
    let! handSizes = Gen.listOfLength count (Gen.choose (0, 6))

    let rec dealHands index hands deck = gen {
        if index = count then
            return List.rev hands, deck
        else
            let! hand, deck' =
                dealHand (index >= activeCount) (List.item index handSizes) [] deck
            return! dealHands (index + 1) (hand :: hands) deck'
    }

    let! hands, undealt = dealHands 0 [] Deck.Full

    let! splits = Gen.listOfLength (Map.count undealt) (Gen.choose (0, 12))

    let deck =
        List.zip (Map.toList undealt) splits
        |> List.map (fun ((card, remaining), split) -> card, min (uint split) remaining)
        |> Map.ofList

    let discards =
        undealt |> Map.map (fun card remaining -> remaining - Map.find card deck)

    let players =
        names
        |> List.mapi (fun i name ->
            Player.Make(
                name,
                List.item i strategies,
                List.item i firmScores,
                List.item i hands,
                List.item i targetings
            )
        )

    let active = List.take activeCount players
    let finished = List.skip activeCount players

    let turnsTaken =
        active
        |> List.map (fun player -> player.Name, (if List.isEmpty player.Hand then 0u else 1u))
        |> Map.ofList

    let! round = Gen.map uint (Gen.choose (1, 10))

    return {
        Round = round
        TurnsTaken = turnsTaken
        Active = active
        Finished = finished
        Decks = deck, discards
    }
}

// Continues a game from a splice after the given history, with everyone
// seated in the order the splice lists them, those still in the round first
let continueSplice (random: Random) (decide: Strategy.Decider) (history: Instant seq) (splice: Splice) : Timeline =
    Timeline.ContinueWith
        random
        decide
        history
        (splice.Active @ splice.Finished |> List.map (fun player -> player.Name))
        splice.Round
        splice.TurnsTaken
        splice.Active
        splice.Finished
        splice.Decks
