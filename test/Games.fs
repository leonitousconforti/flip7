module Games

open System
open FSharp.Control
open FsCheck
open FsCheck.FSharp

open Flip7
open Support

// Games as the play editor plays them, which OracleTests and AimTests both
// hold their subjects to: seats that answer with randomness of their own, the
// edits the editor can make at a question, the games the properties play, and
// one harness that plays a game on a Recording, forks it at a question and
// continues it on the same stream.

// Answers as the default decider does, except for the seats of the strategy
// and the targeting named Own, which answer with randomness of their own,
// unrelated to the game's: a Random seeded by how much of the game they are
// shown, so a seeded game played again from any question answers alike
let own: Strategy.Decider =
    Strategy.Decider.Default
    |> Strategy.addHitOrStandStrategy
        "Own"
        (fun _ history _ _ _ _ _ _ ->
            async.Return(
                if Random(Seq.length history).NextDouble() < 0.6 then
                    Strategy.Hit
                else
                    Strategy.Stand
            )
        )
    |> Strategy.addTargetingStrategy
        "Own"
        (fun _ history _ _ candidates _ _ ->
            async.Return(List.randomChoiceWith (Random(Seq.length history)) candidates)
        )

// Where the decisions stand after the instants
let scanOf (instants: Instant list) : Observation.Scan =
    instants
    |> List.fold (fun scan instant -> fst (Observation.Observe scan instant)) Observation.Start

// The table at a hit-or-stand question, as the play editor changes it: the
// players still in the round, those finished, and the decks
type Table = Player list * Player list * (Deck * Deck)

// A change the play editor can make at a question: a card moved between the
// deck and the discards, a hand's last card taken back to the deck, a busted
// hand's before any other's, which un-busts it while its player stays
// finished, or a card dealt from the deck into a hand. A seat counts round
// everyone at the table
type Edit =
    | Moved
    | Undealt of Seat: int
    | Dealt of Seat: int * Card: Card

// What an edit makes of the table. One with nothing to change, one that the
// editor would refuse because it busts a player still in the round, or one
// that gives a hand a second SecondChance, which the engine never deals and
// cannot play, moves a card instead
let edited (edit: Edit) ((active, finished, (deck, discards)): Table) : Table =
    let everyone = active @ finished

    let moved () =
        match deck |> Map.tryFindKey (fun _ count -> count > 0u) with
        | Some card -> active, finished, (Deck.Decrement deck card, Deck.Increment discards card)
        | None ->
            let card = discards |> Map.findKey (fun _ count -> count > 0u)
            active, finished, (Deck.Increment deck card, Deck.Decrement discards card)

    let holding (name: string) (hand: Hand) =
        let hold =
            List.map (fun (player: Player) ->
                if player.Name = name then
                    { player with Hand = hand }
                else
                    player
            )

        hold active, hold finished

    match edit with
    | Moved -> moved ()
    | Undealt seat ->
        let chosen =
            finished
            |> List.tryFind (fun player -> Hand.IsBust player.Hand)
            |> Option.defaultValue everyone[seat % everyone.Length]

        match chosen.Hand with
        | card :: rest ->
            let active, finished = holding chosen.Name rest
            active, finished, (Deck.Increment deck card, discards)
        | [] -> moved ()
    | Dealt(seat, card) when Map.find card deck > 0u ->
        let chosen = everyone[seat % everyone.Length]
        let active, finished = holding chosen.Name (card :: chosen.Hand)

        if
            active |> List.exists (fun player -> Hand.IsBust player.Hand)
            || card = ActionCard Card.SecondChance && List.contains card chosen.Hand
        then
            moved ()
        else
            active, finished, (Deck.Decrement deck card, discards)
    | Dealt _ -> moved ()

// Where to fork a game: at which hit-or-stand question, counted from zero, and
// what the play editor changes at it, if anything. A generated game's fork
// names its change as an Edit, and the harness takes the change itself
type Fork<'edit> = { AtQuestion: int; Edit: 'edit option }

// The fork with its edit made as edited makes it
let editing (fork: Fork<Edit>) : Fork<Table -> Table> = {
    AtQuestion = fork.AtQuestion
    Edit = Option.map edited fork.Edit
}

// The names of up to twelve seats
let seatNames = [
    "Alice"
    "Bob"
    "Carol"
    "Dave"
    "Eve"
    "Frank"
    "Grace"
    "Heidi"
    "Ivan"
    "Judy"
    "Mallory"
    "Niaj"
]

// Plenty of seats that always hit, for long rounds that run the deck out
let private genHittingStrategy: Gen<Strategy> =
    Gen.frequency [
        4, Gen.constant Strategy.AlwaysHits
        1, Gen.constant Strategy.AlwaysStands
        2, Gen.map (uint >> Strategy.HitUntilScore) (Gen.choose (10, 40))
        1, Gen.map (uint >> Strategy.HitUntilNumCards) (Gen.choose (2, 7))
        1, Gen.map (uint >> Strategy.StandsAfterTurn) (Gen.choose (1, 5))
        2, Gen.map Strategy.RandomWithProbability genProbability
        2, Gen.map (uint >> Strategy.PlaysLikeAHuman) (Gen.choose (10, 30))
        2, Gen.constant Strategy.MaximizesExpectedValue
        2, Gen.constant (Strategy.Custom "Own")
    ]

// Every kind of targeting, Own among them, and taking sides among the names
// at the table, otherwise by a policy the engine plays, since a decider
// registered for Own answers only for ChoosesExternally "Own" itself
let private genAnyTargeting (names: string list) : Gen<Targeting> =
    let policy =
        Gen.oneof [
            Gen.constant Targeting.ChoosesRandomly
            Gen.constant Targeting.PlaysSpitefully
            Gen.map2
                (fun banksAbove deal3sBelow -> Targeting.PlaysGreedily(banksAbove, deal3sBelow))
                genProbability
                genProbability
        ]

    Gen.frequency [
        3, policy
        1, Gen.constant (Targeting.ChoosesExternally "Own")
        1, genSides (Gen.elements names) policy
    ]

let private genEdit: Gen<Edit> =
    Gen.frequency [
        1, Gen.constant Moved
        2, Gen.map Undealt (Gen.choose (0, 11))
        2,
        Gen.map2
            (fun seat card -> Dealt(seat, card))
            (Gen.choose (0, 11))
            (Gen.frequency [ 1, Gen.constant (ActionCard Card.SecondChance); 2, genCard ])
    ]

// A game of two to twelve seats on every kind of targeting, a third of them
// forked at a hit-or-stand question, with an edit or without. OracleTests and
// AimTests play the same games
let genGameWithFork: Gen<int * Lineup * Fork<Edit> option> = gen {
    let! seed = genSeed
    let! count = Gen.choose (2, 12)
    let! strategies = Gen.listOfLength count genHittingStrategy
    let! targetings =
        Gen.listOfLength count (genAnyTargeting (List.take count seatNames))

    let! fork =
        Gen.frequency [
            2, Gen.constant None
            1,
            Gen.map2
                (fun question edit -> Some { AtQuestion = question; Edit = edit })
                (Gen.choose (0, 80))
                (Gen.oneof [ Gen.constant None; Gen.map Some genEdit ])
        ]

    return seed, List.zip3 (List.take count seatNames) strategies targetings, fork
}

// Six seats, most of them always hitting, at which the seeds of the rarest
// roads through a deal3 were found
let rarestRoadsLineup: Lineup = [
    "Alice", Strategy.AlwaysHits, Targeting.ChoosesRandomly
    "Bob", Strategy.AlwaysHits, Targeting.PlaysSpitefully
    "Carol", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
    "Dave", Strategy.AlwaysHits, Targeting.PlaysGreedily(0.3, 0.2)
    "Eve", Strategy.PlaysLikeAHuman 15u, Targeting.ChoosesRandomly
    "Frank", Strategy.AlwaysHits, Targeting.ChoosesRandomly
]

// How many times a game went down the rarer roads. OracleTests counts the
// roads through a deal3, from the calls the interpreter reads off the
// timeline, and AimTests the roads an aim takes, from the questions the
// engine asked, so each counts the fields of its own half and leaves the rest
// at zero
type Roads = {
    // Through a deal3: a freeze set aside and resolved later
    SetAsideFreezes: int
    // A deal3 set aside and resolved later
    SetAsideDeal3s: int
    // A deal3 during whose flips a second chance was passed on
    PassedInFlips: int
    // Flips cut short by a flip7
    CutShortByFlip7: int
    // The deck running out during the flips
    ReshuffledInFlips: int
    // Doing so during a set-aside deal3's, whose card the discards hold by
    // then
    ReshuffledInSetAside: int
    // A second chance discarded during the flips because nobody else could
    // take one
    DiscardedInFlips: int
    // One discarded because the one passed on before it took the last taker
    RanOutOfTakers: int
    // Aims: a card set aside during a deal3 and given out afterwards
    AimedSetAside: int
    // A set-aside card given out by a flipper an earlier one froze out of the
    // round
    AimedByTheFrozen: int
    // A second chance passed on during a deal3's flips
    AimsPassedInFlips: int
    // One passed past a player in the round who holds one already
    AimsPastAHolder: int
    // One passed on before one that nobody is left to take
    AimsBeforeNoTaker: int
    // Two of them passed on during one deal3's flips, each counted
    AimsPassedTwice: int
    // A question with a single candidate
    ForcedAims: int
    // The first aim of a round after the first, when the turn order has moved
    // one seat on
    AimsAfterRotation: int
}

// A game that went down none of the roads
let noRoads: Roads = {
    SetAsideFreezes = 0
    SetAsideDeal3s = 0
    PassedInFlips = 0
    CutShortByFlip7 = 0
    ReshuffledInFlips = 0
    ReshuffledInSetAside = 0
    DiscardedInFlips = 0
    RanOutOfTakers = 0
    AimedSetAside = 0
    AimedByTheFrozen = 0
    AimsPassedInFlips = 0
    AimsPastAHolder = 0
    AimsBeforeNoTaker = 0
    AimsPassedTwice = 0
    ForcedAims = 0
    AimsAfterRotation = 0
}

// Raised by a decider to stop a game at a hit-or-stand question, with
// everything the question showed of it
exception Forked of
    History: Instant list *
    Round: uint *
    Turn: uint *
    Player: Player *
    Others: Player list *
    Finished: Player list *
    Decks: (Deck * Deck)

#if !FABLE_COMPILER
// Answers as own does, naming each question's call on the game's Random as it
// is asked, and forks the game at the given hit-or-stand question, counted
// from zero
let private recorded (game: Recording) (forkAt: int option) : Strategy.Decider =
    let asked = ref 0

    own
    |> beforeEachQuestion (fun _ history question ->
        match question with
        | HitOrStandQuestion(_, round, turn, player, others, finished, decks) ->
            game.Name Questioned (Seq.length history)

            if forkAt = Some asked.Value then
                raise (Forked(List.ofSeq history, round, turn, player, others, finished, decks))

            asked.Value <- asked.Value + 1
        | TargetQuestion _ -> game.Name AimedAt (Seq.length history)
    )

// Plays a game on a Recording, every question asked of a decider that answers
// as own does, inside the given wrapper, or when a fork is asked for, plays it
// up to that hit-or-stand question and continues it on the same stream. Given
// an edit, it continues as the play editor does: after an Edited instant for
// the changed table, made in place of the question's answer and so after the
// call the question drew its seed with, with every active player's turns
// counted one short of the question's. Without one, it continues with nothing
// changed and exactly the game's turns, from just before that call, which the
// continuation makes again. Returns the timeline, every call made for it, and
// whether the game reached the fork
let playForked
    (wrap: Strategy.Decider -> Strategy.Decider)
    (seed: int)
    (lineup: Lineup)
    (fork: Fork<Table -> Table> option)
    : Instant list * Made list * bool =
    let first = Recording seed
    let yielded = ResizeArray<Instant>()
    let forkAt = fork |> Option.map (fun fork -> fork.AtQuestion)

    try
        Timeline.SimulateWithDecider first (wrap (recorded first forkAt)) lineup
        |> AsyncSeq.iter (fun instant ->
            yielded.Add instant
            first.Yielded <- yielded.Count
        )
        |> Async.RunSynchronously

        List.ofSeq yielded, List.ofSeq first.Made, false
    with Forked(history, round, turn, player, others, finished, decks) ->
        let second = Recording seed
        let seating = (List.head history).Players |> List.map (fun player -> player.Name)

        let given, wound, turns, (active, finished, decks) =
            match fork |> Option.bind (fun fork -> fork.Edit) with
            | Some edit ->
                let table = edit (player :: others, finished, decks)
                let active, finished, (deck, discards) = table

                let edited = {
                    Event = Edited (List.head active).Name
                    Players = active @ finished
                    Deck = deck
                    Discards = discards
                }

                let turns = active |> List.map (fun player -> player.Name, turn - 1u) |> Map.ofList
                history @ [ edited ], first.Made.Count, turns, table
            | None ->
                history,
                first.Made.Count - 1,
                Observation.TurnsTaken(scanOf history),
                (player :: others, finished, decks)

        second.Wind(Seq.take wound first.Made)
        let continued = ResizeArray<Instant>()
        second.Yielded <- given.Length

        Timeline.ContinueWith second (wrap (recorded second None)) given seating round turns active finished decks
        |> AsyncSeq.iter (fun instant ->
            continued.Add instant
            second.Yielded <- given.Length + continued.Count
        )
        |> Async.RunSynchronously

        given @ List.ofSeq continued, List.ofSeq second.Made, true
#endif
