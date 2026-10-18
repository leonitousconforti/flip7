module ObservationTests

open Xunit
open Flip7
open Support

// Worked examples of the rules by which Observation reads the questions a
// player was asked out of a timeline. AimTests' ``the questions a timeline
// reveals are the questions the engine asked, one for one`` holds every
// decision and aim of generated games to the questions the engine asked, but
// it cannot say which rule a mismatch broke, and it seldom reaches some of
// these corners. These timelines are small enough to read, and each shows its
// rules at work. The last edit example is the exception: it plays a seeded
// game and forks it as the play editor does, because what it pins is the
// engine's own count of turns after an edit.

let private player (name: string) (hand: Hand) : Player =
    Player.Make(name, Strategy.Random, hand = hand)

// An instant of a hand-written timeline. Its decks are always the full deck
// and no discards, because these examples pin who was asked what, on which
// turn and alongside whom, and leave the decks a question was asked with to
// AimTests, which holds them to the engine's
let private instant (event: Event) (players: Player list) : Instant = {
    Event = event
    Players = players
    Deck = Deck.Full
    Discards = Deck.Empty
}

let private names (players: Player list) : string list =
    players |> List.map (fun player -> player.Name)

let private two = ValueCard Card.Two
let private three = ValueCard Card.Three
let private five = ValueCard Card.Five
let private six = ValueCard Card.Six
let private seven = ValueCard Card.Seven
let private nine = ValueCard Card.Nine
let private freeze = ActionCard Card.Freeze

// The hit-or-stand decisions a timeline shows, without its aims
let private decisions (timeline: Instant list) : Observation list =
    observationsOf timeline
    |> List.where (fun observation -> observation.Choice.IsChoice1Of2)

// A hit-or-stand decision, or an aim with what it aimed and at whom
type private Answer = Choice<Strategy.HitOrStand, Strategy.Ask * string>

// A question as these examples pin it: who was asked, their answer, the round
// and the turn, then the names of the others at the table and of the finished
// players. The others of a decision are those still in the round, and of an
// aim its candidates, the chooser among them for a freeze or a deal3
type private Asked = string * Answer * uint * uint * string list * string list

let private asked (observation: Observation) : Asked =
    observation.Name,
    observation.Choice,
    observation.Round,
    observation.Turn,
    names observation.OtherPlayers,
    names observation.FinishedPlayers

// ---- hit-or-stand decisions ----

[<Fact>]
let ``a player's first turn of a round is dealt, not decided, even holding cards from a deal3, and every later hit or stand is decided from the hand before it``
    ()
    =
    let timeline = [
        instant TableSeated [ player "A" []; player "B" [] ]
        // A's forced first hit draws a deal3 aimed at B, so B holds cards
        // before their own first turn while A holds none after theirs
        instant (Dealt3("A", "B", [ two; nine ])) [ player "B" [ nine; two ]; player "A" [] ]
        // B's first turn is still a forced hit, not a decision
        instant (Drew("B", five)) [ player "A" []; player "B" [ five; nine; two ] ]
        // A's second turn is a decision even though their hand is empty
        instant (Drew("A", six)) [ player "B" [ five; nine; two ]; player "A" [ six ] ]
        instant (Stood "B") [ player "A" [ six ]; player "B" [ five; nine; two ] ]
    ]

    // Each decision's chooser, answer and turn, and the hand they decided from
    Assert.Equal<(string * Answer * uint * Hand) list>(
        [
            "A", Choice1Of2 Strategy.Hit, 2u, []
            "B", Choice1Of2 Strategy.Stand, 2u, [ five; nine; two ]
        ],
        decisions timeline
        |> List.map (fun observation -> observation.Name, observation.Choice, observation.Turn, observation.Player.Hand)
    )

[<Fact>]
let ``a decision's other players are those still in the round, and those who stood, busted or were frozen are finished, each in the order the instant before it lists them``
    ()
    =
    let a = player "A" [ five ]
    let b = player "B" [ seven ]
    let c = player "C" [ two; two ]
    let d = player "D" [ nine ]
    let d' = player "D" [ three; nine ]
    let frozen = player "D" [ freeze; three; nine ]

    let timeline = [
        // The opening deal: every player's first turn is a forced hit
        instant (Drew("A", five)) [ player "B" []; player "C" []; player "D" []; a ]
        instant (Drew("B", seven)) [ player "C" []; player "D" []; a; b ]
        instant (Drew("C", two)) [ player "D" []; a; b; player "C" [ two ] ]
        instant (Drew("D", nine)) [ a; b; player "C" [ two ]; d ]
        // Second turns onward are decisions
        instant (Busted("C", two)) [ b; d; a; c ]
        instant (Stood "B") [ d; a; b; c ]
        instant (Drew("D", three)) [ a; d'; b; c ]
        instant (Froze("A", "D")) [ a; frozen; b; c ]
        instant (Drew("A", six)) [ player "A" [ six; five ]; frozen; b; c ]
    ]

    // Turns count per player within the round: everyone's second turn, then
    // A's third. Others and finished keep the order the instant before each
    // decision lists the players in, as the decider receives them, which is
    // not the seating order
    Assert.Equal<Asked list>(
        [
            // Busting is still choosing to hit
            "C", Choice1Of2 Strategy.Hit, 1u, 2u, [ "A"; "B"; "D" ], []
            "B", Choice1Of2 Strategy.Stand, 1u, 2u, [ "D"; "A" ], [ "C" ]
            "D", Choice1Of2 Strategy.Hit, 1u, 2u, [ "A" ], [ "B"; "C" ]
            // Drawing the freeze is A choosing to hit, not a decision of D's,
            // whom it froze
            "A", Choice1Of2 Strategy.Hit, 1u, 2u, [ "D" ], [ "B"; "C" ]
            "A", Choice1Of2 Strategy.Hit, 1u, 3u, [], [ "D"; "B"; "C" ]
        ],
        decisions timeline |> List.map asked
    )

// ---- edits ----

[<Fact>]
let ``players who busted stay finished after an edit takes their busting cards back`` () =
    let timeline = [
        instant (Drew("A", five)) [ player "B" []; player "C" []; player "A" [ five ] ]
        instant (Drew("B", two)) [ player "C" []; player "A" [ five ]; player "B" [ two ] ]
        instant (Drew("C", nine)) [ player "A" [ five ]; player "B" [ two ]; player "C" [ nine ] ]
        instant (Busted("A", five)) [ player "B" [ two ]; player "C" [ nine ]; player "A" [ five; five ] ]
        // B hits, drawing a deal3 aimed at C, who busts on the first flip
        instant (Dealt3("B", "C", [ nine ])) [
            player "B" [ two ]
            player "C" [ nine; nine ]
            player "A" [ five; five ]
        ]
        // The play editor takes both busting cards back, and the engine still
        // counts A and C finished
        instant (Edited "B") [ player "B" [ two ]; player "C" [ nine ]; player "A" [ five ] ]
        instant (Drew("B", seven)) [ player "B" [ seven; two ]; player "C" [ nine ]; player "A" [ five ] ]
    ]

    Assert.Equal<Asked list>(
        [
            "A", Choice1Of2 Strategy.Hit, 1u, 2u, [ "B"; "C" ], []
            "B", Choice1Of2 Strategy.Hit, 1u, 2u, [ "C" ], [ "A" ]
            // After the edit B is alone in the round
            "B", Choice1Of2 Strategy.Hit, 1u, 3u, [], [ "C"; "A" ]
        ],
        decisions timeline |> List.map asked
    )

// The play editor forks a game at a hit-or-stand question: an Edited instant
// for the changed table, and the game continued with everyone still in the
// round counted as having taken one turn fewer than the question's, as
// Games.playForked continues it. Forked at Bob's second turn, Alice, who sat
// before him and had taken two, is counted as having taken one, so the engine
// asks her about her second turn once more
[<Fact>]
let ``after an edit, everyone still in the round has taken as many turns as the asker had`` () =
    let lineup = [
        "Alice", Strategy.AlwaysHits, Targeting.ChoosesRandomly
        "Bob", Strategy.AlwaysHits, Targeting.ChoosesRandomly
        "Carol", Strategy.AlwaysHits, Targeting.ChoosesRandomly
    ]

    // Every hit-or-stand question of the first round: how much of the game it
    // showed, whom it asked and on which turn
    let questions = ResizeArray<int * string * uint>()

    let logging =
        beforeEachQuestion (fun _ history question ->
            match question with
            | HitOrStandQuestion(Round = 1u; Turn = turn; Player = player) ->
                questions.Add(Seq.length history, player.Name, turn)
            | _ -> ()
        )

    // Forked at the game's second question, where the edit moves a card from
    // the deck to the discards
    let timeline, _, _ =
        Games.playForked logging 4 lineup (Some(Games.editing { AtQuestion = 1; Edit = Some Games.Moved }))

    let editedAt =
        timeline
        |> List.tryFindIndex (fun instant -> instant.Event.IsEdited)
        |> Option.defaultValue -1

    // Seed 4 is no rarity: most seeds ask Bob about his second turn second
    Assert.True(
        questions.Contains(editedAt, "Bob", 2u),
        "the game was not forked at Bob's second turn; pick a new seed"
    )

    let questions =
        questions
        |> Seq.filter (fun (seen, _, _) -> seen > editedAt)
        |> Seq.map (fun (_, name, turn) -> name, turn)
        |> List.ofSeq

    // ...and most keep everyone in the round through the next three questions
    Assert.True(
        List.truncate 3 questions = [ "Bob", 2u; "Carol", 2u; "Alice", 2u ],
        "the engine no longer asks Bob, Carol and then Alice about their second turns after the edit; pick a new seed"
    )

    Assert.Equal<(string * uint) list>(
        questions,
        decisions timeline
        |> List.skip (decisions (List.take editedAt timeline)).Length
        |> List.where (fun observation -> observation.Round = 1u)
        |> List.map (fun observation -> observation.Name, observation.Turn)
    )

// ---- aims ----

[<Fact>]
let ``an instant's hit-or-stand decision comes before its aims, each with its round, turn and candidates`` () =
    let timeline = [
        instant TableSeated [ player "A" []; player "B" []; player "C" [] ]
        // A's forced first hit draws a freeze, which A gives to C
        instant (Froze("A", "C")) [ player "B" []; player "A" []; player "C" [ freeze ] ]
        instant (Drew("B", five)) [ player "A" []; player "B" [ five ]; player "C" [ freeze ] ]
        // A hits on their second turn and draws a deal3, which A gives to B,
        // whose flips set a freeze aside
        instant (Dealt3("A", "B", [ two; freeze; nine ])) [
            player "B" [ nine; freeze; two; five ]
            player "A" []
            player "C" [ freeze ]
        ]
        // ...which B gives to A, having taken one turn
        instant (Froze("B", "A")) [ player "B" [ nine; two; five ]; player "A" [ freeze ]; player "C" [ freeze ] ]
        instant (Stood "B") [ player "B" [ nine; two; five ]; player "A" [ freeze ]; player "C" [ freeze ] ]
        instant (RoundEnded(Map.ofList [ "A", 0u; "B", 16u; "C", 0u ])) [ player "A" []; player "B" []; player "C" [] ]
        // The second round starts one seat on, with B's forced first hit: a
        // deal3 B gives to A before A's first turn, whose flips set a freeze aside
        instant (Dealt3("B", "A", [ two; freeze ])) [ player "C" []; player "A" [ freeze; two ]; player "B" [] ]
        // ...which A gives to C, having taken no turn this round
        instant (Froze("A", "C")) [ player "C" [ freeze ]; player "A" [ two ]; player "B" [] ]
    ]

    Assert.Equal<Asked list>(
        [
            "A", Choice2Of2(Strategy.Ask.WhoToFreeze, "C"), 1u, 1u, [ "B"; "C"; "A" ], []
            "A", Choice1Of2 Strategy.Hit, 1u, 2u, [ "B" ], [ "C" ]
            "A", Choice2Of2(Strategy.Ask.WhoReceivesDeal3, "B"), 1u, 2u, [ "B"; "A" ], [ "C" ]
            // A set-aside card given out is not a decision, and freezing A
            // leaves B alone in the round to stand on their second turn
            "B", Choice2Of2(Strategy.Ask.WhoToFreeze, "A"), 1u, 1u, [ "B"; "A" ], [ "C" ]
            "B", Choice1Of2 Strategy.Stand, 1u, 2u, [], [ "A"; "C" ]
            "B", Choice2Of2(Strategy.Ask.WhoReceivesDeal3, "A"), 2u, 1u, [ "C"; "A"; "B" ], []
            "A", Choice2Of2(Strategy.Ask.WhoToFreeze, "C"), 2u, 0u, [ "C"; "A"; "B" ], []
        ],
        observationsOf timeline |> List.map asked
    )
