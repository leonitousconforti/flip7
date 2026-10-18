module PropertyTests

open System
open FSharp.Control
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support

// Properties of whole games. FsCheck plays a game for every case, from a
// generated seed and lineup or a generated mid-round state, and shrinks any
// failure to a smaller counterexample. A case is a whole game rather than a
// single card or hand, so each property sets its own count instead of taking
// FsCheck's default hundred, and says why that count is enough. The generators
// used here keep to the domain's invariants (every card is accounted for and
// nobody still in a round has busted), with one deliberate exception: the
// history a continuation is given is made of arbitrary events, because the
// engine passes a history on without reading it.

// ---- cards and scores ----

// Every card of the full deck (79 value cards, 6 modifiers and 9 actions, 94
// in all) is in the deck, the discards or a hand at every instant
let private conservesEveryCard (timeline: Instant list) : bool =
    timeline
    |> List.forall (fun instant ->
        instant.Players
        |> List.map (fun player -> player.Hand)
        |> Simulation.Issues instant.Deck instant.Discards
        |> Seq.isEmpty
    )

// What a hand banks when its round ends: its score, or nothing if it busted
let private banks (player: Player) : uint =
    if Hand.IsBust player.Hand then
        0u
    else
        Hand.Score player.Hand

// Each RoundEnded scores every player what their hand banks, adds exactly that
// to their firm score and empties their hand, and no other instant changes a
// firm score. Start is the players as they stood before the first instant
let private roundEndsBankExactly (start: Player list) (timeline: Instant list) : bool =
    let firmScores (players: Player list) =
        players |> List.map (fun player -> player.Name, player.FirmScore) |> Map.ofList

    timeline
    |> List.mapFold (fun before instant -> (before, instant), instant.Players) start
    |> fst
    |> List.forall (fun (before, instant) ->
        match instant.Event with
        | RoundEnded scores ->
            let banked =
                before |> List.map (fun player -> player.Name, banks player) |> Map.ofList

            scores = banked
            && instant.Players |> List.forall (fun player -> List.isEmpty player.Hand)
            && firmScores instant.Players = (firmScores before |> Map.map (fun name firm -> firm + banked[name]))
        | _ -> firmScores instant.Players = firmScores before
    )

// The last instant is a RoundEnded at which somebody has 200 or more, and no
// earlier RoundEnded had anybody there
let private endsAtFirstRoundAnyoneReaches200 (timeline: Instant list) : bool =
    let reached200 (instant: Instant) =
        instant.Players |> List.exists (fun player -> player.FirmScore >= 200u)

    let roundEnds = timeline |> List.filter (fun instant -> instant.Event.IsRoundEnded)
    let last = List.last timeline

    last.Event.IsRoundEnded
    && reached200 last
    && roundEnds |> List.take (roundEnds.Length - 1) |> List.forall (reached200 >> not)

// An instant that shows somebody holding seven distinct values is followed by
// a Flip7Achieved naming them, which still shows the hand and is followed by
// the RoundEnded, and no Flip7Achieved comes any other way
let private flip7EndsTheRound (timeline: Instant list) : bool =
    let holdingSeven (instant: Instant) =
        instant.Players
        |> List.tryFind (fun player -> Hand.HasFlip7Bonus player.Hand)
        |> Option.map (fun player -> player.Name)

    timeline
    |> List.pairwise
    |> List.forall (fun (instant, next) ->
        match holdingSeven instant, instant.Event with
        | Some holder, Flip7Achieved achiever -> holder = achiever && next.Event.IsRoundEnded
        | Some holder, _ -> next.Event = Flip7Achieved holder
        | None, event -> not event.IsFlip7Achieved
    )

// Every rule above, labelled, for a game played out from the given players
let private keepsEveryRule (start: Player list) (timeline: Instant list) : Property =
    Prop.label "every card is in the deck, the discards or a hand at every instant" (conservesEveryCard timeline)
    .&. Prop.label
            "each RoundEnded banks what the hands score, and nothing else moves a firm score"
            (roundEndsBankExactly start timeline)
    .&. Prop.label
            "the game ends at the first RoundEnded with anybody at 200 or more"
            (endsAtFirstRoundAnyoneReaches200 timeline)
    .&. Prop.label "seven distinct values end the round at once" (flip7EndsTheRound timeline)

[<Fact>]
let ``every simulated game conserves all 94 cards, banks each round's scores, and ends at the first round anyone reaches 200``
    ()
    =
    // Twenty cases, because each plays a whole game of many rounds and so
    // exercises every common rule many times over
    let property =
        Prop.forAll
            (Arb.fromGen genEngineGame)
            (fun (seed, lineup) ->
                let timeline = Timeline.SimulateWith (Random seed) lineup |> played
                keepsEveryRule (List.head timeline).Players timeline
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

[<Fact>]
let ``continuing from any legal mid-round state conserves all 94 cards, banks each round's scores, and ends at the first round anyone reaches 200``
    ()
    =
    // genSplice deals firm scores of 0 to 190, so nobody has won before the
    // game goes on; hands of at most six cards, so nobody starts out holding
    // seven distinct values; and rounds 1 to 10, so the round being continued
    // can have started at any of up to five seats. Twenty cases, as for whole
    // games, since each continuation plays on to the end of its game
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.zip genSeed genSplice))
            (fun (seed, splice) ->
                let timeline =
                    continueSplice (Random seed) Strategy.Decider.Default Seq.empty splice |> played

                keepsEveryRule (splice.Active @ splice.Finished) timeline
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

// ---- what each question is shown ----

// What an aim aimed, who aimed it and at whom
type private Aim = { Ask: Strategy.Ask; Chooser: string; Target: string }

// A question as it was asked: the history it was shown, a reading of that
// history begun at the question and finished once the game is over, what the
// history held when the question was asked, how many instants the timeline had
// yielded by then, and for an aim, the aim
type private Shown = {
    History: Instant seq
    FinishReading: unit -> Instant list
    Seen: Instant list
    Yielded: int
    Aim: Aim option
}

// An aim shown instants the timeline had yet to yield, who aimed it, and those
// instants
type private AimedAhead = { Chooser: string; Pending: Instant list }

// What a game's questions were shown, as everyQuestionIsShownThePast below
// tells it
type private Showing = { Unshown: string option; Ahead: AimedAhead list }

// Plays a game through a decider that answers as the default one does, noting
// at every question what it was shown of the game so far and how much of the
// timeline had been yielded when it was asked. Every question has to have been
// shown exactly the instants produced before it: the history the game was
// given, then everything yielded so far, and for an aim whatever the card
// being resolved has produced too, which the timeline yields only once the
// card is resolved. So an aim's history runs up to the very instant that
// records its answer, and every history still holds the same once the game is
// over. Returns a description of the first question that falls short, if any
// does, and every aim that was shown instants the timeline had yet to yield
let private everyQuestionIsShownThePast (given: Instant list) (play: Strategy.Decider -> Timeline) : Showing =
    let inner = Strategy.Decider.Default
    let yielded = ResizeArray<Instant>()
    let questions = ResizeArray<Shown>()

    // Begins reading a history at its question, past the history the game was
    // given and into the engine's own, and leaves the rest to read once the
    // game is over, so a view must stay readable while the game records more
    let startReading (history: Instant seq) : unit -> Instant list =
        let reader = history.GetEnumerator()
        let read = ResizeArray<Instant>()

        while read.Count <= given.Length && reader.MoveNext() do
            read.Add reader.Current

        fun () ->
            while reader.MoveNext() do
                read.Add reader.Current

            List.ofSeq read

    let asked (history: Instant seq) : Shown = {
        History = history
        FinishReading = startReading history
        Seen = List.ofSeq history
        Yielded = yielded.Count
        Aim = None
    }

    let noting: Strategy.Decider = {
        HitOrStand =
            fun random strategy history round turn player others finished decks -> async {
                questions.Add(asked history)
                return! inner.HitOrStand random strategy history round turn player others finished decks
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                // Noted as the game stood when the aim was asked, and logged
                // with the answer
                let shown = asked history

                let! answer =
                    inner.Target random targeting history ask chooser candidates finished decks

                questions.Add {
                    shown with
                        Aim = Some { Ask = ask; Chooser = chooser.Name; Target = answer.Name }
                }

                return answer
            }
    }

    play noting |> AsyncSeq.iter yielded.Add |> Async.RunSynchronously

    let timeline = Array.ofList (given @ List.ofSeq yielded)

    // The very instants of the game rather than equal ones, which also keeps
    // checking every question of a whole game cheap
    let beginsTheTimeline (seen: Instant list) =
        seen.Length <= timeline.Length
        && seen
           |> List.indexed
           |> List.forall (fun (index, instant) -> obj.ReferenceEquals(instant, timeline[index]))

    let unshown (index: int) (question: Shown) : string option =
        let soFar = given.Length + question.Yielded

        let same (later: Instant list) =
            later.Length = question.Seen.Length
            && List.forall2 (fun before after -> obj.ReferenceEquals(before, after)) question.Seen later

        let shownExactly =
            match question.Aim with
            | None -> question.Seen.Length = soFar && beginsTheTimeline question.Seen
            | Some aim ->
                question.Seen.Length >= soFar
                && question.Seen.Length < timeline.Length
                && beginsTheTimeline question.Seen
                && match aim.Ask, timeline[question.Seen.Length].Event with
                   | Strategy.Ask.WhoToFreeze, Froze(source, target)
                   | Strategy.Ask.WhoReceivesDeal3, Dealt3(source, target, _)
                   | Strategy.Ask.WhoReceivesSecondChance, SecondChancePassed(source, target) ->
                       source = aim.Chooser && target = aim.Target
                   // Passed on during a deal3's flips, before the deal3 has an
                   // instant of its own to record it in
                   | Strategy.Ask.WhoReceivesSecondChance, Dealt3(_, target, cards) ->
                       target = aim.Chooser && List.contains (ActionCard Card.SecondChance) cards
                   | _ -> false

        let described =
            match question.Aim with
            | None -> $"question {index}, a hit-or-stand,"
            | Some aim -> $"question {index}, {aim.Chooser} aiming {aim.Ask} at {aim.Target},"

        // Read afresh, and by the reading begun at the question
        if not (same (List.ofSeq question.History) && same (question.FinishReading())) then
            Some $"{described} was shown a history that changed as the game went on"
        elif not shownExactly then
            Some $"{described} was not shown exactly the game before it"
        else
            None

    {
        Unshown =
            questions
            |> Seq.indexed
            |> Seq.tryPick (fun (index, question) -> unshown index question)
        Ahead =
            questions
            |> Seq.choose (fun question ->
                match question.Aim with
                | Some aim when question.Seen.Length > given.Length + question.Yielded ->
                    Some {
                        Chooser = aim.Chooser
                        Pending = List.skip (given.Length + question.Yielded) question.Seen
                    }
                | _ -> None
            )
            |> List.ofSeq
    }

// Whether a game's questions were all shown the past, as a property
let private shownThePast (showing: Showing) : Property =
    match showing.Unshown with
    | None -> Prop.ofTestable true
    | Some reason -> Prop.label reason false

[<Fact>]
let ``every question is shown exactly the game before it`` () =
    let mutable ahead = 0

    // Twenty games, each asking hundreds of questions. An aim asked while a
    // card is still being resolved turns up in a good share of games, so the
    // guard below all but never fails by chance
    let property =
        Prop.forAll
            (Arb.fromGen genEngineGame)
            (fun (seed, lineup) ->
                let showing =
                    everyQuestionIsShownThePast
                        []
                        (fun decide -> Timeline.SimulateWithDecider (Random seed) decide lineup)

                ahead <- ahead + showing.Ahead.Length
                shownThePast showing
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

    Assert.True(ahead > 0, "no aim was asked ahead of the timeline, so the unyielded instants went unchecked")

// The only check that gives a game a history of many instants and a mid-round
// start together, which is how the play editor continues a game
[<Fact>]
let ``every question in a continuation is shown its history and the game since`` () =
    let mutable ahead = 0

    // A continuation plays from a random mid-round state to the end of its
    // game, so fewer of them than of whole games ask an aim while a card is
    // still being resolved. A hundred cases keep the guard below safe, and
    // they are cheap
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.zip3 genSeed genSplice (Gen.nonEmptyListOf genEvent)))
            (fun (seed, splice, events) ->
                // The engine passes the history on without reading it, so any
                // instants will do
                let history =
                    events
                    |> List.map (fun event -> {
                        Event = event
                        Players = splice.Active @ splice.Finished
                        Deck = fst splice.Decks
                        Discards = snd splice.Decks
                    })

                let showing =
                    everyQuestionIsShownThePast
                        history
                        (fun decide -> continueSplice (Random seed) decide history splice)

                ahead <- ahead + showing.Ahead.Length
                shownThePast showing
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

    Assert.True(ahead > 0, "no aim was asked ahead of the timeline, so the unyielded instants went unchecked")

// Seeds 314 and 674 each reach the two deepest places a question is asked: a
// set-aside aimed after an earlier set-aside freeze of its deal3 was resolved,
// and an aim inside a deal3 that a set-aside deal3 started. Only a few random
// lineups in a hundred reach either, so the properties above can miss them in
// a run, and these seeds check them every time, from the opening deal and in a
// continuation given a history alike
[<Theory>]
[<InlineData 314>]
[<InlineData 674>]
let ``a question deep inside a deal3 is shown everything the deal3 has produced`` (seed: int) =
    let players =
        fourSeats
        |> List.map (fun (name, strategy, targeting) -> Player.Make(name, strategy, targeting = targeting))

    let seated = {
        Event = TableSeated
        Players = players
        Deck = Deck.Full
        Discards = Deck.Empty
    }

    // The same game again, continued from its opening deal after an edit, the
    // way the play editor continues one
    let history = [ seated; { seated with Event = Edited "Alice" } ]
    let seating = players |> List.map (fun player -> player.Name)

    let games = [
        "from the opening deal",
        everyQuestionIsShownThePast [] (fun decide -> Timeline.SimulateWithDecider (Random seed) decide fourSeats)

        "continued after an edit",
        everyQuestionIsShownThePast
            history
            (fun decide ->
                Timeline.ContinueWith
                    (Random seed)
                    decide
                    history
                    seating
                    1u
                    Map.empty
                    players
                    []
                    (Deck.Full, Deck.Empty)
            )
    ]

    for game, showing in games do
        match showing.Unshown with
        | Some reason -> Assert.Fail $"seed {seed}, {game}: {reason}"
        | None -> ()

        // The check above means nothing for a seed that never reaches these
        // places, and a change to how the game draws can move a seed off them.
        // An aim shown a Froze not yet yielded comes after a set-aside freeze
        // was resolved
        Assert.True(
            showing.Ahead
            |> List.exists (fun aim -> aim.Pending |> List.exists (fun instant -> instant.Event.IsFroze)),
            $"seed {seed} {game} no longer reaches a set-aside aimed after a set-aside freeze; pick a new seed"
        )

        // The first instant not yet yielded is the deal3 the card being
        // resolved started, whose own set-asides its target aims, so an aim
        // by anybody else comes from inside a deal3 one of them started
        Assert.True(
            showing.Ahead
            |> List.exists (fun aim ->
                match (List.head aim.Pending).Event with
                | Dealt3(_, target, _) -> aim.Chooser <> target
                | _ -> false
            ),
            $"seed {seed} {game} no longer reaches an aim inside a deal3 that a set-aside deal3 started; pick a new seed"
        )

// ---- the game's Random ----

// Two plays of every seed, so this is also the check that a seed fixes the
// whole game within a build, as the TimelineTests digests check it across
// builds
[<Fact>]
let ``what a decider draws from its Random never changes the game`` () =
    // A seat whose decider rolls its own dice on the Random each question comes
    // with, for its hits and its aims alike. It is taught as a Custom strategy
    // and a ChoosesExternally targeting, so its questions also check that the
    // engine routes those to the decider that was taught them
    let dice =
        Strategy.Decider.Default
        |> Strategy.addHitOrStandStrategy
            "Dice"
            (fun random _ _ _ _ _ _ _ ->
                async.Return(
                    if random.NextDouble() < 0.5 then
                        Strategy.Hit
                    else
                        Strategy.Stand
                )
            )
        |> Strategy.addTargetingStrategy
            "Dice"
            (fun random _ _ _ candidates _ _ -> async.Return(List.randomChoiceWith random candidates))

    // Twenty pairs of games: a decider's draws that reached the game would
    // shift nearly every game, since the dice seat is asked questions in all
    // of them
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.zip3 genSeed genLineup genSeed))
            (fun (seed, lineup, extraSeed) ->
                let lineup =
                    ("Dicey", Strategy.Custom "Dice", Targeting.ChoosesExternally "Dice") :: lineup

                let extra = Random extraSeed

                // Answers exactly as dice does, because it asks dice first, and
                // only then draws from the Random it was asked with, a
                // different amount every time
                let drawing: Strategy.Decider = {
                    HitOrStand =
                        fun random strategy history round turn player others finished decks -> async {
                            let! choice =
                                dice.HitOrStand random strategy history round turn player others finished decks

                            for _ in 1 .. extra.Next 20 do
                                random.Next() |> ignore

                            return choice
                        }
                    Target =
                        fun random targeting history ask chooser candidates finished decks -> async {
                            let! target =
                                dice.Target random targeting history ask chooser candidates finished decks
                            random.NextBytes(Array.zeroCreate (extra.Next 20))
                            return target
                        }
                }

                let play decide =
                    Timeline.SimulateWithDecider (Random seed) decide lineup |> played

                play dice = play drawing
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

// How many calls the game's Random had made when a question was asked, and
// whether the last of them drew the seed of the question's Random
type private Cost = { CallsBefore: int; SeededByTheLast: bool }

// Every question costs the game's Random exactly one draw, made just before
// the question is asked and seeding the Random it is asked with, and nothing
// else draws that way: not a forced first hit, which asks nothing, and not a
// second chance nobody can take, which is discarded without asking anyone
[<Fact>]
let ``every question costs the game one draw, made just before it is asked`` () =
    let mutable discarded = 0

    // A hundred games, because a second chance nobody can take is rare enough
    // that fewer games too often discard none
    let property =
        Prop.forAll
            (Arb.fromGen genEngineGame)
            (fun (seed, lineup) ->
                let game = Recording seed
                let costs = ResizeArray<Cost>()

                // Checking the seed takes the first number from the question's
                // Random before the decider answers, which changes what a
                // random strategy answers but not the rules the game keeps
                let note (random: Random) =
                    let calls = game.Calls

                    costs.Add {
                        CallsBefore = calls
                        SeededByTheLast =
                            calls > 0
                            && match game.Made[calls - 1] with
                               | Unnamed drawn -> random.Next() = (Random drawn).Next()
                               | _ -> false
                    }

                let noting =
                    Strategy.Decider.Default |> beforeEachQuestion (fun random _ _ -> note random)

                let timeline = Timeline.SimulateWithDecider game noting lineup |> played

                discarded <-
                    discarded
                    + (timeline
                       |> List.where (fun instant -> instant.Event.IsSecondChanceDiscarded)
                       |> List.length)

                // Nobody names a draw here, so every parameterless Next the
                // game made is still Unnamed
                let seeds = game.Made |> Seq.where (fun call -> call.IsUnnamed) |> Seq.length

                Prop.label "the game made one parameterless draw per question and no more" (seeds = costs.Count)
                .&. Prop.label
                        "each question's Random was seeded by the game's last draw before it"
                        (costs |> Seq.forall (fun cost -> cost.SeededByTheLast))
                .&. Prop.label
                        "no two questions were asked on the same draw"
                        (costs
                         |> Seq.pairwise
                         |> Seq.forall (fun (before, after) -> before.CallsBefore < after.CallsBefore))
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

    Assert.True(discarded > 0, "no game discarded a second chance, so none checked that discarding one draws nothing")
