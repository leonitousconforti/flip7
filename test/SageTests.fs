module SageTests

open System
open System.Threading
open FSharp.Control
open Xunit

open Flip7
open Support

// Opponents whose strategies the default candidate grid holds and which ten
// games tell apart from every other candidate in it, so that a study of them
// names each one exactly and a replay predicts every answer they give: their
// targeting is an engine policy, which a replay keeps. Fewer games left Bob's
// HitUntilScore 24 tied with ChasesFlip7 (25, 6), which only the order of the
// grid broke
let private bob = "Bob", Strategy.HitUntilScore 24u, Targeting.PlaysSpitefully
let private carol = "Carol", Strategy.HitUntilNumCards 4u, Targeting.ChoosesRandomly
let private dave =
    "Dave", Strategy.MaximizesExpectedValue, Targeting.PlaysGreedily(0.3, 0.2)
let private erin = "Erin", Strategy.StandsAfterTurn 3u, Targeting.ChoosesRandomly
let private field: Lineup = [ bob; carol; dave; erin ]

// Sage's seat, whose questions the engine leaves to the table's decider
let private sageSeat =
    "Sage", Strategy.Custom "Sage", Targeting.ChoosesExternally "Sage"

// Who sits in Sage's seat in the games Sage studies, which have no Sage
let private standIn =
    "Sage", Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully

let private lineup: Lineup = sageSeat :: field

// Ten games of the field with the stand-in in Sage's seat, studied once for
// every test that needs them
let private study =
    lazy
        ([
            for seed in 1..10 -> Timeline.SimulateWith (Random(100 + seed)) (standIn :: field)
         ]
         |> Study.OfTimelines
         |> Async.RunSynchronously)

// Searches in the tests find a win within a few dozen replays, and the budget
// only bounds what a search that cannot costs
let private budget = 300

// Sage playing alone for its seat, knowing the field from the study
let private studiedSage () : Sage =
    Sage(Strategy.Decider.Default, "Sage", study = study.Value, budget = budget)

// ---- passing questions on ----

// Passes every question on to the decider, noting for each one asked of a seat
// the filter lets through the Random and the game so far it came with, and
// the answer the decider gave
let private recording
    (into: ResizeArray<Random * Instant seq>)
    (answers: ResizeArray<string>)
    (only: string -> bool)
    (decider: Strategy.Decider)
    : Strategy.Decider = {
    HitOrStand =
        fun random strategy history round turn player others finished decks -> async {
            let! choice =
                decider.HitOrStand random strategy history round turn player others finished decks

            if only player.Name then
                into.Add(random, history)
                answers.Add(string choice)

            return choice
        }
    Target =
        fun random targeting history ask chooser candidates finished decks -> async {
            let! target =
                decider.Target random targeting history ask chooser candidates finished decks

            if only chooser.Name then
                into.Add(random, history)
                answers.Add target.Name

            return target
        }
}

[<Fact>]
let ``Sage passes every other seat's question on with the Random and the game so far it came with`` () =
    let passed = ResizeArray<Random * Instant seq>()
    let decided = ResizeArray<string>()
    let opponents = Strategy.Decider.Default |> recording passed decided (fun _ -> true)

    let sage = Sage(opponents, "Sage", study = study.Value, budget = budget)

    let given = ResizeArray<Random * Instant seq>()
    let answered = ResizeArray<string>()
    let table = sage.Decider |> recording given answered (fun name -> name <> "Sage")

    Timeline.SimulateWithDecider (Random 7) table lineup |> played |> ignore

    // Sage searched, and its replays asked the opponents nothing: they were
    // asked exactly the questions the other seats were
    Assert.NotEmpty sage.Plans
    Assert.NotEmpty given
    Assert.Equal(given.Count, passed.Count)

    // Each with the very Random and game so far the engine gave Sage, and
    // their answers came back unchanged
    for (random, history), (random', history') in Seq.zip given passed do
        Assert.Same(random, random')
        Assert.Same(history, history')

    Assert.Equal<string seq>(decided, answered)

// ---- before the seed is pinned ----

// What a solved seat answers unaided, on dice of its own seeded as given
let private unaidedHitOrStand (seed: int) : Strategy.HitOrStandDecider =
    fun history round turn player others finished decks ->
        let strategy =
            match player.Strategy with
            | Strategy.Custom _ -> Strategy.MaximizesExpectedValue
            | strategy -> strategy

        Strategy.Decider.Default.HitOrStand (Random seed) strategy history round turn player others finished decks

let private unaidedTarget (seed: int) : Strategy.TargetDecider =
    fun history ask chooser candidates finished decks ->
        let targeting =
            match chooser.Targeting with
            | Targeting.ChoosesExternally _ -> Targeting.PlaysSpitefully
            | targeting -> targeting

        Strategy.Decider.Default.Target (Random seed) targeting history ask chooser candidates finished decks

// A protagonist who rolls dice and a supporter who declared a Custom strategy
let private solvedLineup: Lineup = [
    "Sage", Strategy.PlaysLikeAHuman 18u, Targeting.ChoosesExternally "Sage"
    "Ally", Strategy.Custom "Ally", Targeting.PlaysGreedily(0.5, 0.1)
    bob
    carol
]

// A solved seat's question as playSolved notes it: whose it was, how much of
// the game it was shown, whether it was a hit-or-stand question, how many
// answers it allowed, whether Sage answered as the seat would unaided,
// whether that unaided answer is an aim aiming at random on the same dice
// would not have given, and how many plans Sage had made before it and by
// its answer
type private Compared = {
    Seat: string
    Shown: int
    IsHitOrStand: bool
    Choices: int
    AsUnaided: bool
    UnlikeRandom: bool
    PlansBefore: int
    PlansAfter: int
}

// Plays a game with Sage answering for the protagonist and the supporter, each
// solved question asked with a Random of the test's, seeded by a draw from the
// one the engine gave, so that the test can roll the same dice. Returns the
// timeline and every solved question
let private playSolved (random: Random) (sage: Sage) : Instant list * Compared list =
    let asked = ResizeArray<Compared>()
    let solved = set [ "Sage"; "Ally" ]

    let table: Strategy.Decider = {
        HitOrStand =
            fun random strategy history round turn player others finished decks -> async {
                if solved.Contains player.Name then
                    let seed = random.Next()
                    let before = sage.Plans.Length

                    let! answer =
                        sage.Decider.HitOrStand (Random seed) strategy history round turn player others finished decks

                    let! unaided = unaidedHitOrStand seed history round turn player others finished decks

                    asked.Add {
                        Seat = player.Name
                        Shown = Seq.length history
                        IsHitOrStand = true
                        Choices = 2
                        AsUnaided = (answer = unaided)
                        UnlikeRandom = false
                        PlansBefore = before
                        PlansAfter = sage.Plans.Length
                    }

                    return answer
                else
                    return! sage.Decider.HitOrStand random strategy history round turn player others finished decks
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                if solved.Contains chooser.Name then
                    let seed = random.Next()
                    let before = sage.Plans.Length

                    let! answer =
                        sage.Decider.Target (Random seed) targeting history ask chooser candidates finished decks

                    let! unaided = unaidedTarget seed history ask chooser candidates finished decks

                    let! atRandom =
                        Strategy.Decider.Default.Target
                            (Random seed)
                            Targeting.ChoosesRandomly
                            history
                            ask
                            chooser
                            candidates
                            finished
                            decks

                    asked.Add {
                        Seat = chooser.Name
                        Shown = Seq.length history
                        IsHitOrStand = false
                        Choices = candidates.Length
                        AsUnaided = (answer.Name = unaided.Name)
                        UnlikeRandom = (unaided.Name <> atRandom.Name)
                        PlansBefore = before
                        PlansAfter = sage.Plans.Length
                    }

                    return answer
                else
                    return! sage.Decider.Target random targeting history ask chooser candidates finished decks
            }
    }

    let timeline = Timeline.SimulateWithDecider random table solvedLineup |> played
    timeline, List.ofSeq asked

[<Fact>]
let ``until the seed is pinned the solved seats answer unaided, and Sage searches at the first question after`` () =
    let sage =
        Sage(Strategy.Decider.Default, "Sage", supporters = [ "Ally" ], study = study.Value, budget = budget)

    let timeline, asked = playSolved (Random 2026) sage

    // How many instants the solver had seen when it pinned the seed: the
    // scan's first state is the solver before any instant
    let pinned =
        timeline
        |> List.scan Solver.Observe Solver.Start
        |> List.findIndex (fun solver -> (Solver.SeedsOf solver).IsPinned)

    let before, after =
        asked |> List.partition (fun question -> question.Shown < pinned)

    Assert.NotEmpty before

    for question in before do
        Assert.True(question.AsUnaided, $"the question shown {question.Shown} instants was not answered unaided")
        Assert.True((question.PlansAfter = 0), $"Sage searched by the question shown {question.Shown} instants")

    // An aim is never searched, so the solved seats answer unaided up to the
    // first hit-or-stand question once the seed is pinned, where Sage makes
    // its first plan
    let searched = after |> List.findIndex (fun question -> question.IsHitOrStand)

    for question in List.take searched after do
        Assert.True(question.AsUnaided, $"the aim shown {question.Shown} instants was not answered unaided")

    Assert.Equal(0, after[searched].PlansBefore)
    Assert.Equal(1, after[searched].PlansAfter)

// A game whose Random drew before the game began, as one dealt on the Random
// that just shuffled its lineup does, agrees with no seed, so this one draws
// once first. In it the solved seats aim at one of several players again and
// again, and the protagonist, who declared ChoosesExternally and so aims
// spitefully, does so where aiming at random on the same dice picks another
[<Fact>]
let ``in a game no fresh seed dealt, Sage answers unaided throughout`` () =
    let random = Random 2029
    random.Next() |> ignore

    let sage =
        Sage(Strategy.Decider.Default, "Sage", supporters = [ "Ally" ], study = study.Value)

    let _, asked = playSolved random sage

    Assert.Empty sage.Plans

    for question in asked do
        Assert.True(question.AsUnaided, $"the question shown {question.Shown} instants was not answered unaided")

    // A change to how the game draws can take the aims away. Four is a floor
    // under what the seed gives, so that the unaided aims are checked more
    // than once or twice
    let withAChoice =
        asked
        |> List.filter (fun question -> not question.IsHitOrStand && question.Choices > 1)

    Assert.True(
        withAChoice.Length >= 4,
        $"the solved seats aimed at one of several players {withAChoice.Length} times; pick a new seed"
    )

    Assert.True(
        withAChoice
        |> List.exists (fun question -> question.Seat = "Sage" && question.UnlikeRandom),
        "the protagonist never aimed where spite and a random pick differ; pick a new seed"
    )

// ---- what Sage learns of the players ----

// Sage follows a game only through the questions it is asked, so the answer to
// the game's last question counts only once Sage is shown the finished game,
// and then Sage reads everyone exactly as one shown the whole game at once
// does, since both take in the same instants in the same order
[<Fact>]
let ``shown the finished game, Sage reads everyone by all of it`` () =
    let sage = studiedSage ()
    let timeline = Timeline.SimulateWithDecider (Random 5) sage.Decider lineup |> played

    let read (sage: Sage) = [ for name, _, _ in lineup -> sage.ModelOf name, sage.AimingOf name ]

    let asked = read sage
    sage.Follow timeline
    let shown = Sage(Strategy.Decider.Default, "Sage", study = study.Value)
    shown.Follow timeline

    Assert.NotEqual<(PlayerModel option * AimingModel option) list>(asked, read sage)
    Assert.Equal<(PlayerModel option * AimingModel option) list>(read shown, read sage)

[<Fact>]
let ``studying the field names every opponent's strategy and targeting`` () =
    // Named with next to no doubt, rather than by the order of the grid
    for name, strategy, _ in field do
        let likeliest, probability =
            List.head (Study.ModelOf study.Value name).Value.Posterior

        Assert.Equal(strategy, likeliest)
        Assert.True(probability > 0.9, $"{name} is {likeliest} at {probability}")

    // So is how they aim, but for Dave's greedy targeting, which ten games
    // never tell from greedy at higher thresholds to bank at, so that it is
    // only among the likeliest
    for name, _, targeting in field do
        let posterior = (Study.AimingOf study.Value name).Value.Posterior
        let declared = posterior |> List.find (fst >> (=) (Aiming.Plays targeting)) |> snd
        Assert.Equal(snd (List.head posterior), declared)

        if name <> "Dave" then
            Assert.True(declared > 0.9, $"{name} aims {targeting} at {declared}")

// Before a game Sage knows everyone as it studied them. Sage follows a game
// only through the questions it is asked, so by the end of one it has added
// to that at most all of the decisions each opponent made in it, and of their
// aims at one of two or more players, and at least one of each where the game
// showed any, since it was asked something after each opponent's first.
// Shown the finished game, it has added every one of them
[<Fact>]
let ``Sage adds every decision and aim a game shows to what it studied`` () =
    let sage = studiedSage ()

    for name, _, _ in lineup do
        Assert.Equal(Study.ModelOf study.Value name, sage.ModelOf name)
        Assert.Equal(Study.AimingOf study.Value name, sage.AimingOf name)

    let timeline = Timeline.SimulateWithDecider (Random 5) sage.Decider lineup |> played
    let observations = observationsOf timeline

    let shown (name: string) =
        let decided =
            observations
            |> List.filter (fun observation -> observation.Name = name && observation.Choice.IsChoice1Of2)
            |> List.length

        let aimed =
            observations
            |> List.filter (fun aim -> aim.Name = name && aim.Choice.IsChoice2Of2 && aim.OtherPlayers.Length > 1)
            |> List.length

        decided, aimed

    // A change to how the game draws can leave the game showing nothing to add
    Assert.True(
        field |> List.forall (fun (name, _, _) -> fst (shown name) > 0)
        && field |> List.exists (fun (name, _, _) -> snd (shown name) > 0),
        "an opponent never decided, or nobody aimed at one of several players; pick a new seed"
    )

    for name, _, _ in field do
        let decided, aimed = shown name
        let studied = (Study.ModelOf study.Value name).Value.Observations
        let studiedAims = (Study.AimingOf study.Value name).Value.Aims
        Assert.InRange((sage.ModelOf name).Value.Observations, studied + min 1 decided, studied + decided)
        Assert.InRange((sage.AimingOf name).Value.Aims, studiedAims + min 1 aimed, studiedAims + aimed)

    // Shown the finished game, Sage has added every one of them
    sage.Follow timeline

    for name, _, _ in field do
        let decided, aimed = shown name
        let studied = (Study.ModelOf study.Value name).Value.Observations
        let studiedAims = (Study.AimingOf study.Value name).Value.Aims
        Assert.Equal(studied + decided, (sage.ModelOf name).Value.Observations)
        Assert.Equal(studiedAims + aimed, (sage.AimingOf name).Value.Aims)

// At a table of two, the first 26 instants of seed 13 end with Bob freezing
// himself, the only one left in the round, and before it he never aims at
// all, so that a game quit there shows him aim only where he had no choice.
// Neither a study of it nor a Sage following it counts that aim, while the
// whole game counts every aim he made with a choice of players
[<Fact>]
let ``an aim with no choice of players shows nothing of how a player aims`` () =
    let lineup = [
        "Ann", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
        "Bob", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
    ]

    let whole = Timeline.SimulateWith (Random 13) lineup |> played
    let quit = List.take 26 whole

    let aimsOf (timeline: Instant list) =
        observationsOf timeline
        |> List.filter (fun aim -> aim.Name = "Bob" && aim.Choice.IsChoice2Of2)

    // A change to how the game draws can move the seed off its road
    let forced = aimsOf quit

    Assert.True(
        (List.last quit).Event = Froze("Bob", "Bob")
        && not forced.IsEmpty
        && forced |> List.forall (fun aim -> aim.OtherPlayers.Length = 1),
        "the quit game does not end with Bob's forced aims alone; pick a new seed"
    )

    let evidence = Inference.AimEvidence "Bob" forced
    Assert.Equal(0, evidence.Aims)
    Assert.Empty evidence.Names
    Assert.Empty(Inference.FitAims forced)

    // What a Sage shown a game makes of how Bob aims
    let following (timeline: Instant list) : AimingModel option =
        let sage = Sage(Strategy.Decider.Default, "Ann")
        sage.Follow timeline
        sage.AimingOf "Bob"

    let quitStudy = Study.OfTimelines [ AsyncSeq.ofSeq quit ] |> Async.RunSynchronously
    Assert.Equal(None, Study.AimingOf quitStudy "Bob")
    Assert.Equal(None, following quit)

    let chosen =
        aimsOf whole
        |> List.where (fun aim -> aim.OtherPlayers.Length > 1)
        |> List.length

    let wholeStudy =
        Study.OfTimelines [ AsyncSeq.ofSeq whole ] |> Async.RunSynchronously
    Assert.True(chosen > 0, "Bob never aimed at one of several players in the whole game; pick a new seed")
    Assert.Equal(chosen, (Study.AimingOf wholeStudy "Bob").Value.Aims)
    Assert.Equal(chosen, (following whole).Value.Aims)

// Dave hits until a card is 35% likely to bust him, between two points of the
// default grid, so that no study over it could ever name his strategy, but a
// study over the grid and his strategy does, and a Sage given that study
// scores the decisions of the game it plays over the same grid
[<Fact>]
let ``a study over a grid of one's own names a strategy between the default grid's points`` () =
    let between = Strategy.HitUntilBustProbability 0.35

    let fieldWithDaveBetween =
        field
        |> List.map (fun (name, strategy, targeting) -> name, (if name = "Dave" then between else strategy), targeting)

    let games = [
        for seed in 1..10 ->
            Timeline.SimulateWith (Random(300 + seed)) (standIn :: fieldWithDaveBetween)
            |> played
    ]

    let studied (over: Timeline seq -> Async<Study>) : Study =
        games |> List.map AsyncSeq.ofSeq |> over |> Async.RunSynchronously

    let plain = studied Study.OfTimelines
    let own = studied (Study.OfTimelinesWith(Inference.DefaultCandidates @ [ between ]))
    let model = (Study.ModelOf own "Dave").Value

    Assert.NotEqual(between, Inference.MostLikely (Study.ModelOf plain "Dave").Value)
    Assert.Equal(between, Inference.MostLikely model)
    Assert.Equal(Inference.DefaultCandidates.Length + 1, model.Posterior.Length)

    let sage = Sage(Strategy.Decider.Default, "Sage", study = own, budget = budget)

    Timeline.SimulateWithDecider (Random 5) sage.Decider (sageSeat :: fieldWithDaveBetween)
    |> played
    |> ignore

    let followed = (sage.ModelOf "Dave").Value
    Assert.True(followed.Observations > model.Observations)
    Assert.Equal(between, Inference.MostLikely followed)
    Assert.Equal(model.Posterior.Length, followed.Posterior.Length)

// Games read lazily off one Random are the games that Random plays one after
// another only if the study reads each once, in order. Every player is then
// scored exactly as fitting the games' questions scores them, since both sum
// the same terms in the same order
[<Fact>]
let ``a study reads its games once each, in order, and scores them as Inference does`` () =
    let table = standIn :: field
    let shared = Random 7

    let lazily =
        [ for _ in 1..3 -> Timeline.SimulateWith shared table ]
        |> Study.OfTimelines
        |> Async.RunSynchronously

    let inTurn = Random 7
    let games = [ for _ in 1..3 -> Timeline.SimulateWith inTurn table |> played ]

    let observations =
        games |> List.map AsyncSeq.ofSeq |> Observation.FromTimelines |> played

    // Everyone decided and someone aimed at one of several players, so the
    // comparisons are not all of nothing with nothing
    let models = Inference.Fit observations
    let aimings = Inference.FitAims observations
    Assert.Equal(table.Length, models.Length)
    Assert.NotEmpty aimings

    for name, _, _ in table do
        Assert.Equal(models |> List.tryFind (fun model -> model.Name = name), Study.ModelOf lazily name)
        Assert.Equal(aimings |> List.tryFind (fun model -> model.Name = name), Study.AimingOf lazily name)

// ---- playing a plan ----

// A question a solved seat was asked: the answer Sage gave, as a plan records
// it, everyone an aim could have gone to, the events of the game so far, how
// many plans Sage had made before the question and after it, and whether the
// game was edited since the solved seats were last asked anything
type private Asked = {
    Answer: Answer
    Candidates: string list
    Shown: Event array
    Before: int
    After: int
    Edited: bool
}

// Passes every question to Sage, noting each one a solved seat is asked
let private asking (solved: Set<string>) (sage: Sage) (asked: ResizeArray<Asked>) : Strategy.Decider =
    let seen = ref 0

    let note (history: Instant seq) (before: int) (candidates: string list) (answer: Answer) =
        let instants = Array.ofSeq history

        asked.Add {
            Answer = answer
            Candidates = candidates
            Shown = instants |> Array.map (fun instant -> instant.Event)
            Before = before
            After = sage.Plans.Length
            Edited =
                instants[min seen.Value instants.Length ..]
                |> Array.exists (fun instant -> instant.Event.IsEdited)
        }

        seen.Value <- instants.Length

    {
        HitOrStand =
            fun random strategy history round turn player others finished decks -> async {
                let before = sage.Plans.Length

                let! choice =
                    sage.Decider.HitOrStand random strategy history round turn player others finished decks

                if solved.Contains player.Name then
                    note history before [] (Answer.HitOrStand(player.Name, round, turn, player.Hand, choice))

                return choice
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                let before = sage.Plans.Length

                let! target =
                    sage.Decider.Target random targeting history ask chooser candidates finished decks

                if solved.Contains chooser.Name then
                    let names = candidates |> List.map (fun candidate -> candidate.Name)
                    note history before names (Answer.Aim(chooser.Name, ask, target.Name))

                return target
            }
    }

// The protagonist's final score less the best of everyone else's
let private marginOf (protagonist: string) (timeline: Instant list) : int =
    let mine, others =
        (List.last timeline).Players
        |> List.partition (fun player -> player.Name = protagonist)

    int (List.exactlyOne mine).FirmScore
    - (others |> List.map (fun player -> int player.FirmScore) |> List.max)

// The answers the named game gave from a plan's question on are the plan's own
let private follows (game: string) (plan: Plan) (asked: Asked seq) : unit =
    let answers = asked |> Seq.map (fun question -> question.Answer) |> List.ofSeq
    let planned = plan.Answers.Length

    Assert.True(
        answers.Length >= planned,
        $"{game}: the plan has {planned} answers, but the game gave only {answers.Length}"
    )

    let given = List.skip (answers.Length - planned) answers

    match
        List.zip plan.Answers given
        |> List.tryFindIndex (fun (answer, gave) -> answer <> gave)
    with
    | Some index ->
        Assert.Fail $"{game}: answer {index} of the plan is {plan.Answers[index]}, but the game gave {given[index]}"
    | None -> ()

// What a game made of the rules for playing a plan. The plan's answer is
// given, with no search, while the game since the plan's question has gone as
// the plan foresees and asks what the plan expects next: the same seat about
// the same hand on the same turn of the same round, or the same seat the same
// ask with the planned target among the candidates. Anywhere else the plan is
// dropped, as it is once the game is edited, and once Sage has made a plan,
// so that the seed is pinned, it searches at the next hit-or-stand question,
// which the new plan starts with. An aim is never searched. Problems are
// every way the solved seats' answers broke those rules, and Strays counts the
// hit-or-stand questions the plan still expected next once the game had gone
// otherwise than it foresaw, where only the events show that the plan is no
// longer the game's. The rules mirror how Sage follows a game and answers
// (follow and the decider in src/Sage.fs), so a change to those must be made
// here too
type private Audit = { Problems: string list; Strays: int }

let private audit (plans: Plan list) (asked: Asked seq) : Audit =
    let plans = Array.ofList plans
    let problems = ResizeArray<string>()
    let mutable strays = 0
    let mutable line: Answer list = []
    let mutable foreseen: Event array = [||]
    let mutable forkedAt = 0

    for question in asked do
        if question.Edited then
            line <- []

        let made = question.After - question.Before
        let since = question.Shown |> Array.skip (min forkedAt question.Shown.Length)

        let asForeseen =
            since.Length <= foreseen.Length
            && Array.forall2 (=) since (Array.take since.Length foreseen)

        let next =
            match question.Answer, line with
            | Answer.HitOrStand(seat, round, turn, hand, _), Answer.HitOrStand(seat', round', turn', hand', _) :: _ ->
                (seat, round, turn, hand) = (seat', round', turn', hand')
            | Answer.Aim(seat, ask, _), Answer.Aim(seat', ask', target) :: _ ->
                seat = seat' && ask = ask' && List.contains target question.Candidates
            | _ -> false

        if next && not asForeseen && question.Answer.IsHitOrStand then
            strays <- strays + 1

        match question.Answer with
        | _ when next && asForeseen ->
            if made <> 0 then
                problems.Add $"{question.Answer}, searched though the plan expected the question"

            if question.Answer <> List.head line then
                problems.Add $"{question.Answer}, where the plan has {List.head line}"

            line <- List.tail line
        | Answer.Aim _ ->
            if made <> 0 then
                problems.Add $"{question.Answer}, searched at an aim"

            line <- []
        | Answer.HitOrStand _ when question.After = 0 -> line <- []
        | Answer.HitOrStand _ ->
            match made, plans[question.After - 1].Answers with
            | 1, first :: rest when first = question.Answer ->
                line <- rest
                foreseen <- Array.ofList plans[question.After - 1].Foresees
                forkedAt <- question.Shown.Length
            | 1, _ ->
                problems.Add $"{question.Answer}, though the plan made there starts otherwise"
                line <- []
            | _ ->
                problems.Add $"{question.Answer}, after making {made} plans where the plan was left"
                line <- []

    { Problems = List.ofSeq problems; Strays = strays }

// The solved seats' answers in the named game broke none of the rules for
// playing a plan
let private playedByTheRules (game: string) (plans: Plan list) (asked: Asked seq) : unit =
    let problems = (audit plans asked).Problems
    Assert.True(List.isEmpty problems, $"""{game}: {String.concat "; " problems}""")

// Against opponents it has fitted exactly, a plan foresees the whole game:
// every seed below is searched once, at the first question Sage is asked
// once the seed is pinned, and the live game follows the plan to the end.
// Sage won every one of these seeds when the test was written, so needing
// eight of the ten leaves room for a change to how the game draws to cost a
// game or two, while a search that stopped finding wins fails
[<Fact>]
let ``once the seed is pinned the live game follows Sage's plan, which wins most games`` () =
    let won =
        [ 1..10 ]
        |> List.filter (fun seed ->
            let sage = studiedSage ()
            let asked = ResizeArray<Asked>()

            let timeline =
                Timeline.SimulateWithDecider (Random seed) (asking (set [ "Sage" ]) sage asked) lineup
                |> played

            let game = $"seed {seed}"
            let plan = Assert.Single sage.Plans
            follows game plan asked
            playedByTheRules game sage.Plans asked
            Assert.Equal(plan.Margin, marginOf "Sage" timeline)
            plan.Won
        )

    Assert.True(won.Length >= 8, $"won {won.Length} of 10")

// Bob and Carol play as the field's players of the same names do, but under
// ChoosesExternally and taking sides: for Dave and against Sage, and otherwise
// spitefully. Sage, which used to replay anyone who declared ChoosesExternally
// as spiteful, fits how they aim from twenty games of the table
let private partisan =
    Aiming.TakesSides(Some "Dave", Some "Sage", Targeting.PlaysSpitefully)

let private partisans: Strategy.Decider =
    Strategy.Decider.Default
    |> Strategy.addTargetingStrategy "Partisan" (Inference.TargetDecider partisan)

let private partisanField: Lineup =
    let takingSides (name, strategy, _) =
        name, strategy, Targeting.ChoosesExternally "Partisan"

    [ takingSides bob; takingSides carol; dave; erin ]

let private partisanStudy =
    lazy
        ([
            for seed in 1..20 -> Timeline.SimulateWithDecider (Random(200 + seed)) partisans (standIn :: partisanField)
         ]
         |> Study.OfTimelines
         |> Async.RunSynchronously)

// Every seed below is searched once and the live game follows the plan to the
// end, though in each of them Bob or Carol aims somewhere spite would not, so
// that a replay of them as spiteful would have strayed from the live game
[<Fact>]
let ``Sage replays players who take sides as it fitted them, and its plan foresees them`` () =
    for name in [ "Bob"; "Carol" ] do
        let likeliest, probability =
            List.head (Study.AimingOf partisanStudy.Value name).Value.Posterior

        Assert.Equal(partisan, likeliest)
        Assert.True(probability > 0.9, $"{name} aims {likeliest} at {probability}")

    for seed in 1..5 do
        let sage = Sage(partisans, "Sage", study = partisanStudy.Value, budget = budget)
        let asked = ResizeArray<Asked>()

        let timeline =
            Timeline.SimulateWithDecider (Random seed) (asking (set [ "Sage" ]) sage asked) (sageSeat :: partisanField)
            |> played

        let game = $"seed {seed}"
        let plan = Assert.Single sage.Plans
        follows game plan asked
        playedByTheRules game sage.Plans asked
        Assert.Equal(plan.Margin, marginOf "Sage" timeline)

        let unlikeSpite =
            observationsOf timeline
            |> List.filter (fun aim ->
                (aim.Name = "Bob" || aim.Name = "Carol")
                && aim.Choice.IsChoice2Of2
                && Inference.ProbabilityOfTarget (Aiming.Plays Targeting.PlaysSpitefully) aim = 0.0
            )

        Assert.True(not unlikeSpite.IsEmpty, $"{game}: Bob and Carol aimed as spite would; pick a new seed")

[<Fact>]
let ``a replay aims as declared, or as Sage made of one who aims as they please, never at random if they take sides``
    ()
    =
    // Evidence over one name whose posterior is as given. The prior subtracts
    // log (n * n + n) from the weight of each way of taking sides among n
    // names, which over one name is log 2, so adding log 2 here cancels it
    let model (posterior: (Aiming * float) list) : AimingEvidence option =
        Some {
            Name = "Bob"
            Aims = 10
            Names = [ "Carol" ]
            LogLikelihoods =
                posterior
                |> List.map (fun (aiming, probability) ->
                    match aiming with
                    | Aiming.Plays _ -> aiming, log probability
                    | Aiming.TakesSides _ -> aiming, log probability + log 2.0
                )
        }

    let spite = Targeting.PlaysSpitefully
    let external = Targeting.ChoosesExternally "Bob"
    let greedy = Targeting.PlaysGreedily(0.4, 0.2)
    let random = Aiming.Plays Targeting.ChoosesRandomly
    let sidesAtRandom = Aiming.TakesSides(Some "Carol", None, Targeting.ChoosesRandomly)
    let sides = Aiming.TakesSides(Some "Carol", None, spite)

    // Never seen aiming: as declared, or spitefully for ChoosesExternally
    Assert.Equal(Aiming.Plays greedy, Replayed.Aiming None greedy)
    Assert.Equal(Aiming.Plays spite, Replayed.Aiming None external)

    // A declared policy whatever the aims seen of it suggest, even one off the
    // grid, since the engine plays it on the replay's own dice: a few aims at
    // random can look spiteful
    let spiteful = model [ Aiming.Plays spite, 0.9; random, 0.1 ]
    let offGrid = Targeting.PlaysGreedily(0.25, 0.15)
    Assert.Equal(random, Replayed.Aiming spiteful Targeting.ChoosesRandomly)
    Assert.Equal(Aiming.Plays offGrid, Replayed.Aiming spiteful offGrid)
    Assert.Equal(Aiming.Plays spite, Replayed.Aiming spiteful external)

    // One who aims as they please aims as the posterior's head, which of
    // equals is the first
    let tied =
        model [
            Aiming.Plays(Targeting.PlaysGreedily(0.3, 0.2)), 0.4
            Aiming.Plays greedy, 0.4
            sides, 0.2
        ]

    Assert.Equal(Aiming.Plays(Targeting.PlaysGreedily(0.3, 0.2)), Replayed.Aiming tied external)
    Assert.Equal(Inference.MostLikelyAiming(Inference.AimModelFrom tied.Value), Replayed.Aiming tied external)

    // Taking sides at random is replayed as the likeliest aiming that decides,
    // but a plain random chooser stays random, since they may roll the dice
    // the engine hands them
    let atRandom = model [ sidesAtRandom, 0.6; random, 0.3; sides, 0.1 ]
    Assert.Equal(sides, Replayed.Aiming atRandom external)
    Assert.Equal(random, Replayed.Aiming (model [ random, 0.7; sides, 0.3 ]) external)

    // Where no aiming decides, taking sides at random stays the likeliest, as
    // there is nothing to replace it with
    Assert.Equal(sidesAtRandom, Replayed.Aiming (model [ sidesAtRandom, 0.7; random, 0.3 ]) external)

// Sage seated first at twelve, before eleven people who play under a Custom
// label and aim on the given policy, which the engine plays for them
let private twelveAt (targeting: Targeting) : Lineup =
    sageSeat
    :: [
        for name in "Bob Carol Dave Erin Fay Gus Hal Ida Jo Kit Lou".Split ' ' ->
            name, Strategy.Custom "Person", targeting
    ]

// The opponents' decider for them, which plays their label as maximizing
// expected value on the dice the engine hands them
let private maximizers: Strategy.Decider =
    Strategy.Decider.Default
    |> Strategy.addHitOrStandStrategy
        "Person"
        (fun random -> Strategy.Decider.Default.HitOrStand random Strategy.MaximizesExpectedValue)

// Seated first at twelve, Sage is first asked to hit or stand once the twelve
// opening cards have pinned the seed and before anyone else has been, and
// with no study it has never seen the others decide. A budget of one replay
// makes the plan what Sage answers unaided, so the plan foresees the game
// only if the replay plays everyone as they play. That is as maximizing
// expected value, which is how the opponents' decider plays their label,
// rather than on the label, which the engine cannot play, or as the grid's
// first candidate. And it is on the policy they declared, with the dice the
// live game rolls, rather than as the aims Sage saw before its search
// suggest: in seed 12 their random aims fit as another policy, and in seed 1
// their greedy aims off the grid of policies Sage fits can never fit as the
// policy they are
[<Theory>]
[<InlineData("random", 12)>]
[<InlineData("greedy", 1)>]
let ``Sage replays a player it has never seen decide as maximizing expected value, and a targeting policy as it was declared``
    (policy: string, seed: int)
    =
    let targeting =
        match policy with
        | "random" -> Targeting.ChoosesRandomly
        | _ -> Targeting.PlaysGreedily(0.25, 0.15)

    let sage = Sage(maximizers, "Sage", budget = 1)
    let asked = ResizeArray<Asked>()
    let first = ref []

    let noting =
        asking (set [ "Sage" ]) sage asked
        |> beforeEachQuestion (fun _ history question ->
            match question with
            | HitOrStandQuestion(Player = player) when player.Name = "Sage" && first.Value.IsEmpty ->
                first.Value <- List.ofSeq history
            | _ -> ()
        )

    let timeline =
        Timeline.SimulateWithDecider (Random seed) noting (twelveAt targeting) |> played

    let game = $"seed {seed}"

    // A change to how the game draws can move a seed off its road
    match asked |> Seq.find (fun question -> question.Answer.IsHitOrStand) with
    | { Answer = Answer.HitOrStand(_, 1u, 2u, _, _); After = 1 } -> ()
    | question -> Assert.Fail $"{game}: Sage did not search at its first question: {question}; pick a new seed"

    let seen = observationsOf first.Value

    Assert.True(
        seen |> List.forall (fun observation -> observation.Choice.IsChoice2Of2),
        $"{game}: Sage saw a decision before its search; pick a new seed"
    )

    let fitted =
        seen |> List.filter (fun aim -> aim.Name <> "Sage") |> Inference.FitAims

    Assert.True(
        fitted
        |> List.exists (fun model -> Inference.MostLikelyAiming model <> Aiming.Plays targeting),
        $"{game}: every aim before the search fits as declared; pick a new seed"
    )

    let plan = Assert.Single sage.Plans
    follows game plan asked
    Assert.Equal(plan.Margin, marginOf "Sage" timeline)

// Passes every question on, counting the ones a seat other than these is asked
let private counting (solved: Set<string>) (count: int ref) (decider: Strategy.Decider) : Strategy.Decider =
    decider
    |> beforeEachQuestion (fun _ _ question ->
        if not (solved.Contains question.Asked.Name) then
            count.Value <- count.Value + 1
    )

// People at a real table play under Custom labels and aim as they please,
// both left to the opponents' decider, and Sage knows them only by name.
// These play and aim as the field's players of the same names do, on the dice
// the engine hands them, so that Erin aims at random as the study saw her
let private people: Strategy.Decider =
    Strategy.Decider.Default
    |> Strategy.addHitOrStandStrategy
        "Person"
        (fun random history round turn player others finished decks ->
            let _, strategy, _ = field |> List.find (fun (name, _, _) -> name = player.Name)
            Strategy.Decider.Default.HitOrStand random strategy history round turn player others finished decks
        )
    |> Strategy.addTargetingStrategy
        "Person"
        (fun random history ask chooser candidates finished decks ->
            let _, _, targeting = field |> List.find (fun (name, _, _) -> name = chooser.Name)
            Strategy.Decider.Default.Target random targeting history ask chooser candidates finished decks
        )

// Erin, seated first, and Bob have finished the round by the time Sage first
// searches seed 10, so a replay that did not model the players who had
// finished the round would hand the engine a Custom strategy it cannot play
[<Fact>]
let ``Sage replays people by what it made of them, never asking the opponents' decider`` () =
    let lineup = [
        "Erin", Strategy.Custom "Person", Targeting.ChoosesExternally "Person"
        sageSeat
        "Bob", Strategy.Custom "Person", Targeting.ChoosesExternally "Person"
        carol
        dave
    ]

    let passed = ref 0
    let toPeople = people |> counting Set.empty passed
    let sage = Sage(toPeople, "Sage", study = study.Value, budget = budget)

    // Who had finished the round at each question Sage was asked to hit or
    // stand before it made a plan, the last of which it made the plan at
    let finished = ref []
    let asked = ResizeArray<Asked>()
    let live = ref 0

    let atTable =
        asking (set [ "Sage" ]) sage asked
        |> counting (set [ "Sage" ]) live
        |> beforeEachQuestion (fun _ _ question ->
            match question with
            | HitOrStandQuestion(Player = player; Finished = out) when player.Name = "Sage" && sage.Plans.IsEmpty ->
                finished.Value <- out |> List.map (fun player -> player.Name) |> List.sort
            | _ -> ()
        )

    let timeline = Timeline.SimulateWithDecider (Random 10) atTable lineup |> played

    // A change to how the game draws can move the seed off its road
    Assert.True((finished.Value = [ "Bob"; "Erin" ]), $"{finished.Value} had finished at the search; pick a new seed")

    Assert.Equal(live.Value, passed.Value)
    let plan = Assert.Single sage.Plans
    follows "seed 10" plan asked
    playedByTheRules "seed 10" sage.Plans asked
    Assert.Equal(plan.Margin, marginOf "Sage" timeline)

// People who roll dice of their own, which no seed gives away, and aim at
// random on them, so that Sage's plans break whenever they stray from what it
// made of them
let private strays (seed: int) : Strategy.Decider =
    let dice = Random(1_000_000 + seed)

    Strategy.Decider.Default
    |> Strategy.addHitOrStandStrategy
        "Stray"
        (fun _ history round turn player others finished decks ->
            let strategy = Strategy.PlaysLikeAHuman(if player.Name = "Fay" then 14u else 22u)
            Strategy.Decider.Default.HitOrStand dice strategy history round turn player others finished decks
        )
    |> Strategy.addTargetingStrategy "Stray" (fun _ -> Strategy.Decider.Default.Target dice Targeting.ChoosesRandomly)

// The people stray, and some of the time the game still asks Sage what its
// plan expects next, so that only the events show the plan is no longer the
// game's
[<Fact>]
let ``against people who stray, Sage plays its plan only while the game goes as the plan foresees`` () =
    let lineup = [
        sageSeat
        "Fay", Strategy.Custom "Stray", Targeting.ChoosesExternally "Stray"
        "Ally", Strategy.Custom "Ally", Targeting.ChoosesExternally "Ally"
        "Gus", Strategy.Custom "Stray", Targeting.ChoosesExternally "Stray"
        carol
    ]

    let asked = ResizeArray<Asked>()

    let sage =
        Sage(strays 1, "Sage", supporters = [ "Ally" ], study = study.Value, budget = budget)

    Timeline.SimulateWithDecider (Random 1) (asking (set [ "Sage"; "Ally" ]) sage asked) lineup
    |> played
    |> ignore

    // Sage searched again each time it noticed
    Assert.True(sage.Plans.Length > 1, "Sage made a single plan")

    Assert.True(
        (audit sage.Plans asked).Strays > 0,
        "the game never strayed where the plan expected the next question; pick a new seed"
    )

    playedByTheRules "seed 1" sage.Plans asked

// ---- new games and edits ----

// Shown only the opening of the first game, Sage has seen fewer instants than
// the second game's first question shows it, so only the identity of the
// second game's TableSeated, not how much of a game Sage has seen, tells the
// two games apart
[<Fact>]
let ``a new game starts afresh, keeping only what Sage studied`` () =
    let fresh = studiedSage ()
    let used = studiedSage ()

    let opening = Timeline.SimulateWithDecider (Random 3) used.Decider lineup |> played
    Assert.NotEmpty used.Plans
    used.Follow(List.take 2 opening)

    let firstShown = ref None

    let noting =
        used.Decider
        |> beforeEachQuestion (fun _ history _ ->
            if firstShown.Value.IsNone then
                firstShown.Value <- Some(Seq.length history)
        )

    let again = Timeline.SimulateWithDecider (Random 4) noting lineup |> played
    Assert.True(
        firstShown.Value.Value >= 2,
        $"the second game's first question showed {firstShown.Value.Value} instants, which the length check tells apart"
    )

    let first = Timeline.SimulateWithDecider (Random 4) fresh.Decider lineup |> played
    Assert.Equal<Instant list>(first, again)
    Assert.Equal<Plan list>(fresh.Plans, used.Plans)

    for name, _, _ in lineup do
        Assert.Equal(fresh.ModelOf name, used.ModelOf name)
        Assert.Equal(fresh.AimingOf name, used.AimingOf name)

// Plays a game up to the first hit-or-stand question the trigger picks, shown
// every one before it is asked, edits the table there as the play editor does,
// moving a card from the deck to the discards, and continues the game on its
// own Random, past the question's draw, as the editor's fork continues it:
// with every active player's turns counted one short of the question's, so
// that the player is asked about the turn again. Returns the game up to and
// including the Edited instant, the continuation, every question the solved
// seats were asked, and how many plans Sage had made by the edit
let private edited
    (seed: int)
    (lineup: Lineup)
    (solved: Set<string>)
    (sage: Sage)
    (trigger: uint -> uint -> Player -> Player list -> bool)
    : Instant list * Instant list * Asked list * int =
    let random = Random seed
    let asked = ResizeArray<Asked>()
    let table = asking solved sage asked

    let forking =
        table
        |> beforeEachQuestion (fun _ history question ->
            match question with
            | HitOrStandQuestion(_, round, turn, player, others, finished, decks) when trigger round turn player others ->
                raise (Games.Forked(List.ofSeq history, round, turn, player, others, finished, decks))
            | _ -> ()
        )

    let history, round, turn, player, others, finished, (deck, discards) =
        try
            Timeline.SimulateWithDecider random forking lineup |> played |> ignore
            failwith "the game ended before the edit"
        with Games.Forked(history, round, turn, player, others, finished, decks) ->
            history, round, turn, player, others, finished, decks

    let plans = sage.Plans.Length
    let active = player :: others
    let card = deck |> Map.findKey (fun _ count -> count > 0u)

    let edit = {
        Event = Edited player.Name
        Players = active @ finished
        Deck = Deck.Decrement deck card
        Discards = Deck.Increment discards card
    }

    let before = history @ [ edit ]

    let continued =
        Timeline.ContinueWith
            random
            table
            before
            ((List.head history).Players |> List.map (fun player -> player.Name))
            round
            (active |> List.map (fun player -> player.Name, turn - 1u) |> Map.ofList)
            active
            finished
            (edit.Deck, edit.Discards)
        |> played

    before, continued, List.ofSeq asked, plans

// Seated so that the supporter and Erin open the second round, and edited at
// the first opponent asked on turn 3 of round 2 after Erin was asked on it,
// with Erin still in the round. Erin hit on her third turn, and the editor
// counts everyone still in the round as having taken two turns, so she is
// asked about her third turn again and hits, where counting the round's
// events alone would make it her fourth, on which she stands. Observation
// counts her turns as the engine does, so the one plan Sage makes after the
// edit foresees the rest of the game, where a replay counting the events
// alone would have her stand, and Sage would search again
[<Fact>]
let ``a game continued after an edit is played legally, and Sage's plan still foresees it`` () =
    let lineup = [
        sageSeat
        "Ally", Strategy.Custom "Ally", Targeting.ChoosesExternally "Ally"
        erin
        bob
        carol
        dave
    ]

    let solved = set [ "Sage"; "Ally" ]

    let sage =
        Sage(Strategy.Decider.Default, "Sage", supporters = [ "Ally" ], study = study.Value, budget = budget)

    let erinAsked = ref false

    let trigger round turn (player: Player) (others: Player list) =
        if (round, turn) = (2u, 3u) then
            let edit =
                erinAsked.Value
                && not (solved.Contains player.Name)
                && others |> List.exists (fun other -> other.Name = "Erin")

            erinAsked.Value <- erinAsked.Value || player.Name = "Erin"
            edit
        else
            false

    let before, continued, asked, plans = edited 1 lineup solved sage trigger
    let whole = before @ continued

    // Erin's decisions in the second round, before the edit and after it, as
    // Observation numbers them
    let erinsSecondRound (instants: Instant list) =
        observationsOf instants
        |> List.filter (fun observation ->
            observation.Name = "Erin"
            && observation.Round = 2u
            && observation.Choice.IsChoice1Of2
        )
        |> List.map (fun observation -> observation.Turn, observation.Choice)

    let beforeEdit = erinsSecondRound before
    let afterEdit = erinsSecondRound whole |> List.skip beforeEdit.Length
    Assert.Equal((3u, Choice1Of2 Strategy.Hit), List.last beforeEdit)
    Assert.Equal((3u, Choice1Of2 Strategy.Hit), List.head afterEdit)

    // A study of the edited game scores Erin's decisions with those turns
    let wholeStudy =
        Study.OfTimelines [ AsyncSeq.ofSeq whole ] |> Async.RunSynchronously
    let erinsDecisions =
        observationsOf whole
        |> List.filter (fun observation -> observation.Name = "Erin")

    Assert.Equal(
        Some(Inference.ModelFrom(Inference.EvidenceWith 0.1 Inference.DefaultCandidates "Erin" erinsDecisions)),
        Study.ModelOf wholeStudy "Erin"
    )

    // Sage had pinned the seed and searched before the edit, and searched
    // once more after it
    Assert.True(plans > 0, "Sage made no plan before the edit; pick a new seed")
    Assert.Equal(plans + 1, sage.Plans.Length)
    let plan = List.last sage.Plans
    follows "seed 1" plan asked
    playedByTheRules "seed 1" sage.Plans asked
    Assert.Equal(plan.Margin, marginOf "Sage" continued)

// What was left of the plan Sage was playing after the questions: the plan
// made by the last of them, less the answers it had given, which are those
// of every question with as many plans made by its answer, the plan's own
// question among them
let private remainingOf (plans: Plan list) (asked: Asked list) : Answer list =
    let made = (List.last asked).After
    let given = asked |> List.filter (fun question -> question.After = made)
    plans[made - 1].Answers |> List.skip given.Length

// Edited at an opponent's question once Sage has a plan, on a turn Sage has
// yet to be asked about, so that its next question is the one its plan asks
// next, though the plan foresaw the cards from before the edit
[<Fact>]
let ``an edit ends the plan being played, and Sage searches the edited game at its next question`` () =
    let sage = studiedSage ()
    let sageAsked = ref (0u, 0u)

    let trigger round turn (player: Player) (others: Player list) =
        if player.Name = "Sage" then
            sageAsked.Value <- (round, turn)
            false
        else
            not sage.Plans.IsEmpty
            && others |> List.exists (fun other -> other.Name = "Sage")
            && sageAsked.Value < (round, turn)

    let _, continued, asked, plans = edited 1 lineup (set [ "Sage" ]) sage trigger

    let question (answer: Answer) =
        match answer with
        | Answer.HitOrStand(seat, round, turn, hand, _) -> Some(seat, round, turn, hand)
        | Answer.Aim _ -> None

    // Sage's first question after the edit is the one left of its plan asks
    // next
    let next = asked |> List.findIndex (fun question -> question.Edited)
    let left = remainingOf sage.Plans (List.take next asked)
    Assert.True(
        (question asked[next].Answer).IsSome,
        "Sage's first question after the edit was an aim; pick a new seed"
    )
    Assert.Equal(question (List.head left), question asked[next].Answer)

    // and Sage searched there all the same
    Assert.Equal(plans, asked[next].Before)
    Assert.Equal(plans + 1, asked[next].After)
    let plan = List.last sage.Plans
    follows "seed 1" plan asked
    playedByTheRules "seed 1" sage.Plans asked
    Assert.Equal(plan.Margin, marginOf "Sage" continued)

// ---- searches ----

// A game from its TableSeated with the solved seats at nothing and the field's
// first three already at the given score, so that few lines win and a search
// has to look hard
let private behind (score: uint) (solved: string list) (seed: int) (decider: Strategy.Decider) : Timeline =
    let players =
        [
            for name in solved -> Player.Make(name, Strategy.Custom name, targeting = Targeting.ChoosesExternally name)
        ]
        @ [
            for name, strategy, targeting in [ bob; carol; dave ] ->
                Player.Make(name, strategy, firmScore = score, targeting = targeting)
        ]

    let seated = {
        Event = TableSeated
        Players = players
        Deck = Deck.Full
        Discards = Deck.Empty
    }

    Timeline.ContinueWith
        (Random seed)
        decider
        [ seated ]
        (players |> List.map (fun player -> player.Name))
        1u
        Map.empty
        players
        []
        (Deck.Full, Deck.Empty)

// A search replays its first line alone, and then the lines departing from it
// in batches that grow from one replay to as many as there are cores. Seated
// at nothing against the field's first three at 80, Sage's first search in
// seed 18 wins at its third replay, the first of the second batch, which runs
// two replays at once wherever there are two cores or more, so that the
// replay after the win must not count
[<Fact>]
let ``a search plays the first line it finds that wins, whatever its budget beyond it`` () =
    let first (budget: int) : Plan =
        let sage =
            Sage(Strategy.Decider.Default, "Sage", study = study.Value, budget = budget)

        behind 80u [ "Sage" ] 18 sage.Decider
        |> AsyncSeq.iter ignore
        |> Async.RunSynchronously

        List.head sage.Plans

    let plan = first 2000
    Assert.True plan.Won
    let atThree = first 3
    Assert.True(
        atThree.Won && not (first 2).Won,
        "the search no longer first wins at its third replay; pick a new seed"
    )
    Assert.Equal(atThree, plan)

// Seated with a supporter at nothing against the field's first three at 100,
// Sage finds no win in seed 1 departing from its own answers alone within
// half of the budget, and wins departing from the supporter's too
[<Fact>]
let ``a search departs at the supporters' answers only once the protagonist's half of the budget is spent`` () =
    let budget = 200

    let sage =
        Sage(Strategy.Decider.Default, "Sage", supporters = [ "Ally" ], study = study.Value, budget = budget)

    behind 100u [ "Sage"; "Ally" ] 1 sage.Decider
    |> AsyncSeq.iter ignore
    |> Async.RunSynchronously

    let plan = List.head sage.Plans
    Assert.True plan.Won
    Assert.InRange(plan.Replays, budget / 2 + 1, budget - 1)
    Assert.Contains("Ally", plan.Departures |> List.map (fun departure -> plan.Answers[departure].Seat))

// Seated at nothing against the field's first three already at 150, so that
// few lines win, Sage's first search in seed 6 departs from line after line
// before it gives up. Progress is told of each departure with a line starting
// "replay", and cancelled as it departs from the first, it departs from no
// more
[<Fact>]
let ``cancelling the game cancels the search under way`` () =
    use cancellation = new CancellationTokenSource()
    let departed = ref 0

    let sage =
        Sage(
            Strategy.Decider.Default,
            "Sage",
            budget = Int32.MaxValue,
            progress =
                fun message ->
                    if message.StartsWith("replay", StringComparison.Ordinal) then
                        departed.Value <- departed.Value + 1
                        cancellation.Cancel()
        )

    let game = behind 150u [ "Sage" ] 6 sage.Decider |> AsyncSeq.iter ignore

    Assert.ThrowsAny<OperationCanceledException>(fun () ->
        Async.RunSynchronously(game, cancellationToken = cancellation.Token)
    )
    |> ignore

    Assert.Equal(1, departed.Value)
    Assert.Empty sage.Plans

// ---- refusals ----

[<Fact>]
let ``Sage refuses to support itself, to search no games, or to play a seat the table lacks`` () =
    let itself =
        Assert.Throws<ArgumentException>(fun () ->
            Sage(Strategy.Decider.Default, "Sage", supporters = [ "Sage" ]) |> ignore
        )

    Assert.Equal("supporters", itself.ParamName)
    Assert.Contains("Sage", itself.Message)

    let nothing =
        Assert.Throws<ArgumentException>(fun () -> Sage(Strategy.Decider.Default, "Sage", budget = 0) |> ignore)

    Assert.Equal("budget", nothing.ParamName)

    // Without the check nobody at the table would be the supporter, so Sage
    // would silently never answer for them
    let seated = {
        Event = TableSeated
        Players = [
            for name, strategy, targeting in lineup -> Player.Make(name, strategy, targeting = targeting)
        ]
        Deck = Deck.Full
        Discards = Deck.Empty
    }

    let sage = Sage(Strategy.Decider.Default, "Sage", supporters = [ "Ally" ])
    let unseated =
        Assert.Throws<InvalidOperationException>(fun () -> sage.Follow [ seated ])
    Assert.Contains("Ally", unseated.Message)
