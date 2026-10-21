module SageTests

open System
open System.Threading
open FSharp.Control
open Xunit

open Flip7
open Support

// Sage's seat, whose questions the engine leaves to the table's decider
let private sageSeat =
    "Sage", Strategy.Custom "Sage", Targeting.ChoosesExternally "Sage"

let private lineup: Lineup = sageSeat :: field

// Searches in the tests find a win within a few dozen replays, and the budget
// only bounds what a search that cannot costs
let private budget = 300

// Sage playing alone for its seat, knowing the field from the study
let private studiedSage () : Sage =
    Sage("Sage", study = fieldStudy.Value, budget = budget)

// The table's decider with Sage seated on it: each of Sage's players, whom
// the table seats as Custom and ChoosesExternally under their own name, is
// answered by what Sage's Strategies hold for them, and everyone else by the
// decider
let private seated
    (strategies: Map<string, (Random -> Strategy.HitOrStandDecider) * (Random -> Strategy.TargetDecider)>)
    (decider: Strategy.Decider)
    : Strategy.Decider =
    strategies
    |> Map.fold
        (fun decider name (hitOrStand, target) ->
            decider
            |> Strategy.addHitOrStandStrategy name hitOrStand
            |> Strategy.addTargetingStrategy name target
        )
        decider

// Shows a Sage the game so far the only way it follows a game, through a
// question one of its seats is asked with it: the named seat is asked to aim a
// freeze with no choice but itself, which Sage answers without searching
let private show (sage: Sage) (seat: string) (history: Instant seq) : unit =
    let _, target = sage.Strategies[seat]

    let chooser =
        Player.Make(seat, Strategy.Custom seat, targeting = Targeting.ChoosesExternally seat)

    target (Random 0) history Strategy.Ask.WhoToFreeze chooser [ chooser ] [] (Deck.Full, Deck.Empty)
    |> Async.RunSynchronously
    |> ignore

// ---- seating Sage ----

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

// Sage's strategies are seated on the table's decider, each noting whose
// question it was handed, with which Random and game so far, and what Sage
// made of everyone by its answer beside what a Sage shown that game so far
// makes of them. The two agree exactly where Sage took in the game so far
// the question came with
[<Fact>]
let ``the table's decider answers everyone else, and Sage only its seat's questions, each with the Random and the game so far it came with``
    ()
    =
    let passed = ResizeArray<Random * Instant seq>()
    let decided = ResizeArray<string>()
    let opponents = Strategy.Decider.Default |> recording passed decided (fun _ -> true)

    let sage = studiedSage ()
    let read (sage: Sage) = [ for name, _, _ in lineup -> sage.ModelOf name ]
    let handed = ResizeArray<string * Random * Instant seq>()

    // Whether each question was an aim, what a Sage shown its game so far
    // makes of everyone, and what Sage made of them by its answer
    let models = ResizeArray<bool * PlayerModel option list * PlayerModel option list>()

    let noting (aim: bool) (name: string) (random: Random) (history: Instant seq) =
        handed.Add(name, random, history)
        let shown = Sage("Sage", study = fieldStudy.Value)
        show shown "Sage" history
        models.Add(aim, read shown, read sage)

    let noted =
        sage.Strategies
        |> Map.map (fun _ (hitOrStand, target) ->
            (fun random history round turn (player: Player) others finished decks -> async {
                let! choice = hitOrStand random history round turn player others finished decks
                noting false player.Name random history
                return choice
            }),
            (fun random history ask (chooser: Player) candidates finished decks -> async {
                let! aimed = target random history ask chooser candidates finished decks
                noting true chooser.Name random history
                return aimed
            })
        )

    let given = ResizeArray<Random * Instant seq>()
    let answered = ResizeArray<string>()
    let asked = ResizeArray<Random * Instant seq>()

    let table =
        seated noted opponents
        |> recording given answered (fun name -> name <> "Sage")
        |> recording asked (ResizeArray()) (fun name -> name = "Sage")

    Timeline.SimulateWithDecider (Random 7) table lineup |> played |> ignore

    // Sage searched, and its replays asked the table's decider nothing: it
    // was asked exactly the questions the other seats were
    Assert.NotEmpty sage.Plans
    Assert.NotEmpty given
    Assert.Equal(given.Count, passed.Count)

    // Each with the very Random and game so far the engine gave the table,
    // and its answers came back unchanged
    for (random, history), (random', history') in Seq.zip given passed do
        Assert.Same(random, random')
        Assert.Same(history, history')

    Assert.Equal<string seq>(decided, answered)

    // Sage was handed its seat's questions alone, every one of them, each
    // with the very Random and game so far the engine gave the table
    Assert.NotEmpty asked
    Assert.Equal(asked.Count, handed.Count)

    for (random, history), (name, random', history') in Seq.zip asked handed do
        Assert.Equal("Sage", name)
        Assert.Same(random, random')
        Assert.Same(history, history')

    // and took in the game so far each came with, so that by its answer it
    // made of everyone what a Sage shown that game so far makes of them. Some
    // aim must show Sage something its last question had not, or a Sage that
    // ignored the game so far its aims came with would pass
    Assert.True(
        models
        |> Seq.pairwise
        |> Seq.exists (fun ((_, before, _), (aim, after, _)) -> aim && after <> before),
        "no aim of Sage's showed it anything new; pick a new seed"
    )

    for _, shown, followed in models do
        Assert.Equal<PlayerModel option list>(shown, followed)

// Were Bob to declare Ally's labels, the table would hand his questions to
// what Sage registers for Ally, which must not answer them as Ally's
[<Fact>]
let ``what Sage registers for a seat refuses anyone else's question`` () =
    let sage = Sage("Sage", supporters = [ "Ally" ])
    let hitOrStand, target = sage.Strategies["Ally"]
    let bob =
        Player.Make("Bob", Strategy.Custom "Ally", targeting = Targeting.ChoosesExternally "Ally")

    let players = [
        Player.Make("Sage", Strategy.Custom "Sage", targeting = Targeting.ChoosesExternally "Sage")
        Player.Make("Ally", Strategy.Custom "Ally", targeting = Targeting.ChoosesExternally "Ally")
        bob
    ]

    let history = [
        {
            Event = TableSeated
            Players = players
            Deck = Deck.Full
            Discards = Deck.Empty
        }
    ]

    let others = players |> List.where (fun player -> player.Name <> "Bob")

    let asked =
        Assert.Throws<InvalidOperationException>(fun () ->
            hitOrStand (Random 1) history 1u 1u bob others [] (Deck.Full, Deck.Empty)
            |> Async.RunSynchronously
            |> ignore
        )

    let aimed =
        Assert.Throws<InvalidOperationException>(fun () ->
            target (Random 1) history Strategy.Ask.WhoToFreeze bob players [] (Deck.Full, Deck.Empty)
            |> Async.RunSynchronously
            |> ignore
        )

    for refusal in [ asked; aimed ] do
        Assert.Contains("Ally", refusal.Message)
        Assert.Contains("Bob", refusal.Message)

    // Sage registers one entry for each of its seats, by name
    Assert.Equal<string list>([ "Ally"; "Sage" ], sage.Strategies |> Map.keys |> List.ofSeq)

// ---- before the seed is pinned ----

// What a solved seat answers unaided, on dice of its own seeded as given: it
// maximizes expected value and aims spitefully, as the engine plays those
let private unaidedHitOrStand (seed: int) : Strategy.HitOrStandDecider =
    Strategy.Decider.Default.HitOrStand (Random seed) Strategy.MaximizesExpectedValue

let private unaidedTarget (seed: int) : Strategy.TargetDecider =
    Strategy.Decider.Default.Target (Random seed) Targeting.PlaysSpitefully

// The protagonist and a supporter, seated as Sage requires
let private solvedLineup: Lineup = [
    sageSeat
    "Ally", Strategy.Custom "Ally", Targeting.ChoosesExternally "Ally"
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

// Plays a game with Sage seated on the default decider, answering for the
// protagonist and the supporter, each solved question asked with a Random of
// the test's, seeded by a draw from the one the engine gave, so that the test
// can roll the same dice. Returns the timeline and every solved question
let private playSolved (random: Random) (sage: Sage) : Instant list * Compared list =
    let asked = ResizeArray<Compared>()
    let solved = set [ "Sage"; "Ally" ]
    let withSage = seated sage.Strategies Strategy.Decider.Default

    let table: Strategy.Decider = {
        HitOrStand =
            fun random strategy history round turn player others finished decks -> async {
                if solved.Contains player.Name then
                    let seed = random.Next()
                    let before = sage.Plans.Length

                    let! answer =
                        withSage.HitOrStand (Random seed) strategy history round turn player others finished decks

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
                    return! withSage.HitOrStand random strategy history round turn player others finished decks
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                if solved.Contains chooser.Name then
                    let seed = random.Next()
                    let before = sage.Plans.Length

                    let! answer =
                        withSage.Target (Random seed) targeting history ask chooser candidates finished decks

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
                    return! withSage.Target random targeting history ask chooser candidates finished decks
            }
    }

    let timeline = Timeline.SimulateWithDecider random table solvedLineup |> played
    timeline, List.ofSeq asked

[<Fact>]
let ``until the seed is pinned the solved seats answer unaided, and Sage searches at the first question after`` () =
    let sage =
        Sage("Sage", supporters = [ "Ally" ], study = fieldStudy.Value, budget = budget)

    let timeline, asked = playSolved (Random 2026) sage

    // How many instants the oracle had seen when it pinned the seed: the
    // scan's first state is the oracle before any instant
    let pinned =
        timeline
        |> List.scan Oracle.Observe Oracle.Start
        |> List.findIndex (fun oracle -> (Oracle.SeedsOf oracle).IsPinned)

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

    let sage = Sage("Sage", supporters = [ "Ally" ], study = fieldStudy.Value)

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

// Before a game Sage knows everyone as it studied them. Sage follows a game
// only through its seat's questions, so by the end of one it has added to
// that at most all of the decisions each opponent made in it, and of their
// aims at one of two or more players, and at least one of each where the game
// showed any, since its seat was asked something after each opponent's first.
// Its seat asked something with the finished game, it has added every one of
// them
[<Fact>]
let ``Sage adds every decision and aim a game shows to what it studied`` () =
    let sage = studiedSage ()

    for name, _, _ in lineup do
        Assert.Equal(Study.ModelOf fieldStudy.Value name, sage.ModelOf name)

    let timeline =
        Timeline.SimulateWithDecider (Random 5) (seated sage.Strategies Strategy.Decider.Default) lineup
        |> played
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
        let studied = (Study.ModelOf fieldStudy.Value name).Value
        let model = (sage.ModelOf name).Value
        Assert.InRange(model.Observations, studied.Observations + min 1 decided, studied.Observations + decided)
        Assert.InRange(model.Aims, studied.Aims + min 1 aimed, studied.Aims + aimed)

    // Its seat asked something with the finished game, Sage has added every
    // one of them
    show sage "Sage" timeline

    for name, _, _ in field do
        let decided, aimed = shown name
        let studied = (Study.ModelOf fieldStudy.Value name).Value
        let model = (sage.ModelOf name).Value
        Assert.Equal(studied.Observations + decided, model.Observations)
        Assert.Equal(studied.Aims + aimed, model.Aims)

// The lineup with the named player seated as Sage requires of its seats, as
// Custom and ChoosesExternally under their own name, and a decider that plays
// them as the lineup declared them, with the Random each question comes with,
// as the engine would. So the game is the one the lineup deals, and a Sage for
// that player can be shown it
let private declaredFor (name: string) (lineup: Lineup) : Lineup * Strategy.Decider =
    let _, strategy, targeting = lineup |> List.find (fun (seat, _, _) -> seat = name)

    let decider =
        Strategy.Decider.Default
        |> Strategy.addHitOrStandStrategy name (fun random -> Strategy.Decider.Default.HitOrStand random strategy)
        |> Strategy.addTargetingStrategy name (fun random -> Strategy.Decider.Default.Target random targeting)

    let seats =
        lineup
        |> List.map (fun (seat, strategy, targeting) ->
            if seat = name then
                seat, Strategy.Custom name, Targeting.ChoosesExternally name
            else
                seat, strategy, targeting
        )

    seats, decider

// At a table of two, the first 26 instants of seed 13 end with Bob freezing
// himself, the only one left in the round, and before it he never aims at
// all, so that a game quit there shows him aim only where he had no choice.
// Neither a study of it nor a Sage following it counts that aim, while the
// whole game counts every aim he made with a choice of players
[<Fact>]
let ``an aim with no choice of players shows nothing of how a player aims`` () =
    let lineup, decider =
        declaredFor "Ann" [
            "Ann", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
            "Bob", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
        ]

    let whole = Timeline.SimulateWithDecider (Random 13) decider lineup |> played
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

    let evidence = evidenceOf "Bob" forced
    Assert.Equal(0, evidence.Aims)
    Assert.Empty evidence.Names
    Assert.Empty(modelsOf forced)

    // How many of Bob's aims a model counts, none for no model at all
    let aimsIn (model: PlayerModel option) : int =
        model |> Option.map (fun model -> model.Aims) |> Option.defaultValue 0

    // What a Sage for Ann shown a game makes of Bob
    let following (timeline: Instant list) : PlayerModel option =
        let sage = Sage("Ann")
        show sage "Ann" timeline
        sage.ModelOf "Bob"

    let quitStudy = Study.OfTimelines [ AsyncSeq.ofSeq quit ] |> Async.RunSynchronously
    Assert.Equal(0, aimsIn (Study.ModelOf quitStudy "Bob"))
    Assert.Equal(0, aimsIn (following quit))

    let chosen =
        aimsOf whole
        |> List.where (fun aim -> aim.OtherPlayers.Length > 1)
        |> List.length

    let wholeStudy =
        Study.OfTimelines [ AsyncSeq.ofSeq whole ] |> Async.RunSynchronously
    Assert.True(chosen > 0, "Bob never aimed at one of several players in the whole game; pick a new seed")
    Assert.Equal(chosen, aimsIn (Study.ModelOf wholeStudy "Bob"))
    Assert.Equal(chosen, aimsIn (following whole))

// At a table of two, the first three instants of seed 171 show Bob only
// dealing three to himself once Ann froze herself, and those of seed 1 show
// him only dealing three to Ann while both were in the round. Neither a study
// of a game nor a Sage following it models a player it never saw decide or aim
// at one of two or more players, and both model one seen only aiming as
// fitting does. That model's hit rate is NaN, which equality on an option of
// it compares as unequal to itself, so the models are compared bare
[<Fact>]
let ``a player seen only aiming is modelled once seen aiming at one of two or more players`` () =
    let lineup, decider =
        declaredFor "Ann" [
            "Ann", Strategy.HitUntilScore 25u, Targeting.ChoosesRandomly
            "Bob", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
        ]

    // Bob's observations in the first three instants of a seed's game, and
    // what a study of them and a Sage for Ann shown them make of him
    let opening (seed: int) =
        let instants =
            Timeline.SimulateWithDecider (Random seed) decider lineup
            |> played
            |> List.take 3

        let study = Study.OfTimelines [ AsyncSeq.ofSeq instants ] |> Async.RunSynchronously
        let sage = Sage("Ann")
        show sage "Ann" instants

        let bobs =
            observationsOf instants
            |> List.where (fun observation -> observation.Name = "Bob")

        bobs, Study.ModelOf study "Bob", sage.ModelOf "Bob"

    // A change to how the game draws can move either seed off its road
    let aimsAmong (candidates: int -> bool) (observations: Observation list) =
        not observations.IsEmpty
        && observations
           |> List.forall (fun aim -> aim.Choice.IsChoice2Of2 && candidates aim.OtherPlayers.Length)

    let forced, studied, followed = opening 171

    Assert.True(
        aimsAmong (fun count -> count = 1) forced,
        "Bob does more than aim with no choice of players in seed 171's opening; pick a new seed"
    )

    Assert.Equal(None, studied)
    Assert.Equal(None, followed)

    let chosen, studied, followed = opening 1

    Assert.True(
        aimsAmong (fun count -> count > 1) chosen,
        "Bob does more than aim at one of several players in seed 1's opening; pick a new seed"
    )

    let fitted = modelsOf chosen |> List.exactlyOne
    Assert.Equal(0, fitted.Observations)
    Assert.Equal(chosen.Length, fitted.Aims)
    Assert.Equal(fitted, studied.Value)
    Assert.Equal(fitted, followed.Value)

// A Sage given a study over grids of one's own scores the game it plays over
// the same grids, even once the study's sitting is concluded, so shown the
// whole game it reads everyone exactly as Inference scores the game over them,
// onto what the study holds of them carried into it. Over the default grids,
// the game's evidence would not line up with the study's
[<Fact>]
let ``a Sage scores the game it plays over its study's grids`` () =
    let strategies =
        Inference.DefaultStrategyCandidates @ [ Strategy.HitUntilBustProbability 0.35 ]

    let policies =
        Inference.DefaultTargetingCandidates @ [ Targeting.PlaysGreedily(0.0, 0.2) ]

    let own =
        [ for seed in 1..10 -> Timeline.SimulateWith (Random(100 + seed)) (standIn :: field) ]
        |> Study.OfTimelinesWith strategies policies
        |> Async.RunSynchronously
        |> Study.Conclude

    let sage = Sage("Sage", study = own, budget = budget)

    let game =
        Timeline.SimulateWithDecider (Random 5) (seated sage.Strategies Strategy.Decider.Default) lineup
        |> played

    show sage "Sage" game

    let names = lineup |> List.map (fun (name, _, _) -> name)
    let observations = observationsOf game
    let theirs (name: string) = observations |> List.where (fun observation -> observation.Name = name)

    Assert.True(
        field
        |> List.exists (fun (name, _, _) ->
            theirs name
            |> List.exists (fun observation -> observation.Choice.IsChoice2Of2 && observation.OtherPlayers.Length > 1)
        ),
        "nobody in the field aims at one of several players in seed 5's game; pick a new seed"
    )

    for name in names do
        let scored =
            Inference.Combine
                (Inference.Carry Study.Stickiness names (Study.EvidenceOf own name).Value)
                (Inference.Evidence 0.1 strategies policies names name (theirs name))
            |> Inference.ModelFrom

        Assert.Equal(scored, (sage.ModelOf name).Value)

// A Sage plays its game as the next of its study's sitting: as the game
// starts, it carries what it studied of everyone into it, so Bob most likely
// takes the sides he took throughout the study, though no longer as surely as
// at its end, since he keeps them from one game to the next only with
// probability 0.8
[<Fact>]
let ``Sage carries the sides everyone took in its study into its game`` () =
    let sage = Sage("Sage", study = partisanStudy.Value, budget = budget)
    Assert.Equal(Study.ModelOf partisanStudy.Value "Bob", sage.ModelOf "Bob")

    // The first instant of a game of the field, with Sage seated as Sage
    // requires of its seats
    let seated =
        let first =
            Timeline.SimulateWithDecider (Random 1) partisans (standIn :: partisanField)
            |> AsyncSeq.take 1
            |> played
            |> List.exactlyOne

        {
            first with
                Players =
                    first.Players
                    |> List.map (fun player ->
                        if player.Name = "Sage" then
                            {
                                player with
                                    Strategy = Strategy.Custom "Sage"
                                    Targeting = Targeting.ChoosesExternally "Sage"
                            }
                        else
                            player
                    )
        }

    show sage "Sage" [ seated ]

    let table = seated.Players |> List.map (fun player -> player.Name)

    let carried =
        Inference.ModelFrom(Inference.Carry Study.Stickiness table (Study.EvidenceOf partisanStudy.Value "Bob").Value)

    Assert.Equal(carried, (sage.ModelOf "Bob").Value)

    let likeliest, probability = List.head carried.TargetingPosterior
    Assert.Equal(partisan, likeliest)
    // Carried, a way of taking sides keeps its weight with probability 0.8 and
    // is otherwise chosen afresh at its prior, half its policy's weight shared
    // among the n^2 + n ways over the table's names
    let ways = float (table.Length * table.Length + table.Length)
    Assert.InRange(probability, 0.5, Study.Stickiness + (1.0 - Study.Stickiness) / (2.0 * ways))
    Assert.True(probability < aimsAs partisan partisanStudy.Value "Bob")

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

// Seats Sage on the opponents' decider and passes every question to it,
// noting each one a solved seat is asked
let private asking
    (solved: Set<string>)
    (sage: Sage)
    (asked: ResizeArray<Asked>)
    (opponents: Strategy.Decider)
    : Strategy.Decider =
    let withSage = seated sage.Strategies opponents
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
                    withSage.HitOrStand random strategy history round turn player others finished decks

                if solved.Contains player.Name then
                    note history before [] (Answer.HitOrStand(player.Name, round, turn, player.Hand, choice))

                return choice
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                let before = sage.Plans.Length

                let! target =
                    withSage.Target random targeting history ask chooser candidates finished decks

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
// every way the solved seats' answers broke those rules, and every plan whose
// count of answers played differs from how many the rules played of it, and
// Strays counts the hit-or-stand questions the plan still expected next once
// the game had gone otherwise than it foresaw, where only the events show that
// the plan is no longer the game's. The rules mirror how Sage follows a game
// and answers (follow and the strategies it registers, in src/Sage.fs), so a
// change to those must be made here too
type private Audit = { Problems: string list; Strays: int }

let private audit (plans: Plan list) (asked: Asked seq) : Audit =
    let plans = Array.ofList plans
    let problems = ResizeArray<string>()
    let mutable strays = 0
    let mutable line: Answer list = []
    let mutable foreseen: Event array = [||]
    let mutable forkedAt = 0
    let played = Array.zeroCreate<int> plans.Length
    let mutable playing = -1

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
            played[playing] <- played[playing] + 1
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
                playing <- question.After - 1
                played[playing] <- 1
            | 1, _ ->
                problems.Add $"{question.Answer}, though the plan made there starts otherwise"
                line <- []
            | _ ->
                problems.Add $"{question.Answer}, after making {made} plans where the plan was left"
                line <- []

    for index, plan in Array.indexed plans do
        if plan.Played <> played[index] then
            problems.Add $"plan {index + 1} counts {plan.Played} answers played, where the game played {played[index]}"

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
                Timeline.SimulateWithDecider
                    (Random seed)
                    (asking (set [ "Sage" ]) sage asked Strategy.Decider.Default)
                    lineup
                |> played

            let game = $"seed {seed}"
            let plan = Assert.Single sage.Plans
            follows game plan asked
            playedByTheRules game sage.Plans asked
            Assert.Equal(plan.Margin, marginOf "Sage" timeline)
            plan.Won
        )

    Assert.True(won.Length >= 8, $"won {won.Length} of 10")

// Every seed below is searched once and the live game follows the plan to the
// end, though in each of them Bob or Carol aims somewhere spite would not, so
// that a replay of them as spiteful would have strayed from the live game
[<Fact>]
let ``Sage replays players who take sides as it fitted them, and its plan foresees them`` () =
    for name in [ "Bob"; "Carol" ] do
        let likeliest, probability =
            List.head (Study.ModelOf partisanStudy.Value name).Value.TargetingPosterior

        Assert.Equal(partisan, likeliest)
        Assert.True(probability > 0.9, $"{name} aims {likeliest} at {probability}")

    for seed in 1..5 do
        let sage = Sage("Sage", study = partisanStudy.Value, budget = budget)
        let asked = ResizeArray<Asked>()

        let timeline =
            Timeline.SimulateWithDecider
                (Random seed)
                (asking (set [ "Sage" ]) sage asked partisans)
                (sageSeat :: partisanField)
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
                && probabilityOfTarget Targeting.PlaysSpitefully aim = 0.0
            )

        Assert.True(not unlikeSpite.IsEmpty, $"{game}: Bob and Carol aimed as spite would; pick a new seed")

[<Fact>]
let ``a replay aims as declared, or as Sage made of one who aims as they please, never at random if they take sides``
    ()
    =
    // Evidence of ten aims over one name whose aim posterior is as given, and
    // of no decisions. The prior subtracts log (n * n + n) from the weight of
    // each way of taking sides among n names, which over one name is log 2, so
    // adding log 2 here cancels it
    let model (posterior: (Targeting * float) list) : PlayerEvidence option =
        Some {
            Name = "Bob"
            Observations = 0
            Hits = 0
            StrategyLogLikelihoods =
                Inference.DefaultStrategyCandidates
                |> List.map (fun candidate -> candidate, 0.0)
            Aims = 10
            Names = [ "Carol" ]
            TargetingLogLikelihoods =
                posterior
                |> List.map (fun (targeting, probability) ->
                    match targeting with
                    | Targeting.TakesSides _ -> targeting, log probability + log 2.0
                    | _ -> targeting, log probability
                )
        }

    let spite = Targeting.PlaysSpitefully
    let external = Targeting.ChoosesExternally "Bob"
    let greedy = Targeting.PlaysGreedily(0.4, 0.2)
    let random = Targeting.ChoosesRandomly
    let sidesAtRandom =
        Targeting.TakesSides(Some "Carol", None, Targeting.ChoosesRandomly)
    let sides = Targeting.TakesSides(Some "Carol", None, spite)

    // Never seen aiming: as declared, or spitefully for ChoosesExternally,
    // even with evidence of how they hit or stand, which holds no aims
    let decidedOnly =
        model [ Targeting.ChoosesRandomly, 1.0 ]
        |> Option.map (fun evidence -> { evidence with Observations = 10; Aims = 0 })

    Assert.Equal(greedy, Replayed.Aiming None greedy)
    Assert.Equal(spite, Replayed.Aiming None external)
    Assert.Equal(spite, Replayed.Aiming decidedOnly external)

    // A declared policy whatever the aims seen of it suggest, even one off the
    // grid or one that takes sides at random, since the engine plays it on the
    // replay's own dice: a few aims at random can look spiteful
    let spiteful = model [ spite, 0.9; random, 0.1 ]
    let offGrid = Targeting.PlaysGreedily(0.25, 0.15)
    Assert.Equal(random, Replayed.Aiming spiteful Targeting.ChoosesRandomly)
    Assert.Equal(offGrid, Replayed.Aiming spiteful offGrid)
    Assert.Equal(sidesAtRandom, Replayed.Aiming spiteful sidesAtRandom)
    Assert.Equal(spite, Replayed.Aiming spiteful external)

    // One who aims as they please aims as the posterior's head, which of
    // equals is the first
    let tied = model [ Targeting.PlaysGreedily(0.3, 0.2), 0.4; greedy, 0.4; sides, 0.2 ]

    Assert.Equal(Targeting.PlaysGreedily(0.3, 0.2), Replayed.Aiming tied external)
    Assert.Equal(snd (Inference.MostLikely(Inference.ModelFrom tied.Value)), Replayed.Aiming tied external)

    // Taking sides at random is replayed as the likeliest targeting that
    // decides, but a plain random chooser stays random, since they may roll the
    // dice the engine hands them
    let atRandom = model [ sidesAtRandom, 0.6; random, 0.3; sides, 0.1 ]
    Assert.Equal(sides, Replayed.Aiming atRandom external)
    Assert.Equal(random, Replayed.Aiming (model [ random, 0.7; sides, 0.3 ]) external)

    // Where no targeting decides, taking sides at random stays the likeliest,
    // as there is nothing to replace it with
    Assert.Equal(sidesAtRandom, Replayed.Aiming (model [ sidesAtRandom, 0.7; random, 0.3 ]) external)

    // One who takes sides and otherwise chooses externally, however deep, is
    // answered by the table's decider, sides and all, which the engine cannot
    // play, so they are fitted just as one who declared ChoosesExternally
    let sidesExternal = Targeting.TakesSides(Some "Carol", None, external)
    let deeper = Targeting.TakesSides(None, Some "Dan", sidesExternal)
    Assert.Equal(spite, Replayed.Aiming None sidesExternal)
    Assert.Equal(spite, Replayed.Aiming spiteful deeper)
    Assert.Equal(Targeting.PlaysGreedily(0.3, 0.2), Replayed.Aiming tied sidesExternal)

// Hal takes sides for Jon and otherwise aims under a label the table's decider
// answers, here at random among the rest. The engine cannot play that label,
// so Sage's replays fit Hal rather than hand the label to the engine, and the
// games Sage plays at the table play to the end. In both seeds a replay
// reaches Hal's label, which the engine refused when the replays kept his
// declaration
[<Fact>]
let ``Sage plays at a table whose decider answers one who takes sides and otherwise chooses externally`` () =
    let leaning: Strategy.Decider = {
        Strategy.Decider.Default with
            Target =
                fun random targeting ->
                    match targeting with
                    | Targeting.TakesSides(favours, opposes, Targeting.ChoosesExternally "Lean") ->
                        Strategy.Decider.Default.Target
                            random
                            (Targeting.TakesSides(favours, opposes, Targeting.ChoosesRandomly))
                    | targeting -> Strategy.Decider.Default.Target random targeting
    }

    let lineup: Lineup = [
        sageSeat
        "Hal",
        Strategy.MaximizesExpectedValue,
        Targeting.TakesSides(Some "Jon", None, Targeting.ChoosesExternally "Lean")
        "Jon", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
        "Kim", Strategy.HitUntilNumCards 4u, Targeting.PlaysSpitefully
    ]

    for seed in 1..2 do
        let sage = Sage("Sage", budget = 50)

        let timeline =
            Timeline.SimulateWithDecider (Random seed) (seated sage.Strategies leaning) lineup
            |> played

        match List.last timeline with
        | { Event = RoundEnded _; Players = players } ->
            Assert.True(players |> List.exists (fun player -> player.FirmScore >= 200u), $"seed {seed}")
        | instant -> Assert.Fail $"seed {seed}: the game ended at {instant.Event}"

// Sage seated first at twelve, before eleven people who play under a Custom
// label and aim on the given policy, which the engine plays for them
let private twelveAt (targeting: Targeting) : Lineup =
    sageSeat
    :: [
        for name in "Bob Carol Dave Erin Fay Gus Hal Ida Jo Kit Lou".Split ' ' ->
            name, Strategy.Custom "Person", targeting
    ]

// The table's decider for them, which plays their label as maximizing
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
// expected value, which is how the table's decider plays their label,
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

    let sage = Sage("Sage", budget = 1)
    let asked = ResizeArray<Asked>()
    let first = ref []

    let noting =
        asking (set [ "Sage" ]) sage asked maximizers
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

    let fitted = seen |> List.filter (fun aim -> aim.Name <> "Sage") |> modelsOf

    Assert.True(
        fitted
        |> List.exists (fun model -> snd (Inference.MostLikely model) <> targeting),
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
// both left to the table's decider, and Sage knows them only by name.
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
// finished the round would hand the engine a Custom strategy it cannot play.
// The people's decider is the table's, and is asked exactly the questions the
// other seats are at the table
[<Fact>]
let ``Sage replays people by what it made of them, never by the table's decider`` () =
    let lineup = [
        "Erin", Strategy.Custom "Person", Targeting.ChoosesExternally "Person"
        sageSeat
        "Bob", Strategy.Custom "Person", Targeting.ChoosesExternally "Person"
        carol
        dave
    ]

    let passed = ref 0
    let toPeople = people |> counting Set.empty passed
    let sage = Sage("Sage", study = fieldStudy.Value, budget = budget)

    // Who had finished the round at each question Sage was asked to hit or
    // stand before it made a plan, the last of which it made the plan at
    let finished = ref []
    let asked = ResizeArray<Asked>()
    let live = ref 0

    let atTable =
        asking (set [ "Sage" ]) sage asked toPeople
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
        Sage("Sage", supporters = [ "Ally" ], study = fieldStudy.Value, budget = budget)

    Timeline.SimulateWithDecider (Random 1) (asking (set [ "Sage"; "Ally" ]) sage asked (strays 1)) lineup
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
// its seat's first question in the second game shows it, so only the identity
// of the second game's TableSeated, not how much of a game Sage has seen,
// tells the two games apart
[<Fact>]
let ``a new game starts afresh, keeping only what Sage studied`` () =
    let fresh = studiedSage ()
    let used = studiedSage ()

    let opening =
        Timeline.SimulateWithDecider (Random 3) (seated used.Strategies Strategy.Decider.Default) lineup
        |> played

    Assert.NotEmpty used.Plans
    show used "Sage" (List.take 2 opening)

    let firstShown = ref None

    let noting =
        seated used.Strategies Strategy.Decider.Default
        |> beforeEachQuestion (fun _ history question ->
            if firstShown.Value.IsNone && question.Asked.Name = "Sage" then
                firstShown.Value <- Some(Seq.length history)
        )

    let again = Timeline.SimulateWithDecider (Random 4) noting lineup |> played
    Assert.True(
        firstShown.Value.Value >= 2,
        $"Sage's first question in the second game showed {firstShown.Value.Value} instants, which the length check tells apart"
    )

    let first =
        Timeline.SimulateWithDecider (Random 4) (seated fresh.Strategies Strategy.Decider.Default) lineup
        |> played
    Assert.Equal<Instant list>(first, again)
    Assert.Equal<Plan list>(fresh.Plans, used.Plans)

    for name, _, _ in lineup do
        Assert.Equal(fresh.ModelOf name, used.ModelOf name)

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
    let table = asking solved sage asked Strategy.Decider.Default

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
        Sage("Sage", supporters = [ "Ally" ], study = fieldStudy.Value, budget = budget)

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

    // A study of the edited game scores Erin's decisions with those turns, and
    // her aims over everyone at the table
    let wholeStudy =
        Study.OfTimelines [ AsyncSeq.ofSeq whole ] |> Async.RunSynchronously
    let erinsObservations =
        observationsOf whole
        |> List.filter (fun observation -> observation.Name = "Erin")

    Assert.Equal(
        Some(
            Inference.ModelFrom(
                Inference.Evidence
                    0.1
                    Inference.DefaultStrategyCandidates
                    Inference.DefaultTargetingCandidates
                    (lineup |> List.map (fun (name, _, _) -> name))
                    "Erin"
                    erinsObservations
            )
        ),
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

    let tableSeated = {
        Event = TableSeated
        Players = players
        Deck = Deck.Full
        Discards = Deck.Empty
    }

    Timeline.ContinueWith
        (Random seed)
        decider
        [ tableSeated ]
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
        let sage = Sage("Sage", study = fieldStudy.Value, budget = budget)

        behind 80u [ "Sage" ] 18 (seated sage.Strategies Strategy.Decider.Default)
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
        Sage("Sage", supporters = [ "Ally" ], study = fieldStudy.Value, budget = budget)

    behind 100u [ "Sage"; "Ally" ] 1 (seated sage.Strategies Strategy.Decider.Default)
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
            "Sage",
            budget = Int32.MaxValue,
            progress =
                fun message ->
                    if message.StartsWith("replay", StringComparison.Ordinal) then
                        departed.Value <- departed.Value + 1
                        cancellation.Cancel()
        )

    let game =
        behind 150u [ "Sage" ] 6 (seated sage.Strategies Strategy.Decider.Default)
        |> AsyncSeq.iter ignore

    Assert.ThrowsAny<OperationCanceledException>(fun () ->
        Async.RunSynchronously(game, cancellationToken = cancellation.Token)
    )
    |> ignore

    Assert.Equal(1, departed.Value)
    Assert.Empty sage.Plans

// ---- refusals ----

// The TableSeated instant a game of the lineup starts with
let private sittingDown (lineup: Lineup) : Instant = {
    Event = TableSeated
    Players = [
        for name, strategy, targeting in lineup -> Player.Make(name, strategy, targeting = targeting)
    ]
    Deck = Deck.Full
    Discards = Deck.Empty
}

[<Fact>]
let ``Sage refuses to support itself, to search no games, or to play a seat the table lacks`` () =
    let itself =
        Assert.Throws<ArgumentException>(fun () -> Sage("Sage", supporters = [ "Sage" ]) |> ignore)

    Assert.Equal("supporters", itself.ParamName)
    Assert.Contains("Sage", itself.Message)

    let nothing =
        Assert.Throws<ArgumentException>(fun () -> Sage("Sage", budget = 0) |> ignore)

    Assert.Equal("budget", nothing.ParamName)

    // Without the check nobody at the table would be the supporter, so Sage
    // would silently never answer for them
    let sage = Sage("Sage", supporters = [ "Ally" ])
    let unseated =
        Assert.Throws<InvalidOperationException>(fun () -> show sage "Sage" [ sittingDown lineup ])
    Assert.Contains("Ally", unseated.Message)

// A seat of Sage's declared any other way than as Custom and ChoosesExternally
// under its own name, in either part, would have the engine, or whoever the
// label names, answer questions Sage's plans count on answering. So Sage
// refuses it at the start of every game, at whichever question one of its
// seats is asked first, and says how to seat it
[<Fact>]
let ``Sage refuses a game that seats one of its players other than as Custom and ChoosesExternally under their name``
    ()
    =
    // The field with Sage seated as it requires and Ally as given
    let withAlly (strategy: Strategy) (targeting: Targeting) : Lineup =
        sageSeat :: ("Ally", strategy, targeting) :: field

    let declared =
        withAlly (Strategy.Custom "Ally") (Targeting.ChoosesExternally "Ally")

    let allySpiteful = withAlly (Strategy.Custom "Ally") Targeting.PlaysSpitefully

    let strategyAlone =
        ("Sage", Strategy.Custom "Sage", Targeting.PlaysSpitefully)
        :: List.tail declared

    // Ally on an engine strategy or targeting policy, or under Sage's label
    // in either part or both, and Sage declared Custom for its strategy
    // alone, each with the seat the refusal is about
    let refused = [
        "Ally", withAlly Strategy.MaximizesExpectedValue (Targeting.ChoosesExternally "Ally")
        "Ally", allySpiteful
        "Ally", withAlly (Strategy.Custom "Sage") (Targeting.ChoosesExternally "Ally")
        "Ally", withAlly (Strategy.Custom "Ally") (Targeting.ChoosesExternally "Sage")
        "Ally", withAlly (Strategy.Custom "Sage") (Targeting.ChoosesExternally "Sage")
        "Sage", strategyAlone
    ]

    for name, lineup in refused do
        let sage = Sage("Sage", supporters = [ "Ally" ])
        let refusal =
            Assert.Throws<InvalidOperationException>(fun () -> show sage "Sage" [ sittingDown lineup ])
        Assert.Contains($"Strategy.Custom \"{name}\"", refusal.Message)
        Assert.Contains($"Targeting.ChoosesExternally \"{name}\"", refusal.Message)

    // Seated as it requires, Sage follows the game, and asked its first
    // question in a later game that seats Ally on spite, it refuses that game
    let sage = Sage("Sage", supporters = [ "Ally" ])
    show sage "Sage" [ sittingDown declared ]

    let refusal =
        Assert.Throws<InvalidOperationException>(fun () ->
            Timeline.SimulateWithDecider (Random 1) (seated sage.Strategies Strategy.Decider.Default) allySpiteful
            |> played
            |> ignore
        )

    Assert.Contains("Targeting.ChoosesExternally \"Ally\"", refusal.Message)
