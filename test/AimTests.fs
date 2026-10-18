module AimTests

open System
open FSharp.Control
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7
open Support
open Games

// Observation reads every question a player was asked off a timeline, and
// Inference fits how a player aims from the aims among them. The first half
// holds the questions a timeline reveals to the ones the engine asked, call by
// call, the way SolverTests holds the interpreter to the game's Random, over
// the games SolverTests plays too. The second half holds the aiming model to
// the engine's own deciders.

// ---- Observation: the questions a timeline reveals ----

// A question the engine asked, with its answer, as an observation, and how
// much of the game the decider was shown. The game so far tells a
// TargetDecider the round, one more than the rounds it ended, but not the
// turn, so an aim's turn is left at zero
type private Asked = { Seen: int; Question: Observation }

// What an aim was aiming
let private askOf (aim: Observation) : Strategy.Ask =
    match aim.Choice with
    | Choice2Of2(ask, _) -> ask
    | Choice1Of2 _ -> invalidArg (nameof aim) "a hit-or-stand decision aims nothing"

// Whether a question was an aim of the given kind
let private asks (ask: Strategy.Ask) (call: Asked) : bool =
    call.Question.Choice.IsChoice2Of2 && askOf call.Question = ask

// Passes every question to the decider, logging each with its answer
let private recording (asked: ResizeArray<Asked>) (decider: Strategy.Decider) : Strategy.Decider = {
    decider with
        HitOrStand =
            fun random strategy history round turn player others finished decks -> async {
                let! choice =
                    decider.HitOrStand random strategy history round turn player others finished decks

                asked.Add {
                    Seen = Seq.length history
                    Question = {
                        Name = player.Name
                        Choice = Choice1Of2 choice
                        Round = round
                        Turn = turn
                        Player = player
                        OtherPlayers = others
                        FinishedPlayers = finished
                        Deck = fst decks
                        Discards = snd decks
                    }
                }

                return choice
            }
        Target =
            fun random targeting history ask chooser candidates finished decks -> async {
                let! target =
                    decider.Target random targeting history ask chooser candidates finished decks

                let ended =
                    history |> Seq.where (fun instant -> instant.Event.IsRoundEnded) |> Seq.length

                asked.Add {
                    Seen = Seq.length history
                    Question = {
                        Name = chooser.Name
                        Choice = Choice2Of2(ask, target.Name)
                        Round = 1u + uint ended
                        Turn = 0u
                        Player = chooser
                        OtherPlayers = candidates
                        FinishedPlayers = finished
                        Deck = fst decks
                        Discards = snd decks
                    }
                }

                return target
            }
}

// The second chances the target of a deal3 passed on during its flips are
// asked of their decider with the game as it stood before the deal3, so two of
// them in one deal3 are shown as much of the game as each other and no other
// second chance is. Those are the aims the timeline cannot order
let private ambiguous (asked: Asked list) : Asked list =
    asked
    |> List.where (asks Strategy.Ask.WhoReceivesSecondChance)
    |> List.groupBy (fun call -> call.Seen)
    |> List.collect (fun (_, calls) -> if calls.Length > 1 then calls else [])

// Where the questions the timeline reveals disagree with those the engine
// asked, if anywhere, comparing the two in the order the engine asked them:
// every hit-or-stand decision, turns and all, and every aim but its turn
let private disagreement (timeline: Instant list) (asked: Asked list) : string option =
    let skipped = ambiguous asked

    let expected =
        asked
        |> List.where (fun call -> not (List.contains call skipped))
        |> List.map (fun call -> call.Question)

    let observed =
        observationsOf timeline
        |> List.map (fun question ->
            if question.Choice.IsChoice2Of2 then
                { question with Turn = 0u }
            else
                question
        )

    match
        Seq.zip expected observed
        |> Seq.tryFindIndex (fun (expected, observed) -> expected <> observed)
    with
    | Some index -> Some $"question {index} was asked as %A{expected[index]} but observed as %A{observed[index]}"
    | None when expected.Length <> observed.Length ->
        Some
            $"the engine asked {expected.Length} questions besides {skipped.Length} aims it cannot order, but {observed.Length} were observed"
    | None -> None

// How many hit-or-stand questions after an edit, in the edited round, went to
// a player whose count of turns the edit changed: it counts everyone still in
// the round as having taken as many turns as the asker had
let private recounted (timeline: Instant list) (asked: Asked list) : int =
    match timeline |> List.tryFindIndex (fun instant -> instant.Event.IsEdited) with
    | None -> 0
    | Some edit ->
        let scan = scanOf (List.take edit timeline)

        let taken (name: string) =
            Observation.TurnsTaken scan |> Map.tryFind name |> Option.defaultValue 0u

        let asker =
            match timeline[edit].Event with
            | Edited asker -> asker
            | _ -> invalidOp "an edit names its asker"

        let changed =
            timeline[edit].Players
            |> List.where (fun player -> Observation.InRound scan player && taken player.Name <> taken asker)
            |> List.map (fun player -> player.Name)

        // The edited round ends at the next RoundEnded, if the game got that far
        let ended =
            timeline
            |> List.indexed
            |> List.tryFind (fun (index, instant) -> index > edit && instant.Event.IsRoundEnded)
            |> Option.map fst
            |> Option.defaultValue timeline.Length

        asked
        |> List.where (fun call ->
            call.Question.Choice.IsChoice1Of2
            && call.Seen > edit
            && call.Seen <= ended
            && List.contains call.Question.Name changed
        )
        |> List.length

// How many of a game's aims went down each of the rarer roads, filling the
// aim fields of Roads from the questions the engine asked
let private roadsOf (timeline: Instant list) (asked: Asked list) : Roads =
    let aims = asked |> List.where (fun call -> call.Question.Choice.IsChoice2Of2)

    let deal3s =
        aims
        |> List.where (asks Strategy.Ask.WhoReceivesDeal3)
        |> List.map (fun call -> call.Seen)
        |> Set.ofList

    // The scan before each instant, which tells a card set aside during a
    // deal3 by the flipper's debt
    let scans =
        timeline
        |> List.scan (fun scan instant -> fst (Observation.Observe scan instant)) Observation.Start
        |> Array.ofList

    let count (condition: Asked -> bool) =
        aims |> List.where condition |> List.length

    let secondChance = ActionCard Card.SecondChance

    let passedInFlips (call: Asked) =
        asks Strategy.Ask.WhoReceivesSecondChance call && deal3s.Contains call.Seen

    // Everyone in the round besides the one passing it on, as the deal3 that
    // flipped it began
    let others (call: Asked) =
        timeline[call.Seen - 1].Players
        |> List.where (fun player -> player.Name <> call.Question.Name && Observation.InRound scans[call.Seen] player)

    {
        noRoads with
            AimedSetAside =
                count (fun call ->
                    not (asks Strategy.Ask.WhoReceivesSecondChance call)
                    && Observation.Owes scans[call.Seen] call.Question.Name > 0
                )
            AimedByTheFrozen =
                count (fun call ->
                    not (asks Strategy.Ask.WhoReceivesSecondChance call)
                    && not (
                        call.Question.OtherPlayers
                        |> List.exists (fun player -> player.Name = call.Question.Name)
                    )
                )
            AimsPassedInFlips = count passedInFlips
            AimsPastAHolder =
                count (fun call ->
                    passedInFlips call
                    && others call
                       |> List.exists (fun player -> List.contains secondChance player.Hand)
                )
            // Two second chances flipped to a target who held one, of which one
            // was passed on: the other was discarded
            AimsBeforeNoTaker =
                count (fun call ->
                    let dealtTo =
                        timeline[call.Seen - 1].Players
                        |> List.find (fun player -> player.Name = call.Question.Name)

                    passedInFlips call
                    && (aims |> List.where (fun other -> passedInFlips other && other.Seen = call.Seen)).Length = 1
                    && List.contains secondChance dealtTo.Hand
                    && match timeline[call.Seen].Event with
                       | Dealt3(_, _, cards) -> (cards |> List.where ((=) secondChance)).Length = 2
                       | _ -> false
                )
            AimsPassedTwice = (ambiguous asked).Length
            ForcedAims = count (fun call -> call.Question.OtherPlayers.Length = 1)
            AimsAfterRotation =
                count (fun call ->
                    let before = timeline[call.Seen - 1]
                    before.Event.IsRoundEnded
                    && call.Question.Name <> (List.head before.Players).Name
                )
    }

[<Fact>]
let ``the questions a timeline reveals are the questions the engine asked, one for one`` () =
    let roads = ResizeArray<Roads>()
    let mutable edits = 0
    let mutable continuations = 0
    let mutable recounts = 0

    let property =
        Prop.forAll
            (Arb.fromGen genGameWithFork)
            (fun (seed, lineup, fork) ->
                let log = ResizeArray<Asked>()

                let timeline, _, forked =
                    playForked (recording log) seed lineup (Option.map editing fork)

                let asked = List.ofSeq log

                roads.Add(roadsOf timeline asked)
                recounts <- recounts + recounted timeline asked

                if forked && fork |> Option.exists (fun fork -> fork.Edit.IsSome) then
                    edits <- edits + 1
                elif forked then
                    continuations <- continuations + 1

                match disagreement timeline asked with
                | None -> Prop.ofTestable true
                | Some reason -> Prop.label reason false
            )

    // A hundred games take about a second and reach every road guarded below
    // many times over
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 100, property)

    let total (road: Roads -> int) = roads |> Seq.sumBy road

    // Giving out a set-aside card is also the road on which a Froze or a
    // Dealt3 is not a decision, which the comparison above then holds to the
    // engine's count of decisions and turns
    Assert.True(total (fun roads -> roads.AimedSetAside) > 0, "no game gave out a card set aside during a deal3")

    Assert.True(total (fun roads -> roads.ForcedAims) > 0, "no game asked an aim with a single candidate")

    Assert.True(
        total (fun roads -> roads.AimsAfterRotation) > 0,
        "no game asked an aim first in a round after the first"
    )

    Assert.True(edits > 0, "no game was continued after an edit")
    Assert.True(continuations > 0, "no game was continued without an edit")
    Assert.True(recounts > 0, "no question after an edit went to a player whose turns the edit recounted")

// These seeds take the rarest roads an aim can take through a deal3, at a
// six-seat table where most seats always hit and the roads come up more often
// than in the property's games. Even here a second chance is passed on during
// the flips in about one game in twenty, passed past a holder or given out
// from outside the round in about one in a hundred and fifty, and passed on
// twice in about one in two thousand, and a search through thousands of seeds
// found only seed 8164 passing one on before one nobody is left to take. So
// the property above can miss them in a run, and these seeds take them every
// time
[<Fact>]
let ``the aims follow a deal3 down its rarest roads`` () =
    let roads (seed: int) =
        let log = ResizeArray<Asked>()
        let timeline, _, _ = playForked (recording log) seed rarestRoadsLineup None
        let asked = List.ofSeq log
        Assert.Equal(None, disagreement timeline asked)
        roadsOf timeline asked

    Assert.True(
        (roads 3).AimsPassedInFlips > 0,
        "seed 3 no longer passes a second chance on during a deal3's flips; pick a new seed"
    )

    Assert.True(
        (roads 164).AimsPastAHolder > 0,
        "seed 164 no longer passes a second chance past a player in the round who holds one; pick a new seed"
    )

    Assert.True(
        (roads 8164).AimsBeforeNoTaker > 0,
        "seed 8164 no longer passes a second chance on before flipping one nobody is left to take; pick a new seed"
    )

    Assert.True(
        (roads 80).AimedByTheFrozen > 0,
        "seed 80 no longer has a flipper frozen by their own set-aside freeze give out the next card; pick a new seed"
    )

    Assert.True(
        (roads 1266).AimsPassedTwice > 0,
        "seed 1266 no longer passes two second chances on during one deal3's flips; pick a new seed"
    )

// ---- Inference: the aiming model ----

// The aim given to another of its candidates
let private givenTo (name: string) (aim: Observation) : Observation = { aim with Choice = Choice2Of2(askOf aim, name) }

// Whom an aiming's decider gives the card of an aim to, at the aim's table
let private pickedBy (aiming: Aiming) (aim: Observation) : string =
    let picked =
        Inference.TargetDecider
            aiming
            (Random 0)
            Seq.empty
            (askOf aim)
            aim.Player
            aim.OtherPlayers
            aim.FinishedPlayers
            (aim.Deck, aim.Discards)
        |> Async.RunSynchronously

    picked.Name

// Every aim of a game of two to twelve seats on every kind of targeting
let private genAims: Gen<Observation list> =
    genGameWithFork
    |> Gen.map (fun (seed, lineup, _) ->
        let timeline, _, _ = playForked id seed lineup None

        observationsOf timeline
        |> List.where (fun observation -> observation.Choice.IsChoice2Of2)
    )

// The aimings an aim is judged by: each of the policies played plainly, and
// every way of taking sides among the names at its table and a stranger's,
// each way with the next of the policies in turn rather than with all of them,
// which would judge as many times more aimings as there are policies
let private aimingsAt (policies: Targeting list) (aim: Observation) : Aiming list =
    let names =
        "Stranger" :: (aim.OtherPlayers |> List.map (fun candidate -> candidate.Name))

    let sides =
        Inference.AimingCandidates [ Targeting.PlaysSpitefully ] names
        |> List.choose (fun aiming ->
            match aiming with
            | Aiming.TakesSides(favours, opposes, _) -> Some(favours, opposes)
            | Aiming.Plays _ -> None
        )

    (policies |> List.map Aiming.Plays)
    @ (sides
       |> List.mapi (fun index (favours, opposes) ->
           Aiming.TakesSides(favours, opposes, policies[index % policies.Length])
       ))

// An aiming judged at an aim: the likelihood it gives the candidate its
// decider picks, and the likelihood it gives each candidate in turn
type private Judged = {
    Aim: Observation
    Aiming: Aiming
    OfPick: float
    Likelihoods: float list
}

// Whether an aiming picks at random wherever it does not take a side
let private picksAtRandom (judged: Judged) : bool =
    match judged.Aiming with
    | Aiming.Plays Targeting.ChoosesRandomly
    | Aiming.TakesSides(_, _, Targeting.ChoosesRandomly) -> true
    | _ -> false

// Greedy at thresholds of its own joins the engine's policies. A policy that
// decides puts all of the probability on the candidate its decider picks, and
// one that picks at random spreads it evenly over everyone it picks among
[<Fact>]
let ``every aiming's likelihood is one where its decider aims, or even over a random pick, and sums to one`` () =
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.zip genAims (Gen.zip genProbability genProbability)))
            (fun (aims, (banksAbove, deal3sBelow)) ->
                let policies =
                    Targeting.PlaysGreedily(banksAbove, deal3sBelow) :: Inference.DefaultTargetings

                let judged = [
                    for aim in aims do
                        for aiming in aimingsAt policies aim ->
                            let likelihood (name: string) =
                                Inference.ProbabilityOfTarget aiming (givenTo name aim)

                            {
                                Aim = aim
                                Aiming = aiming
                                OfPick = likelihood (pickedBy aiming aim)
                                Likelihoods = aim.OtherPlayers |> List.map (fun candidate -> likelihood candidate.Name)
                            }
                ]

                // A claim, labelled with the first aiming and aim it fails for
                let labelled (claim: string) (holds: Judged -> bool) =
                    match judged |> List.tryFind (holds >> not) with
                    | None -> Prop.label claim true
                    | Some failed -> Prop.label $"{claim}, but not {failed.Aiming} at %A{failed.Aim}" false

                labelled
                    "a policy that decides puts all of the probability where its decider aims"
                    (fun judged -> picksAtRandom judged || judged.OfPick = 1.0)
                .&. labelled
                    "one that picks at random spreads it evenly over everyone it picks among, its decider's pick among them"
                    (fun judged ->
                        let among = judged.Likelihoods |> List.where (fun p -> p > 0.0)

                        not (picksAtRandom judged)
                        || (among |> List.forall (fun p -> p = 1.0 / float among.Length)
                            && judged.OfPick > 0.0)
                    )
                .&. labelled
                    "the likelihoods of every candidate sum to one"
                    (fun judged -> abs (List.sum judged.Likelihoods - 1.0) < 1e-12)
            )

    // Each game judges each of its aims by tens to hundreds of aimings, so
    // twenty games are already a broad check
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

// Ann's aims over twenty games of a table whose first seat, Ann, aims as
// given, where the targeting named Sides favours Carol and opposes Bob
let private aimsAs (targeting: Targeting) : Observation list =
    let lineup = [
        "Ann", Strategy.HitUntilScore 25u, targeting
        "Bob", Strategy.HitUntilScore 20u, Targeting.PlaysSpitefully
        "Carol", Strategy.AlwaysHits, Targeting.ChoosesRandomly
        "Dan", Strategy.HitUntilNumCards 5u, Targeting.PlaysGreedily(0.3, 0.2)
        "Eve", Strategy.HitUntilScore 30u, Targeting.ChoosesRandomly
    ]

    let decider =
        Strategy.Decider.Default
        |> Strategy.addTargetingStrategy
            "Sides"
            (Inference.TargetDecider(Aiming.TakesSides(Some "Carol", Some "Bob", Targeting.PlaysSpitefully)))

    [
        for seed in 1..20 -> Timeline.SimulateWithDecider (Random seed) decider lineup
    ]
    |> Observation.FromTimelines
    |> AsyncSeq.filter (fun aim -> aim.Name = "Ann" && aim.Choice.IsChoice2Of2)
    |> played

// Each targeting is named with next to no doubt, rather than by the order of
// the grid. Greedy at 0.3 and 0.2 would not do: twenty games never froze Ann
// with a card between 0.3 and 0.5 likely to bust her, which would tell it from
// greedy at 0.4 or 0.5
[<Fact>]
let ``fitting a handful of games names each targeting, and a player who takes sides`` () =
    let cases = [
        Targeting.ChoosesRandomly, Aiming.Plays Targeting.ChoosesRandomly
        Targeting.PlaysSpitefully, Aiming.Plays Targeting.PlaysSpitefully
        Targeting.PlaysGreedily(0.2, 0.1), Aiming.Plays(Targeting.PlaysGreedily(0.2, 0.1))
        Targeting.ChoosesExternally "Sides", Aiming.TakesSides(Some "Carol", Some "Bob", Targeting.PlaysSpitefully)
    ]

    for targeting, expected in cases do
        let model = Inference.Fit(aimsAs targeting) |> List.exactlyOne
        let likeliest, probability = List.head model.AimPosterior
        Assert.Equal(expected, likeliest)
        Assert.True(probability > 0.9, $"{targeting} was fitted as {likeliest} at {probability}")
        Assert.Equal(1.0, model.AimPosterior |> List.sumBy snd, 1e-12)

        // Twenty games show Ann making about fifty aims among two or more
        // candidates, and the fit should rest on no fewer than forty
        Assert.True(model.Aims >= 40, $"{targeting} was fitted from only {model.Aims} aims")

        // Only her aims are fitted, so how she plays is the prior, even over
        // the candidates, and her hit rate is NaN
        Assert.Equal(0, model.Observations)
        Assert.True(Double.IsNaN model.HitRate, $"{targeting} was fitted with a hit rate of {model.HitRate}")

        Assert.Equal<(Strategy * float) list>(strategyPrior, model.Posterior)

// Evidence of each aim on its own, scored over the names of its own table,
// folds into the evidence of them all scored at once over every name, even as
// the folding meets names it has not seen: a game at a table of Ann, Bob and
// Carol and another of Abe, Ann and Eve
[<Fact>]
let ``aim evidence folded one aim at a time is the evidence scored at once`` () =
    let tables = [
        [
            "Ann", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
            "Bob", Strategy.AlwaysHits, Targeting.ChoosesRandomly
            "Carol", Strategy.HitUntilScore 20u, Targeting.PlaysSpitefully
        ]
        [
            "Abe", Strategy.AlwaysHits, Targeting.ChoosesRandomly
            "Ann", Strategy.HitUntilScore 25u, Targeting.PlaysSpitefully
            "Eve", Strategy.HitUntilNumCards 4u, Targeting.PlaysGreedily(0.3, 0.2)
        ]
    ]

    // Any seeds do that show Ann aiming among two or more candidates at both
    // tables, which the guards below check
    let aims =
        tables
        |> List.mapi (fun index lineup ->
            let names = lineup |> List.map (fun (name, _, _) -> name)

            Timeline.SimulateWith (Random(40 + index)) lineup
            |> Observation.FromTimeline
            |> AsyncSeq.filter (fun aim -> aim.Name = "Ann" && aim.Choice.IsChoice2Of2 && aim.OtherPlayers.Length > 1)
            |> played
            |> List.map (fun aim -> names, aim)
        )
        |> List.concat

    Assert.True(
        aims |> List.exists (fun (names, _) -> List.contains "Bob" names),
        "Ann never aimed at the first table; pick a new seed"
    )

    Assert.True(
        aims |> List.exists (fun (names, _) -> List.contains "Abe" names),
        "Ann never aimed at the second table; pick a new seed"
    )

    // The default rate of out-of-character aims
    let epsilon = 0.1

    let evidenceOf (names: string list) (observations: Observation list) =
        Inference.EvidenceWith epsilon Inference.DefaultCandidates Inference.DefaultTargetings names "Ann" observations

    let folded =
        aims
        |> List.map (fun (names, aim) -> evidenceOf names [ aim ])
        |> List.reduce Inference.Combine

    let atOnce = evidenceOf [ "Abe"; "Ann"; "Bob"; "Carol"; "Eve" ] (List.map snd aims)

    Assert.Equal(atOnce, folded)

    // And each candidate's sum is the log of the likelihood of every aim,
    // which is the same under any strategy
    Assert.Equal<(Aiming * float) list>(
        atOnce.AimLogLikelihoods
        |> List.map (fun (candidate, _) ->
            candidate,
            aims
            |> List.sumBy (fun (_, aim) -> log (Inference.Likelihood epsilon Strategy.AlwaysHits candidate aim))
        ),
        atOnce.AimLogLikelihoods
    )

    Assert.Equal<string list>([ "Abe"; "Ann"; "Bob"; "Carol"; "Eve" ], folded.Names)
    Assert.Equal(aims.Length, folded.Aims)

    // Every policy played plainly, and with every way of favouring one of the
    // five names, opposing one, or favouring one and opposing another, each
    // once
    let grid = atOnce.AimLogLikelihoods |> List.map fst
    Assert.Equal(Inference.DefaultTargetings.Length * (1 + 5 + 5 + 5 * 4), grid.Length)
    Assert.Equal(grid.Length, (List.distinct grid).Length)

// The prior shares the weight of taking sides with a policy among however many
// ways the names allow, so with nothing seen, the ways together weigh as much
// as the policy played plainly, at a table of one name or of twelve. With no
// aims, the rate of out-of-character aims plays no part
[<Fact>]
let ``with no aims seen, taking sides with a policy weighs as much as playing it, at any table size`` () =
    let mismatches = [
        for count in [ 1; 3; 12 ] do
            let model =
                Inference.ModelFrom(
                    Inference.EvidenceWith
                        0.1
                        Inference.DefaultCandidates
                        Inference.DefaultTargetings
                        (List.take count seatNames)
                        "Ann"
                        []
                )

            for policy in Inference.DefaultTargetings do
                let plainly =
                    model.AimPosterior
                    |> List.find (fun (aiming, _) -> aiming = Aiming.Plays policy)
                    |> snd

                let sided =
                    model.AimPosterior
                    |> List.sumBy (fun (aiming, probability) ->
                        match aiming with
                        | Aiming.TakesSides(_, _, otherwise) when otherwise = policy -> probability
                        | _ -> 0.0
                    )

                if abs (plainly - sided) > 1e-12 then
                    yield $"{policy} over {count} names weighs {plainly} plainly but {sided} taking sides"
    ]

    Assert.Empty(mismatches)

// ---- Inference: taking sides ----

// A player at the table, and hands one card is unlikely and likely to bust
let private player (name: string) (firmScore: uint) (hand: Hand) : Player =
    Player.Make(name, Strategy.AlwaysHits, firmScore = firmScore, hand = hand)

let private unlikely: Hand = [ ValueCard Card.Twelve ]

let private likely: Hand = [
    ValueCard Card.Twelve
    ValueCard Card.Eleven
    ValueCard Card.Ten
    ValueCard Card.Nine
    ValueCard Card.Eight
]

// An aim at the table as it stands, from a full deck less everyone's cards.
// Its target is a placeholder that givenTo fills in
let private aimAt (ask: Strategy.Ask) (chooser: Player) (candidates: Player list) : Observation =
    let deck =
        chooser :: candidates
        |> List.distinctBy (fun player -> player.Name)
        |> List.collect (fun player -> player.Hand)
        |> List.fold Deck.Decrement Deck.Full

    {
        Name = chooser.Name
        Choice = Choice2Of2(ask, "")
        Round = 1u
        Turn = 1u
        Player = chooser
        OtherPlayers = candidates
        FinishedPlayers = []
        Deck = deck
        Discards = Deck.Empty
    }

// Whom an aiming picks, by its decider and by its likelihood, which must agree
let private pickOf (aiming: Aiming) (aim: Observation) : string =
    let picked = pickedBy aiming aim
    Assert.Equal(1.0, Inference.ProbabilityOfTarget aiming (givenTo picked aim))
    picked

let private ann = player "Ann" 10u []
let private spite = Targeting.PlaysSpitefully

// Ann aims at Bob, Carol and Dan, of whom Carol shows the most and Dan the
// least: spite freezes whoever shows the most and gives a second chance to
// whoever shows the least, so a side taken shows wherever it picks someone else
[<Fact>]
let ``one who takes sides freezes the opposed and passes a second chance to the favoured, leaving both out of the rest``
    ()
    =
    let showing = [
        player "Bob" 50u unlikely
        player "Carol" 80u unlikely
        player "Dan" 30u unlikely
    ]

    let freeze = aimAt Strategy.Ask.WhoToFreeze ann (showing @ [ ann ])
    let secondChance = aimAt Strategy.Ask.WhoReceivesSecondChance ann showing

    Assert.Equal("Carol", pickOf (Aiming.Plays spite) freeze)
    Assert.Equal("Bob", pickOf (Aiming.TakesSides(Some "Carol", None, spite)) freeze)
    Assert.Equal("Dan", pickOf (Aiming.TakesSides(None, Some "Dan", spite)) freeze)
    Assert.Equal("Dan", pickOf (Aiming.Plays spite) secondChance)
    Assert.Equal("Bob", pickOf (Aiming.TakesSides(None, Some "Dan", spite)) secondChance)
    Assert.Equal("Carol", pickOf (Aiming.TakesSides(Some "Carol", None, spite)) secondChance)

// A deal3 goes to the favoured when one card is unlikely to bust them, or else
// to the opposed when one is likely to, and otherwise as spite deals it, to
// the likeliest to bust of the rest. The first three tables put Carol,
// favoured, and Dan, opposed, just either side of the thresholds, by the
// copies of their values left among the 88 cards nobody holds
[<Fact>]
let ``one who takes sides deals three to the favoured while safe, to the opposed while at risk, and otherwise as their policy would``
    ()
    =
    let busts (aim: Observation) (name: string) =
        let player = aim.OtherPlayers |> List.find (fun player -> player.Name = name)
        Simulation.probabilityToBust aim.Deck aim.Discards player.Hand false

    let deal3 (carol: Hand) (dan: Hand) =
        aimAt Strategy.Ask.WhoReceivesDeal3 ann [
            player "Bob" 50u unlikely
            player "Carol" 80u carol
            player "Dan" 30u dan
            ann
        ]

    // The thresholds, private to Inference: a deal3 goes to the favoured at
    // one chance in ten or less of busting on a card, and to the opposed at
    // 0.35 or more
    let safe, atRisk = 0.1, 0.35

    let sides = Aiming.TakesSides(Some "Carol", Some "Dan", spite)

    // Carol busts on any of the eight Nines left, just under one in ten, and
    // Dan on four Fives, seven Eights, ten Elevens and ten Twelves, just over
    // 0.35
    let both =
        deal3 (values [ Card.Nine ]) (values [ Card.Five; Card.Eight; Card.Eleven; Card.Twelve ])

    Assert.Equal(8.0 / 88.0, busts both "Carol", 1e-9)
    Assert.Equal((4.0 + 7.0 + 10.0 + 10.0) / 88.0, busts both "Dan", 1e-9)
    Assert.Equal("Carol", pickOf sides both)

    // Carol holds a Ten, nine of which are left, just over one in ten, and Dan
    // busts on three Fours, eight Nines, ten Elevens and ten Twelves, still
    // just over 0.35
    let opposedOnly =
        deal3 (values [ Card.Ten ]) (values [ Card.Four; Card.Nine; Card.Eleven; Card.Twelve ])

    Assert.Equal(9.0 / 88.0, busts opposedOnly "Carol", 1e-9)
    Assert.Equal((3.0 + 8.0 + 10.0 + 10.0) / 88.0, busts opposedOnly "Dan", 1e-9)
    Assert.Equal("Dan", pickOf sides opposedOnly)

    // Dan's Four becomes a Three, two of which are left, just under 0.35
    let neither =
        deal3 (values [ Card.Ten ]) (values [ Card.Three; Card.Nine; Card.Eleven; Card.Twelve ])

    Assert.Equal(9.0 / 88.0, busts neither "Carol", 1e-9)
    Assert.Equal((2.0 + 8.0 + 10.0 + 10.0) / 88.0, busts neither "Dan", 1e-9)
    Assert.Equal("Bob", pickOf sides neither)

    // Spite deals three to Carol, likeliest to bust, and then to Dan, likelier
    // than Bob, but neither is dealt three by the sides: Carol is too likely to
    // bust to be helped, and Dan not likely enough to be harmed
    let rest = deal3 likely [ ValueCard Card.Twelve; ValueCard Card.Eleven ]
    Assert.True(busts rest "Dan" > busts rest "Bob", "Dan is no likelier to bust than Bob")
    Assert.True(busts rest "Dan" < atRisk, "Dan is likely enough to bust to be dealt three as the opposed")
    Assert.Equal("Carol", pickOf (Aiming.Plays spite) rest)
    Assert.Equal("Dan", pickOf (Aiming.TakesSides(Some "Carol", None, spite)) rest)
    Assert.Equal("Bob", pickOf sides rest)

    // Frozen, Ann hands out a card she set aside to Carol or Dan alone, and
    // with neither dealt three for their side, spite deals it to the likelier
    // to bust of the two
    let named =
        aimAt Strategy.Ask.WhoReceivesDeal3 ann [
            player "Carol" 80u (values [ Card.Ten ])
            player "Dan" 30u (values [ Card.Three; Card.Nine; Card.Eleven; Card.Twelve ])
        ]

    Assert.True(busts named "Carol" > safe, "Carol is unlikely enough to bust to be dealt three as the favoured")
    Assert.True(busts named "Dan" < atRisk, "Dan is likely enough to bust to be dealt three as the opposed")
    Assert.Equal("Dan", pickOf sides named)

// The end-of-game summary prints a fitted aiming this way, so a player who
// takes sides must read as whom they favour and whom they oppose
[<Fact>]
let ``a player who takes sides reads as whom they favour and oppose`` () =
    let greedy = Targeting.PlaysGreedily(0.3, 0.2)

    let aimings = [
        Aiming.Plays spite, "PlaysSpitefully"
        Aiming.TakesSides(Some "Carol", Some "Bob", spite), "for Carol and against Bob, otherwise PlaysSpitefully"
        Aiming.TakesSides(Some "Carol", None, greedy), "for Carol, otherwise PlaysGreedily 0.3 0.2"
        Aiming.TakesSides(None, Some "Bob", Targeting.ChoosesRandomly), "against Bob, otherwise ChoosesRandomly"
        Aiming.TakesSides(None, None, spite), "PlaysSpitefully"
    ]

    Assert.Equal<(Aiming * string) list>(aimings, aimings |> List.map (fun (aiming, _) -> aiming, string aiming))
