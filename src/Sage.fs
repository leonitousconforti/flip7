namespace Flip7

open FSharp.Control

/// <summary>
/// One answer a solved seat gives the game: whether it hits or stands holding a
/// hand on a turn of a round, or who it aims an action card at. The question
/// travels with the answer, so whoever plays a plan can tell when the game asks
/// something the plan does not expect.
/// </summary>
[<RequireQualifiedAccess>]
type public Answer =
    | HitOrStand of Seat: string * Round: uint * Turn: uint * Hand: Hand * Choice: Strategy.HitOrStand
    | Aim of Seat: string * Ask: Strategy.Ask * Target: string

    member self.Seat: string =
        match self with
        | HitOrStand(seat, _, _, _, _)
        | Aim(seat, _, _) -> seat

    override self.ToString() : string =
        let showing (hand: Hand) =
            if List.isEmpty hand then
                "nothing"
            else
                hand |> Hand.Serialize |> String.concat ", "

        match self with
        | HitOrStand(seat, round, turn, hand, Strategy.Hit) ->
            $"{seat} hits holding {showing hand} on turn {turn} of round {round}"
        | HitOrStand(seat, round, turn, hand, Strategy.Stand) ->
            $"{seat} stands holding {showing hand} on turn {turn} of round {round}"
        | Aim(seat, ask, target) -> $"{seat} answers {ask} with {target}"

/// <summary>
/// A line of play from the moment a solved seat was asked to hit or stand:
/// every answer the solved seats give from then on, in the order the game asks
/// for them, and how the protagonist finishes when they are given and everyone
/// else plays as Sage modelled them.
/// </summary>
[<RequireQualifiedAccess>]
type public Plan = {
    /// Every answer the solved seats give, in the order the game asks for them,
    /// starting with the question the search was made at
    Answers: Answer list
    /// Every event the game produces from the question the search was made at
    /// on, as the plan's replay played it. Sage plays the plan for only as long
    /// as the game goes this way
    Foresees: Event list
    /// The protagonist's final score less the best of everyone else's: positive
    /// when the protagonist wins outright
    Margin: int
    /// Where in the answers the plan departs from what the solved seats would
    /// answer unaided, counted from zero
    Departures: int list
    /// How many games the search replayed before it stopped
    Replays: int
    /// How many of the answers, from the first, Sage has played so far. It plays
    /// them for as long as the game goes as the plan foresaw and none of the rest
    /// once the game leaves the plan, though the game can also leave it after its
    /// last answer, with every answer played
    Played: int
} with

    /// <summary>
    /// Whether the protagonist wins outright. A tie for the top is not a win.
    /// </summary>
    member self.Won: bool = self.Margin > 0

module internal Replayed =
    // How a player other than the solved seats aims in Sage's replays, given
    // the evidence of them and what they declared. The engine plays a declared
    // targeting itself, taking sides or not, with the very Random a replay
    // hands it, so a replay that keeps the declaration aims exactly as the live
    // game will, where a fit could only come close: a few aims at random can
    // look like spite, and no amount of them names a policy off the grid. So
    // only one who declared ChoosesExternally, on its own or as what they
    // otherwise do after taking sides, which the engine cannot play and the
    // table's decider answers as it pleases, is replayed as fitted, sides and
    // all: as they most likely aim, once seen aiming at one of two or more
    // players, and spitefully until then, however many decisions the evidence
    // holds. One who takes sides chooses deliberately, so where their most
    // likely targeting takes sides and otherwise picks at random, it is that no
    // policy explains the rest of their aims rather than that they roll the
    // engine's dice, and a random pick in a replay would match theirs only by
    // chance: they aim as their most likely targeting that never picks at
    // random. Only the likeliest are wanted, so rather than the posterior,
    // which sorts every candidate, it takes the first of the greatest
    // log-weights, which is the posterior's head
    let internal Aiming (evidence: PlayerEvidence option) (declared: Targeting) : Targeting =
        let rec external (targeting: Targeting) =
            match targeting with
            | Targeting.TakesSides(_, _, otherwise) -> external otherwise
            | Targeting.ChoosesExternally _ -> true
            | _ -> false

        match external declared, evidence |> Option.filter (fun evidence -> evidence.Aims > 0) with
        | true, Some evidence ->
            let weights = Inference.AimLogWeights evidence

            let decides (targeting: Targeting, _) =
                match targeting with
                | Targeting.ChoosesRandomly
                | Targeting.TakesSides(_, _, Targeting.ChoosesRandomly) -> false
                | _ -> true

            match weights |> List.maxBy snd |> fst with
            | Targeting.TakesSides(_, _, Targeting.ChoosesRandomly) as likeliest ->
                match weights |> List.where decides with
                | [] -> likeliest
                | deciding -> deciding |> List.maxBy snd |> fst
            | likeliest -> likeliest
        | true, None -> Targeting.PlaysSpitefully
        | false, _ -> declared

// What Sage has followed of the game it is playing: how many of its instants it
// has seen, the first and the last of them, by which it tells the game from any
// other, everyone at the table, what it studied of them, and everything it has
// learnt from the instants
[<NoComparison; NoEquality>]
type internal Followed = {
    First: Instant
    Last: Instant
    Seen: int
    Seating: string list option
    Table: string list
    Studied: Map<string, PlayerEvidence>
    Oracle: Oracle
    Scan: Observation.Scan
    Evidence: Map<string, PlayerEvidence>
}

// Where a search starts: the table as a solved seat is asked to hit or stand,
// with every other player seated to play and aim as Sage models them, what led
// to it, and the oracle that pinned the game's seed, which winds a Random to
// where the game's stood for every game replayed from there
[<NoComparison; NoEquality>]
type internal Fork = {
    Oracle: Oracle
    History: Instant seq
    Seating: string list
    Round: uint
    Turns: Map<string, uint>
    Active: Player list
    Finished: Player list
    Decks: Deck * Deck
}

// A replayed game: the line it followed, which seat each of the solved seats'
// questions was asked of, how many answers each allowed, and the protagonist's
// margin
[<NoComparison; NoEquality>]
type internal Replay = {
    Line: Map<int, int>
    Seats: string array
    Choices: int array
    Margin: int
}

/// <summary>
/// Plays one game for a protagonist and any supporters, its seats. A table
/// seats each of them as Strategy.Custom and Targeting.ChoosesExternally under
/// their own name and registers what Strategies holds for them on its own
/// decider, which asks Sage its seats' questions and answers everyone else's
/// itself. Every question comes with the game so far, which is how Sage follows
/// the game: it keeps what it learnt from the instants it has seen and takes in
/// only the ones each of its seats' questions adds, however many the game has
/// produced since the last. At the start of every game it refuses a seat of its
/// own declared any other way, since its plans count on answering every
/// question the seat is asked.
///
/// Sage models every player by name, both how they hit or stand and how they
/// aim their action cards. It starts from what it studied of them, if anything,
/// carried into its game as the next of the study's sitting, so the sides they
/// took in the study's last games most likely hold but need not, and adds every
/// decision and every aim the game reveals, so a player it knows nothing of is
/// learnt as they play. At the same time it recovers the seed of the game's
/// Random with an Oracle, which once pinned winds a Random to where the game's
/// stands, so Sage can play the rest of the game in advance exactly as the
/// engine will, wherever everyone answers as it expects.
///
/// When a solved seat is asked to hit or stand, Sage has no line for the
/// question and the seed is pinned, it searches from the table as it is. Every
/// other player plays their most likely fitted strategy, or maximizes expected
/// value if never seen deciding, and aims as they declared, which for one of
/// the engine's targeting policies the replay plays with the very Random the
/// live game will. One who declared ChoosesExternally, on its own or as what
/// they otherwise do after taking sides, aims as they most likely aim once seen
/// aiming at one of two or more players, and spitefully until then. One who
/// takes sides chooses deliberately, so where their most likely targeting takes
/// sides and otherwise picks at random, they aim as their most likely targeting
/// that never picks at random, which the live game is likelier to match than a
/// replay's dice. The engine's own decider answers for them, seated as Sage
/// models them, never the table's, which may be people at the terminal.
/// Starting from what the solved seats would answer unaided, the search departs
/// from those answers one question at a time, replaying the rest of the game
/// for each line it tries, and keeps the lines whose games went best for the
/// protagonist to depart from further. The first line whose game the
/// protagonist wins outright is the plan, or the best line once the budget is
/// spent or every line has been tried, and Sage gives its answers for as long
/// as the game goes as the line's replay did and asks what the line expects,
/// searching again at the next hit-or-stand question once it does not. With
/// supporters, each search spends the first half of its budget on the
/// protagonist's own questions alone, and departs at the supporters' too only
/// in a game the protagonist cannot win that way. Replays run in parallel, but
/// their results are taken in the order one at a time would take them, so a
/// plan never depends on how they were scheduled.
///
/// Until the seed is pinned, or in a game that does not start at its
/// TableSeated or that no seed dealt, the solved seats answer unaided: they
/// maximize expected value and aim spitefully, as the engine plays those with
/// the Random each question came with. The seed is pinned in a game dealt by a
/// Random fresh from its seed at the TableSeated instant, usually within its
/// first dozen cards, and almost never in any other: no more than about one
/// game in a thousand that no seed dealt shows a seed as pinned for a while,
/// and Sage's plans then foresee a game that is not coming, so they break,
/// though every answer stays legal.
///
/// Every answer is one the seat could have given at the table. The plan
/// foresees the cards because the seed gives them away, and the opponents
/// because the engine hands every question in a replay the same Random the live
/// game will: an opponent whose fitted strategy is the one they play, dice and
/// all, answers in the replay exactly as they will at the table, while a
/// person's choices are only modelled, and once one differs from the model the
/// game is no longer the one the plan foresees, so Sage drops the plan, even
/// where the game goes on asking what the plan expects, as it can after a
/// freeze aimed elsewhere or a stand where the model hit. A plan is a fact
/// about one game, and says nothing about how strong its answers would be in
/// any other.
///
/// An edit changes the cards the seed would have dealt, so an Edited instant
/// ends the plan being played, and Sage searches the edited game at the next
/// hit-or-stand question a solved seat is asked. After an Edited instant the
/// play editor's fork counts every active player's turns as one fewer than the
/// edited question's turn, whatever each had taken, and Observation counts them
/// as the engine does, so Sage's replays number turns as the live game does and
/// the decisions it learns from carry the turns their players were asked about.
///
/// The budget is the most games one search replays, 5000 unless given, which at
/// the terminal app's table is far more than a search ever needs. Progress, if
/// given, is told of every line a search departs from and of what every search
/// found, a line each.
///
/// A Sage serves one game at a time: its questions must not arrive from two
/// games at once. It tells one game from another by the instants themselves,
/// since two games of a lineup start with equal TableSeated instants: a
/// question whose game so far does not start with the very instant the game
/// Sage was following started with, or no longer passes through the last
/// instant Sage saw of it, the last that one of its seats' questions showed
/// it, starts a new game, which forgets the old game's plans and what it
/// showed of the players but keeps what Sage studied. A game continued from
/// after that instant stays the same game, plans and all, even where it drops
/// instants the table produced since. Cancelling the game cancels any search
/// under way.
/// </summary>
type public Sage(protagonist: string, ?supporters: string list, ?study: Study, ?budget: int, ?progress: string -> unit)
    =
    let supporters = defaultArg supporters []
    let study = defaultArg study Study.Nothing

    // The most games a search replays. In interactive play it is a backstop
    // rather than a limit, since a search either wins within a few dozen
    // replays or runs out of lines to try first. At the terminal app's five
    // seats, Sage beside two people who roll dice of their own and two of the
    // app's bots, 60 games made 508 searches, which took 2 ms at the median, 20
    // ms at the 99th percentile and under 80 ms at most, and replayed at most
    // 130 games, the same searches whatever the budget from 2000 to
    // 50000. At twelve seats the slowest of 91 took 0.3 s. Seated at nothing
    //        against bots already at 100 or 150, so that few lines win, a
    //        search replayed at most 978 games. A whole game of five replays in
    //        some 0.26 ms ten at a time on ten cores, so even a search that
    //        spends the whole budget from the first round takes about 1.3 s
    let budget = defaultArg budget 5000
    let report = defaultArg progress ignore
    let solved = Set.ofList (protagonist :: supporters)

    do
        if List.contains protagonist supporters then
            invalidArg (nameof supporters) $"{protagonist} cannot support themselves"

        if budget < 1 then
            invalidArg (nameof budget) "the search needs at least one game"

    // What a solved seat answers unaided, with the Random the question came
    // with. Its seat declares Custom and ChoosesExternally, which the engine
    // leaves to a decider, so it maximizes expected value and aims spitefully
    let unaidedHitOrStand (random: System.Random) : Strategy.HitOrStandDecider =
        Strategy.Decider.Default.HitOrStand random Strategy.MaximizesExpectedValue

    let unaidedTarget (random: System.Random) : Strategy.TargetDecider =
        Strategy.Decider.Default.Target random Targeting.PlaysSpitefully

    // The game being followed, the answers of the plan being played that the
    // game has yet to ask for, the events the plan foresees and how many
    // instants of the game came before them, and every plan made this game,
    // newest first
    let mutable game: Followed option = None
    let mutable line: Answer list = []
    let mutable foreseen: Event array = [||]
    let mutable forkedAt = 0
    let mutable plans: Plan list = []

    // Plays the next answer of the plan being played, which is always the
    // newest, leaving the rest of its line
    let play (rest: Answer list) =
        line <- rest

        match plans with
        | plan :: older -> plans <- { plan with Played = plan.Played + 1 } :: older
        | [] -> ()

    // The evidence of a player now: what Sage studied of them and what this
    // game has shown, which share the study's grids of strategies and policies,
    // and for a player at the table the very names their aims are scored over
    let evidenceOf (name: string) : PlayerEvidence option =
        let studied =
            game
            |> Option.bind (fun followed -> followed.Studied |> Map.tryFind name)
            |> Option.orElse (Study.EvidenceOf study name)

        let shown =
            game |> Option.bind (fun followed -> followed.Evidence |> Map.tryFind name)

        match studied, shown with
        | Some studied, Some shown -> Some(Inference.Combine studied shown)
        | Some evidence, None
        | None, Some evidence -> Some evidence
        | None, None -> None

    // What Sage makes of a player now
    let modelOf (name: string) : PlayerModel option =
        evidenceOf name |> Option.map Inference.ModelFrom

    // Takes one more instant of the game into account. Every observation is
    // scored over everyone at the table and every name what Sage studied of the
    // player is over, so that this game's evidence of them shares one grid
    // however many aims it holds, and the grid of what Sage studied of them. An
    // aim with a single candidate shows nothing, so it is not kept
    let observe (followed: Followed) (instant: Instant) : Followed =
        let scan, observations = Observation.Observe followed.Scan instant

        let evidence =
            observations
            |> List.fold
                (fun (evidence: Map<string, PlayerEvidence>) observation ->
                    let names =
                        followed.Studied
                        |> Map.tryFind observation.Name
                        |> Option.map (fun studied -> studied.Names)
                        |> Option.defaultValue followed.Table

                    let single =
                        Inference.Evidence
                            0.1
                            (Study.StrategiesOf study)
                            (Study.TargetingsOf study)
                            names
                            observation.Name
                            [ observation ]

                    if single.Observations + single.Aims = 0 then
                        evidence
                    else
                        evidence
                        |> Map.change
                            observation.Name
                            (function
                            | Some accumulated -> Some(Inference.Combine accumulated single)
                            | None -> Some single
                            )
                )
                followed.Evidence

        {
            followed with
                Last = instant
                Seen = followed.Seen + 1
                Oracle = Oracle.Observe followed.Oracle instant
                Scan = scan
                Evidence = evidence
        }

    // Brings what Sage follows up to the game so far, taking in only the
    // instants it has not seen, or starting afresh when the game so far does
    // not continue the game it was following
    let follow (history: Instant seq) : Followed option =
        let instants = Array.ofSeq history

        let continues (followed: Followed) =
            instants.Length >= followed.Seen
            && obj.ReferenceEquals(instants[0], followed.First)
            && obj.ReferenceEquals(instants[followed.Seen - 1], followed.Last)

        match game with
        | Some followed when continues followed ->
            let unseen = instants[followed.Seen ..]

            // An edit changes the cards the seed would have dealt, so the plan
            // being played foresaw a game that is no longer coming, even where
            // the game goes on asking what the plan expects
            if unseen |> Array.exists (fun instant -> instant.Event.IsEdited) then
                line <- []

            // A person's choices are only modelled, so once the game shows an
            // event the plan's replay did not, the plan foresees a game that is
            // no longer coming, even where the game goes on asking what the
            // plan expects
            let strays (index: int, instant: Instant) =
                let offset = followed.Seen + index - forkedAt
                offset >= 0 && (offset >= foreseen.Length || instant.Event <> foreseen[offset])

            if unseen |> Array.indexed |> Array.exists strays then
                line <- []

            game <- Some(Array.fold observe followed unseen)
        | _ ->
            line <- []
            plans <- []

            game <-
                instants
                |> Array.tryHead
                |> Option.map (fun first ->
                    let seating =
                        if first.Event.IsTableSeated then
                            Some(first.Players |> List.map (fun player -> player.Name))
                        else
                            None

                    for name in protagonist :: supporters do
                        if seating |> Option.exists (List.contains name >> not) then
                            invalidOp $"{name} has no seat at this table"

                    // Every question a seat of Sage's is asked must come to
                    // Sage, since its plans count on answering them all, where
                    // a seat declared Custom or ChoosesExternally under its
                    // name for only one part would have the engine answer the
                    // other
                    for seat in first.Players do
                        if
                            solved.Contains seat.Name
                            && (seat.Strategy <> Strategy.Custom seat.Name
                                || seat.Targeting <> Targeting.ChoosesExternally seat.Name)
                        then
                            invalidOp
                                $"Sage plays {seat.Name}, who must be seated as Strategy.Custom \"{seat.Name}\" and Targeting.ChoosesExternally \"{seat.Name}\" with both registered from Sage.Strategies, not as {seat.Strategy} and {seat.Targeting}"

                    let table = first.Players |> List.map (fun player -> player.Name)

                    // What the study holds of everyone at the table, carried
                    // into this game as the next of the study's sitting and so
                    // widened to the table's names as well as its own, once a
                    // game, rather than at every search
                    let studied =
                        table
                        |> List.choose (fun name ->
                            Study.EvidenceOf study name
                            |> Option.map (fun evidence -> name, Inference.Carry Study.Stickiness table evidence)
                        )
                        |> Map.ofList

                    let started = {
                        First = first
                        Last = first
                        Seen = 0
                        Seating = seating
                        Table = table
                        Studied = studied
                        Oracle = Oracle.Start
                        Scan = Observation.Start
                        Evidence = Map.empty
                    }

                    Array.fold observe started instants
                )

        game

    // Replays the rest of the game from a fork along a line: the questions the
    // solved seats are asked, counted from zero in the order the game asks
    // them, mapped to the answer taken there instead of the unaided one. The
    // alternatives at a question are the unaided answer first and then every
    // other legal answer, so one is the first departure. Every question in a
    // replay comes with the Random the engine derives from the game's, which
    // the replay winds to where the game's stood, so a line replays its
    // parent's game exactly up to its newest departure. Each replay also gives
    // the answers and the events of its game
    let replay (fork: Fork) (departures: Map<int, int>) : Async<Replay * (Answer array * Event array)> = async {
        let engine = Strategy.Decider.Default
        let answers = ResizeArray<Answer>()
        let choices = ResizeArray<int>()

        let pick (alternatives: int) : int =
            let question = choices.Count
            choices.Add alternatives
            departures |> Map.tryFind question |> Option.defaultValue 0

        let decider: Strategy.Decider = {
            HitOrStand =
                fun random strategy history round turn player others finished decks ->
                    if solved.Contains player.Name then
                        async {
                            let! preferred =
                                unaidedHitOrStand random history round turn player others finished decks

                            let choice =
                                match pick 2, preferred with
                                | 0, _ -> preferred
                                | _, Strategy.Hit -> Strategy.Stand
                                | _, Strategy.Stand -> Strategy.Hit

                            answers.Add(Answer.HitOrStand(player.Name, round, turn, player.Hand, choice))
                            return choice
                        }
                    else
                        engine.HitOrStand random strategy history round turn player others finished decks
            Target =
                fun random targeting history ask chooser candidates finished decks ->
                    if solved.Contains chooser.Name then
                        async {
                            let! preferred = unaidedTarget random history ask chooser candidates finished decks

                            let alternatives =
                                preferred
                                :: (candidates |> List.where (fun candidate -> candidate.Name <> preferred.Name))

                            let target = List.item (pick alternatives.Length) alternatives
                            answers.Add(Answer.Aim(chooser.Name, ask, target.Name))
                            return target
                        }
                    else
                        engine.Target random targeting history ask chooser candidates finished decks
        }

        let random =
            match Oracle.RandomOf fork.Oracle with
            | Some random -> random
            | None -> invalidOp "a game can only be replayed once its seed is pinned"

        // The last instant is the RoundEnded where someone first reaches 200
        let rest =
            Timeline.ContinueWith
                random
                decider
                fork.History
                fork.Seating
                fork.Round
                fork.Turns
                fork.Active
                fork.Finished
                fork.Decks

        let! instants = AsyncSeq.toArrayAsync rest

        match Array.tryLast instants with
        | None -> return invalidOp "the game ended before it was searched"
        | Some final ->
            let score (player: Player) = int player.FirmScore

            let protagonists, others =
                final.Players |> List.partition (fun player -> player.Name = protagonist)

            let answers = answers.ToArray()

            let replayed = {
                Line = departures
                Seats = answers |> Array.map (fun answer -> answer.Seat)
                Choices = choices.ToArray()
                Margin =
                    (protagonists |> List.exactlyOne |> score)
                    - (others |> List.map score |> List.fold max 0)
            }

            return replayed, (answers, instants |> Array.map (fun instant -> instant.Event))
    }

    // Every line that departs once more than this one does, at a question one
    // of the departing seats is asked after its newest departure, earliest
    // first, so that no line is reached twice. A line's game is its parent's
    // game up to that newest departure, so the parent's replay already shows
    // which questions come after it and what each allows
    let children (departs: string -> bool) (parent: Replay) : Map<int, int> list =
        let newest =
            if parent.Line.IsEmpty then
                -1
            else
                fst (Map.maxKeyValue parent.Line)

        [
            for question in newest + 1 .. parent.Choices.Length - 1 do
                if departs parent.Seats[question] then
                    for alternative in 1 .. parent.Choices[question] - 1 do
                        Map.add question alternative parent.Line
        ]

    // Searches the rest of the game from a fork
    let solve (fork: Fork) : Async<Plan> =
        let cores = System.Environment.ProcessorCount

        // Best first: of every line replayed but not yet departed from, the one
        // whose game the protagonist finished best in, then the one with the
        // fewest departures, then the one replayed first. Its departures are
        // replayed in turn, and the search stops at the first win, at the
        // limit, or once every line has been tried, returning the best game it
        // saw, with its answers, and the replays so far. Departures are
        // replayed a batch at a time, the batches growing from one to as many
        // as there are cores, and taken in order: the replays a batch made
        // after its first win are never counted, so the search returns what it
        // would replaying one at a time, and a search that wins at once spends
        // little more
        let rec search
            (limit: int)
            (departs: string -> bool)
            (frontier: Set<int * int * int>)
            (replayed: Map<int, Replay>)
            (pending: Map<int, int> list)
            (width: int)
            (replays: int)
            (best: Replay * (Answer array * Event array))
            : Async<(Replay * (Answer array * Event array)) * int> = async {
            if (fst best).Margin > 0 || replays >= limit then
                return best, replays
            else

            match pending with
            | [] when frontier.IsEmpty -> return best, replays
            | [] ->
                let (_, _, id) as top = frontier.MinimumElement
                let parent = replayed[id]
                report $"replay {replays}: departing from a line with margin {parent.Margin}"

                return!
                    search
                        limit
                        departs
                        (frontier.Remove top)
                        (Map.remove id replayed)
                        (children departs parent)
                        width
                        replays
                        best
            | pending ->
                let batch = pending |> List.truncate (min width (limit - replays))

                let! games =
                    Async.Parallel(batch |> List.map (replay fork), maxDegreeOfParallelism = cores)

                let rec take (index: int) frontier replayed replays (best: Replay * (Answer array * Event array)) =
                    if index = games.Length || (fst best).Margin > 0 then
                        frontier, replayed, replays, best
                    else
                        let game, _ = games[index]
                        let best =
                            if game.Margin > (fst best).Margin then
                                games[index]
                            else
                                best
                        let frontier = Set.add (-game.Margin, game.Line.Count, replays) frontier
                        take (index + 1) frontier (Map.add replays game replayed) (replays + 1) best

                let frontier, replayed, replays, best = take 0 frontier replayed replays best

                return!
                    search
                        limit
                        departs
                        frontier
                        replayed
                        (List.skip batch.Length pending)
                        (min cores (2 * width))
                        replays
                        best
        }

        async {
            let! root = replay fork Map.empty
            let frontier = Set.singleton (-(fst root).Margin, 0, 0)
            let replayed = Map.add 0 (fst root) Map.empty

            // The protagonist's own departures first, with half the budget when
            // there are supporters. A supporter's departure only helps through
            // the protagonist's game, and searching every seat's at once spends
            // the budget on lines the protagonist did not need, so the
            // supporters depart only in the games the protagonist cannot win
            // alone
            let alone =
                if List.isEmpty supporters then
                    budget
                else
                    max 1 (budget / 2)

            let! best, replays = search alone ((=) protagonist) frontier replayed [] 1 1 root

            let! (best, (answers, events)), replays =
                if (fst best).Margin > 0 || List.isEmpty supporters then
                    async.Return(best, replays)
                else
                    search budget (fun _ -> true) frontier replayed [] 1 replays best

            return {
                Answers = List.ofArray answers
                Foresees = List.ofArray events
                Margin = best.Margin
                Departures = best.Line |> Map.keys |> List.ofSeq
                Replays = replays
                Played = 0
            }
        }

    // What Sage answers when the named seat of its own is asked to hit or
    // stand, once it has taken in the game so far
    let hitOrStandFor (name: string) (random: System.Random) : Strategy.HitOrStandDecider =
        fun history round turn player others finished decks -> async {
            if player.Name <> name then
                return
                    invalidOp
                        $"Sage's strategy for {name} was asked to hit or stand for {player.Name}; only {name} may be seated as Strategy.Custom \"{name}\""
            else

            let followed = follow history

            match line, followed with
            | Answer.HitOrStand(seat, planned, plannedTurn, hand, choice) :: rest, _ when
                seat = player.Name
                && planned = round
                && plannedTurn = turn
                && hand = player.Hand
                ->
                play rest
                return choice
            | _, Some({ Seating = Some seating } as followed) when (Oracle.SeedsOf followed.Oracle).IsPinned ->
                line <- []

                // The asker has taken one turn fewer than the one they are
                // asked about, and everyone else as many as the engine counts,
                // as the scan does
                let taken = Observation.TurnsTaken followed.Scan

                let turns =
                    player :: others
                    |> List.map (fun active ->
                        if active.Name = player.Name then
                            active.Name, turn - 1u
                        else
                            active.Name, taken |> Map.tryFind active.Name |> Option.defaultValue 0u
                    )
                    |> Map.ofList

                // Everyone else, with the evidence of them
                let unsolved =
                    player :: others @ finished
                    |> List.where (fun seat -> not (solved.Contains seat.Name))
                    |> List.map (fun seat -> seat, evidenceOf seat.Name)

                // Everyone else plays as they most likely play, or maximizes
                // expected value if never seen deciding, however many aims Sage
                // has seen them make, and aims in the replays as
                // Replayed.Aiming has them aim
                let models =
                    unsolved
                    |> List.map (fun (seat, evidence) ->
                        let strategy =
                            evidence
                            |> Option.filter (fun evidence -> evidence.Observations > 0)
                            |> Option.map (Inference.ModelFrom >> Inference.MostLikely >> fst)
                            |> Option.defaultValue Strategy.MaximizesExpectedValue

                        seat.Name, (strategy, Replayed.Aiming evidence seat.Targeting)
                    )
                    |> Map.ofList

                let modelled (seat: Player) : Player =
                    match models |> Map.tryFind seat.Name with
                    | Some(strategy, targeting) -> { seat with Strategy = strategy; Targeting = targeting }
                    | None -> seat

                let! plan =
                    solve {
                        Oracle = followed.Oracle
                        History = history
                        Seating = seating
                        Round = round
                        Turns = turns
                        Active = player :: others |> List.map modelled
                        Finished = finished |> List.map modelled
                        Decks = decks
                    }

                plans <- plan :: plans

                let outcome =
                    if plan.Won then
                        $"wins by {plan.Margin}"
                    else
                        $"falls {-plan.Margin} short"

                report
                    $"round {round}, turn {turn}: {player.Name} searched {plan.Replays} games for a line that {outcome}"

                match plan.Answers with
                | Answer.HitOrStand(_, _, _, _, choice) :: rest ->
                    play rest
                    foreseen <- Array.ofList plan.Foresees
                    forkedAt <- followed.Seen
                    return choice
                | _ -> return invalidOp "a search always starts at the question it was made at"
            | _ ->
                line <- []
                return! unaidedHitOrStand random history round turn player others finished decks
        }

    // What Sage answers when the named seat of its own is asked to aim, once it
    // has taken in the game so far
    let targetFor (name: string) (random: System.Random) : Strategy.TargetDecider =
        fun history ask chooser candidates finished decks -> async {
            if chooser.Name <> name then
                return
                    invalidOp
                        $"Sage's targeting for {name} was asked to aim for {chooser.Name}; only {name} may be seated as Targeting.ChoosesExternally \"{name}\""
            else

            follow history |> ignore

            let planned =
                match line with
                | Answer.Aim(seat, plannedAsk, target) :: rest when seat = chooser.Name && plannedAsk = ask ->
                    candidates
                    |> List.tryFind (fun candidate -> candidate.Name = target)
                    |> Option.map (fun candidate -> candidate, rest)
                | _ -> None

            match planned with
            | Some(candidate, rest) ->
                play rest
                return candidate
            | None ->
                // A card cannot be searched from mid-resolution, so the seat
                // answers unaided until it is next asked to hit or stand
                line <- []
                return! unaidedTarget random history ask chooser candidates finished decks
        }

    let strategies =
        protagonist :: supporters
        |> List.map (fun name -> name, (hitOrStandFor name, targetFor name))
        |> Map.ofList

    /// <summary>
    /// What Sage answers for each of its seats, the protagonist and every
    /// supporter, by name: the seat's hit-or-stand and targeting
    /// implementations, each handed the Random its question comes with. A table
    /// seats each of them as Strategy.Custom and Targeting.ChoosesExternally
    /// under their name, as Sage requires at the start of every game, and
    /// registers both on its own decider, which answers everyone else, such as
    /// the opponents' decider here:
    /// <code>
    /// sage.Strategies |> Map.fold (fun decider name (hitOrStand, target) ->
    /// decider |> Strategy.addHitOrStandStrategy name hitOrStand |>
    /// Strategy.addTargetingStrategy name target) opponents
    /// </code>
    /// Each implementation answers its own seat alone, and raises
    /// InvalidOperationException when asked for anyone else, as it is when
    /// another player declares its seat's label.
    /// </summary>
    member _.Strategies
        : Map<string, (System.Random -> Strategy.HitOrStandDecider) * (System.Random -> Strategy.TargetDecider)> =
        strategies

    /// <summary>
    /// Every plan Sage's searches have made so far this game, in the order it
    /// made them, each with how many of its answers Sage has played.
    /// </summary>
    member _.Plans: Plan list = List.rev plans

    /// <summary>
    /// What Sage makes of a player now, by name: what it studied of them,
    /// carried into the game it follows as the next of the study's sitting, or
    /// as studied before it follows any, together with every decision and every
    /// aim they have made in the game so far, as of the last of its seats'
    /// questions, or None for a player it has never seen decide or aim at one
    /// of two or more players. Sage follows a game only through its seats'
    /// questions, so whatever the game shows after the last of them, such as
    /// the answer to it, never counts here. The model's targeting is the fit,
    /// which is not always how Sage's replays aim for the player: they keep to
    /// a targeting the player declared, and replay a player whose most likely
    /// targeting takes sides and otherwise picks at random as the most likely
    /// targeting that never picks at random.
    /// </summary>
    member _.ModelOf(name: string) : PlayerModel option = modelOf name
