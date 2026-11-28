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
/// else plays as the search expected them to.
/// </summary>
[<RequireQualifiedAccess>]
type public Plan = {
    /// Every answer the solved seats give, in the order the game asks for them,
    /// starting with the question the search was made at
    Answers: Answer list
    /// Every event the game produces from the question the search was made at
    /// on, as the plan's replay played it. The plan is played for only as long
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
    /// How many of the answers, from the first, have been played so far. They
    /// are played for as long as the game goes as the plan foresaw and none of
    /// the rest once the game leaves the plan, though the game can also leave
    /// it after its last answer, with every answer played
    Played: int
} with

    /// <summary>
    /// Whether the protagonist wins outright. A tie for the top is not a win.
    /// </summary>
    member self.Won: bool = self.Margin > 0

/// <summary>
/// Searches the rest of a game for a plan: given where the game stands, a
/// System.Random wound to where the game's Random stands and a decider for how
/// everyone answers unaided, the answers the solved seats give that win the
/// game for the protagonist, found by replaying the rest of the game along one
/// line of answers after another.
/// </summary>
module internal Solver =
    /// <summary>
    /// Where a search starts: the table as a solved seat is asked to hit or
    /// stand, and what led to it, with how the rest of the game plays from
    /// there. Random gives a System.Random fresh from the game's seed and wound
    /// to where the game's stood, a new one for every game replayed, since a
    /// replay draws from it. Decider answers every question unaided: the solved
    /// seats as they would without a plan, which the search departs from, and
    /// everyone else as they are expected to play.
    /// </summary>
    [<NoComparison; NoEquality>]
    type internal Fork = {
        Random: unit -> System.Random
        Decider: Strategy.Decider
        History: Instant seq
        Seating: string list
        Round: uint
        Turns: Map<string, uint>
        Active: Player list
        Finished: Player list
        Decks: Deck * Deck
    }

    // A replayed game: the line it followed, which seat each of the solved
    // seats' questions was asked of, how many answers each allowed, and the
    // protagonist's margin
    [<NoComparison; NoEquality>]
    type private Replay = {
        Line: Map<int, int>
        Seats: string array
        Choices: int array
        Margin: int
    }

    // Plays the rest of the game from a fork along a line, scoring it for the
    // protagonist. A line is the questions the solved seats are asked, counted
    // from zero in the order the game asks them, mapped to the answer taken
    // there instead of the unaided one. The alternatives at a question are the
    // unaided answer first and then every other legal answer, so one is the
    // first departure, and the unaided answer is the fork's decider's. Every
    // question in a replay comes with the Random the engine derives from the
    // game's, which the fork winds to where the game's stood, so a line replays
    // its parent's game exactly up to its newest departure. Each replay also
    // gives the answers and the events of its game
    let private Play
        (protagonist: string)
        (solved: Set<string>)
        (fork: Fork)
        (departures: Map<int, int>)
        : Async<Replay * (Answer array * Event array)> = async {
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
                                fork.Decider.HitOrStand random strategy history round turn player others finished decks

                            let choice =
                                match pick 2, preferred with
                                | 0, _ -> preferred
                                | _, Strategy.Hit -> Strategy.Stand
                                | _, Strategy.Stand -> Strategy.Hit

                            answers.Add(Answer.HitOrStand(player.Name, round, turn, player.Hand, choice))
                            return choice
                        }
                    else
                        fork.Decider.HitOrStand random strategy history round turn player others finished decks
            Target =
                fun random targeting history ask chooser candidates finished decks ->
                    if solved.Contains chooser.Name then
                        async {
                            let! preferred =
                                fork.Decider.Target random targeting history ask chooser candidates finished decks

                            let alternatives =
                                preferred
                                :: (candidates |> List.where (fun candidate -> candidate.Name <> preferred.Name))

                            let target = List.item (pick alternatives.Length) alternatives
                            answers.Add(Answer.Aim(chooser.Name, ask, target.Name))
                            return target
                        }
                    else
                        fork.Decider.Target random targeting history ask chooser candidates finished decks
        }

        // The last instant is the RoundEnded where someone first reaches 200
        let rest =
            Timeline.ContinueWith
                (fork.Random())
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
    let private Children (departs: string -> bool) (parent: Replay) : Map<int, int> list =
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

    /// <summary>
    /// Searches the rest of a game from a fork for the answers the solved
    /// seats, the protagonist and any supporters, give that win it for the
    /// protagonist, replaying at most budget games. Starting from what the
    /// solved seats answer unaided, as the fork's decider answers them, the
    /// search departs from those answers one question at a time, replaying the
    /// rest of the game for each line it tries, and keeps the lines whose games
    /// went best for the protagonist to depart from further. The first line
    /// whose game the protagonist wins outright is the plan, or the best line
    /// once the budget is spent or every line has been tried. With supporters,
    /// it spends the first half of the budget on the protagonist's own
    /// questions alone, and departs at the supporters' too only in a game the
    /// protagonist cannot win that way. Replays run in parallel, but their
    /// results are taken in the order one at a time would take them, so a plan
    /// never depends on how they were scheduled. Report is told of every line
    /// the search departs from.
    /// </summary>
    let internal Solve
        (protagonist: string)
        (supporters: string list)
        (budget: int)
        (report: string -> unit)
        (fork: Fork)
        : Async<Plan> =
        let solved = Set.ofList (protagonist :: supporters)
        let replay = Play protagonist solved

        // Under Fable the replays run one at a time, so a batch of more would
        // only spend replays after its first win
        let cores =
#if FABLE_COMPILER
            1
#else
            System.Environment.ProcessorCount
#endif

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
                        (Children departs parent)
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
