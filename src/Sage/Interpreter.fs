/// <summary>
/// Reads the steps Observation's scan replays each instant of a timeline in as
/// the calls the engine made on the game's Random: how many values each step
/// takes, and what the draws among them say about the seed.
/// </summary>
module internal Flip7.Interpreter

// A draw of a card from a pile as the Random sees it, laid out in Map order
// with the copies of a card side by side: how many cards the pile holds, how
// many of them sit before the drawn card's copies, and how many copies of it
// there are
let private Layout (pile: Deck) (card: Card) : struct (int * int * int) =
    let count, below, copies =
        pile
        |> Map.fold
            (fun (count, below, copies) other held ->
                let held = int held

                if other < card then count + held, below + held, copies
                elif other = card then count + held, below, held
                else count + held, below, copies
            )
            (0, 0, 0)

    struct (count, below, copies)

/// <summary>
/// How many values a step takes from the game's Random. A question, whether to
/// hit or stand or who to aim an action card at, takes one, which seeds the
/// Random the question is asked with, and so does one the play editor was
/// opened at instead of answering it, since the Random drew it all the same.
/// Drawing a card from Count cards shuffles them and takes the head, which
/// takes Count - 1 values, so a card drawn from one card takes none. The pile
/// must hold a copy of the card, as it does in every replay that is possible.
/// </summary>
let internal Values (step: Observation.Step) : int =
    match step with
    | Observation.Step.Asked _
    | Observation.Step.Interrupted _
    | Observation.Step.Aimed _ -> 1
    | Observation.Step.Drawn(card, pile) ->
        let struct (count, _, _) = Layout pile card
        count - 1

/// <summary>
/// What the draws among steps taken from a position on say about the values the
/// game's Random produced, and so about the seed: the first value a draw from
/// Count cards, two or more, takes is the one Next(0, Count) turns into the
/// index it draws, one of the drawn card's copies, which sit side by side in
/// the pile laid out in Map order.
/// </summary>
let internal Constraints (position: int) (steps: Observation.Step list) : KnuthSubtractiveGenerator.Constraint list =
    steps
    |> List.mapFold
        (fun position (step: Observation.Step) ->
            let constrained: KnuthSubtractiveGenerator.Constraint option =
                match step with
                | Observation.Step.Drawn(card, pile) ->
                    let struct (count, below, copies) = Layout pile card

                    if count >= 2 then
                        let struct (low, high) =
                            KnuthSubtractiveGenerator.Interval count below (below + copies - 1)

                        Some { Position = position; Low = low; High = high }
                    else
                        None
                | Observation.Step.Asked _
                | Observation.Step.Interrupted _
                | Observation.Step.Aimed _ -> None

            constrained, position + Values step
        )
        position
    |> fst
    |> List.choose id
