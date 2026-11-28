namespace Flip7

/// <summary>
/// What happened at a single moment of a game.
/// </summary>
type public Event =
    | TableSeated
    | Drew of Name: string * Card: Card
    | Stood of Name: string
    | Busted of Name: string * Card: Card
    | Froze of Source: string * Target: string
    | SecondChancePassed of Source: string * Target: string
    | SecondChanceDiscarded of Name: string
    | Dealt3 of Source: string * Target: string * Cards: Card list * Passed: string list
    | Flip7Achieved of Name: string
    | Edited of Name: string
    | RoundEnded of Scores: Map<string, uint>

    override self.ToString() : string =
        match self with
        | TableSeated -> "the table is seated"
        | Drew(name, card) -> $"{name} drew {card}"
        | Stood name -> $"{name} stood"
        | Busted(name, card) -> $"{name} drew {card} and busted"
        | Froze(source, target) when source = target -> $"{source} drew Freeze and froze themselves"
        | Froze(source, target) -> $"{source} drew Freeze and froze {target}"
        | SecondChancePassed(source, target) -> $"{source} passed a SecondChance to {target}"
        | SecondChanceDiscarded name -> $"{name} discarded a duplicate SecondChance"
        | Dealt3(source, target, cards, passed) ->
            let cards = cards |> List.map string |> String.concat ", "

            let passing =
                match passed with
                | [] -> ""
                | passed -> passed |> String.concat " then " |> sprintf ", passing SecondChance to %s"

            $"{source} drew Deal3, dealing {target}: {cards}{passing}"
        | Flip7Achieved name -> $"{name} flipped 7 and ended the round!"
        | Edited name -> $"{name} edited the table"
        | RoundEnded scores ->
            let scores =
                scores
                |> Map.toList
                |> List.map (fun (name, score) -> $"{name} +{score}")
                |> String.concat "  "

            $"round over: {scores}"

    member public self.Actor() : string option =
        match self with
        | Drew(name, _) -> Some name
        | Stood name -> Some name
        | Busted(name, _) -> Some name
        | SecondChanceDiscarded name -> Some name
        | Flip7Achieved name -> Some name
        | Froze(_, target) -> Some target
        | SecondChancePassed(_, target) -> Some target
        | Dealt3(_, target, _, _) -> Some target
        | TableSeated -> None
        | Edited _ -> None
        | RoundEnded _ -> None

module public Event =
    // What starts the line of a deal3's that names whom a second chance passed
    // on during its flips went to, which no card's line starts with
    [<Literal>]
    let private PassedTo = "passed: "

    /// <summary>
    /// Converts an event to an array of lines: the event kind followed by its
    /// fields, one per line, a deal3's cards each on a line of their own and
    /// then whom each second chance passed on during its flips went to, each
    /// after "passed: ".
    /// </summary>
    let public Serialize (event: Event) : string array =
        match event with
        | TableSeated -> [| "TableSeated" |]
        | Drew(name, card) -> [| "Drew"; name; string card |]
        | Stood name -> [| "Stood"; name |]
        | Busted(name, card) -> [| "Busted"; name; string card |]
        | Froze(source, target) -> [| "Froze"; source; target |]
        | SecondChancePassed(source, target) -> [| "SecondChancePassed"; source; target |]
        | SecondChanceDiscarded name -> [| "SecondChanceDiscarded"; name |]
        | Dealt3(source, target, cards, passed) -> [|
            "Dealt3"
            source
            target
            yield! cards |> List.map string
            yield! passed |> List.map (fun name -> PassedTo + name)
          |]
        | Flip7Achieved name -> [| "Flip7Achieved"; name |]
        | Edited name -> [| "Edited"; name |]
        | RoundEnded scores -> [|
            "RoundEnded"
            yield! scores |> Map.toList |> List.map (fun (name, score) -> $"{name}: {score}")
          |]

    /// <summary>
    /// Parses an event from a sequence of lines: the event kind followed by its
    /// fields, one per line, a deal3's cards each on a line of their own and
    /// then whom each second chance passed on during its flips went to, each
    /// after "passed: ". A deal3 saved before those were recorded reads back as
    /// passing none on.
    /// </summary>
    let public Deserialize (lines: string seq) : Event =
        let passedTo (line: string) =
            line.StartsWith(PassedTo, System.StringComparison.Ordinal)

        match lines |> Seq.toList with
        | [ "TableSeated" ] -> TableSeated
        | [ "Drew"; name; card ] -> Drew(name, Card.Parse card)
        | [ "Stood"; name ] -> Stood name
        | [ "Busted"; name; card ] -> Busted(name, Card.Parse card)
        | [ "Froze"; source; target ] -> Froze(source, target)
        | [ "SecondChancePassed"; source; target ] -> SecondChancePassed(source, target)
        | [ "SecondChanceDiscarded"; name ] -> SecondChanceDiscarded name
        | "Dealt3" :: source :: target :: rest when rest |> List.skipWhile (passedTo >> not) |> List.forall passedTo ->
            let cards = rest |> List.takeWhile (passedTo >> not)
            let passed = rest |> List.skip cards.Length

            Dealt3(
                source,
                target,
                cards |> List.map Card.Parse,
                passed |> List.map (fun line -> line.Substring PassedTo.Length)
            )
        | [ "Flip7Achieved"; name ] -> Flip7Achieved name
        | [ "Edited"; name ] -> Edited name
        | "RoundEnded" :: scores ->
            scores
            |> List.map (fun line ->
                match line.LastIndexOf(": ", System.StringComparison.Ordinal) with
                | -1 -> raise (System.FormatException $"Invalid score line format: {line}")
                | index -> line.Substring(0, index), uint (line.Substring(index + 2))
            )
            |> Map.ofList
            |> RoundEnded
        | lines -> raise (System.FormatException $"Invalid event lines: %A{lines}")

/// <summary>
/// A snapshot of the game immediately after an event. Players still in the
/// round come first (the next player to act at the head), followed by players
/// who have stood, busted, or been frozen.
/// </summary>
type public Instant = {
    Event: Event
    Players: Player list
    Deck: Deck
    Discards: Deck
}
