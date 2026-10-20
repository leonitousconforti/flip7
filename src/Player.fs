namespace Flip7

module private Decoding =
    /// <summary>
    /// The float a strategy or targeting string holds, read the same on .NET
    /// and in JavaScript: an optional minus sign, ASCII digits with an optional
    /// fraction or a fraction alone, and an optional exponent, or exactly NaN,
    /// Infinity or -Infinity. That covers whatever WriteFloat writes on either
    /// platform, and anything else, spaces and other signs included, is None.
    /// The characters are checked here, since the platforms' own parsers accept
    /// different strings, and only then converted with float, which is
    /// Double.Parse with the invariant culture on .NET and JavaScript's own
    /// conversion under Fable, both correctly rounded.
    /// </summary>
    let internal ReadFloat (text: string) : float option =
        match text with
        | "NaN" -> Some System.Double.NaN
        | "Infinity" -> Some System.Double.PositiveInfinity
        | "-Infinity" -> Some System.Double.NegativeInfinity
        | _ ->
            // Whether the character at the index is one of these
            let isAt (index: int) (characters: string) : bool =
                index < text.Length && characters.IndexOf(text[index]) >= 0

            // The index just past the run of digits from the index
            let digitsFrom (start: int) : int =
                let mutable index = start

                while isAt index "0123456789" do
                    index <- index + 1

                index

            let signed = if isAt 0 "-" then 1 else 0
            let integerEnd = digitsFrom signed

            let fractionEnd =
                if isAt integerEnd "." then
                    digitsFrom (integerEnd + 1)
                else
                    integerEnd

            // An exponent without digits is left unread, so the string is
            // refused for not having been read to its end
            let exponentEnd =
                if isAt fractionEnd "eE" then
                    let digits =
                        if isAt (fractionEnd + 1) "+-" then
                            fractionEnd + 2
                        else
                            fractionEnd + 1

                    let exponentDigitsEnd = digitsFrom digits
                    if exponentDigitsEnd > digits then
                        exponentDigitsEnd
                    else
                        fractionEnd
                else
                    fractionEnd

            let hasDigits = integerEnd > signed || fractionEnd > integerEnd + 1

            if hasDigits && exponentEnd = text.Length then
                Some(float text)
            else
                None

module private Encoding =
    /// <summary>
    /// A float as a strategy or targeting string writes it, in the shortest
    /// form that reads back as the same double on the platform writing it,
    /// except that JavaScript writes -0 as 0.
    /// </summary>
    let internal WriteFloat (value: float) : string =
#if FABLE_COMPILER
        string value
#else
        value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
#endif

open Decoding
open Encoding

/// <summary>
/// A strategy decides whether a player hits or stands. Strategies are
/// represented as data rather than functions so that they can be serialized and
/// deserialized; use Strategy.Decider.Default to evaluate one. The numbers in
/// the string form are read strictly, and the same on .NET and in JavaScript.
/// </summary>
[<RequireQualifiedAccess>]
type public Strategy =
    | AlwaysHits
    | AlwaysStands
    | RandomWithProbability of float
    | HitUntilScore of uint
    | HitUntilNumCards of uint
    | HitUntilBustProbability of float
    | HitUntilNaiveBustProbability of float
    | SoftHitUntilScore of Threshold: uint * Temperature: float
    | HitUntilTotal of uint
    | HitUntilUniqueValues of uint
    | ChasesFlip7 of Score: uint * Uniques: uint
    | EmboldenedBySecondChance of uint
    | HitWhileBehindLeader of uint
    | StandsAfterTurn of uint
    | PlaysLikeAHuman of Caution: uint
    | MaximizesExpectedValue
    | Custom of string

    override self.ToString() : string =
        match self with
        | AlwaysHits -> "AlwaysHits"
        | AlwaysStands -> "AlwaysStands"
        | RandomWithProbability probability -> $"RandomWithProbability {WriteFloat probability}"
        | HitUntilScore threshold -> $"HitUntilScore {threshold}"
        | HitUntilNumCards threshold -> $"HitUntilNumCards {threshold}"
        | HitUntilBustProbability threshold -> $"HitUntilBustProbability {WriteFloat threshold}"
        | HitUntilNaiveBustProbability threshold -> $"HitUntilNaiveBustProbability {WriteFloat threshold}"
        | SoftHitUntilScore(threshold, temperature) -> $"SoftHitUntilScore {threshold} {WriteFloat temperature}"
        | HitUntilTotal target -> $"HitUntilTotal {target}"
        | HitUntilUniqueValues threshold -> $"HitUntilUniqueValues {threshold}"
        | ChasesFlip7(score, uniques) -> $"ChasesFlip7 {score} {uniques}"
        | EmboldenedBySecondChance threshold -> $"EmboldenedBySecondChance {threshold}"
        | HitWhileBehindLeader margin -> $"HitWhileBehindLeader {margin}"
        | StandsAfterTurn turns -> $"StandsAfterTurn {turns}"
        | PlaysLikeAHuman caution -> $"PlaysLikeAHuman {caution}"
        | MaximizesExpectedValue -> "MaximizesExpectedValue"
        | Custom name -> $"Custom {name}"

    static member public Parse(string: string) : Strategy =
        let invalid () =
            raise (System.FormatException $"Invalid strategy string: {string}")

        let readFloat (value: string) =
            match ReadFloat value with
            | Some read -> read
            | None -> invalid ()

        // A uint in plain digits. Comparing with the value written back refuses
        // the signs, spaces and leading zeros UInt32 also reads, and a value
        // out of range is refused rather than overflowing. The string function
        // is named in full, since the parameter hides it
        let readUint (value: string) =
            match System.UInt32.TryParse value with
            | true, read when Operators.string read = value -> read
            | _ -> invalid ()

        if string.StartsWith("Custom ", System.StringComparison.Ordinal) then
            Custom(string.Substring("Custom ".Length))
        else
            match string.Split ' ' with
            | [| "AlwaysHits" |] -> AlwaysHits
            | [| "AlwaysStands" |] -> AlwaysStands
            | [| "RandomWithProbability"; probability |] -> RandomWithProbability(readFloat probability)
            | [| "HitUntilScore"; threshold |] -> HitUntilScore(readUint threshold)
            | [| "HitUntilNumCards"; threshold |] -> HitUntilNumCards(readUint threshold)
            | [| "HitUntilBustProbability"; threshold |] -> HitUntilBustProbability(readFloat threshold)
            | [| "HitUntilNaiveBustProbability"; threshold |] -> HitUntilNaiveBustProbability(readFloat threshold)
            | [| "SoftHitUntilScore"; threshold; temp |] -> SoftHitUntilScore(readUint threshold, readFloat temp)
            | [| "HitUntilTotal"; target |] -> HitUntilTotal(readUint target)
            | [| "HitUntilUniqueValues"; threshold |] -> HitUntilUniqueValues(readUint threshold)
            | [| "ChasesFlip7"; score; uniques |] -> ChasesFlip7(readUint score, readUint uniques)
            | [| "EmboldenedBySecondChance"; threshold |] -> EmboldenedBySecondChance(readUint threshold)
            | [| "HitWhileBehindLeader"; margin |] -> HitWhileBehindLeader(readUint margin)
            | [| "StandsAfterTurn"; turns |] -> StandsAfterTurn(readUint turns)
            | [| "PlaysLikeAHuman"; caution |] -> PlaysLikeAHuman(readUint caution)
            | [| "MaximizesExpectedValue" |] -> MaximizesExpectedValue
            | _ -> invalid ()

    static member TryParse(string: string) : Strategy option =
        try
            string |> Strategy.Parse |> Some
        with :? System.FormatException ->
            None

/// <summary>
/// A targeting policy decides who an action card is given to: who gets frozen,
/// who is dealt three cards, and who is passed a second chance its holder
/// cannot keep. Like a strategy it is data rather than a function so that it
/// can be serialized and deserialized; use Strategy.Decider.Default to evaluate
/// one. The escape hatch is called ChoosesExternally rather than Custom because
/// Strategy.Custom is matched unqualified in several places and a second case
/// of the same name in this namespace would rebind those matches.
///
/// One who takes sides favours a player, opposes one, or both: a second chance
/// goes to the favoured player whenever they can take it, a freeze to the
/// opposed player whenever they are in the round, and a deal3 to the favoured
/// player when one card is unlikely to bust them, with a chance of at most 0.1,
/// or else to the opposed player when one is likely to, with a chance of at
/// least 0.35, since three forced flips are a gift to a hand they seldom bust
/// and a blow to one they often do. Every other aim goes as Otherwise would aim
/// it among the candidates but the favoured and the opposed, or among them all
/// when nobody else is left, so the favoured player is never frozen nor the
/// opposed one helped while anyone else can be. TakesSides(None, None,
/// otherwise) plays exactly as otherwise. Otherwise may be any targeting: one
/// that takes sides takes them again among the rest, while ChoosesExternally
/// raises in Decider.Default as it does on its own, since a decider registered
/// for a label answers only for ChoosesExternally of that label itself. It is
/// written with its names between double quotes, a backslash before any quote
/// or backslash in them, and Otherwise last, as in TakesSides for "Carol"
/// against "Bob" otherwise PlaysSpitefully. The numbers of PlaysGreedily are
/// read strictly, and the same on .NET and in JavaScript. A chain of TakesSides
/// holds at most three, counting the outermost. Parse refuses a deeper one with
/// a FormatException, Timeline refuses to seat a player on one with an
/// ArgumentException where the lineup is written, and Decider.Default, and
/// Inference's likelihood of an aim, raise InvalidOperationException for one.
/// ToString still writes any value however deep, since a ToString that throws
/// breaks debuggers and printing, and what it writes for one too deep does not
/// parse.
/// </summary>
[<RequireQualifiedAccess>]
type public Targeting =
    | ChoosesRandomly
    | PlaysSpitefully
    | PlaysGreedily of BanksAbove: float * Deal3sBelow: float
    | TakesSides of Favours: string option * Opposes: string option * Otherwise: Targeting
    | ChoosesExternally of string

    /// <summary>
    /// How many TakesSides a chain may hold, counting the outermost.
    /// </summary>
    static member public SidesLimit: int = 3

    /// <summary>
    /// Whether a targeting takes sides more than SidesLimit deep. It counts in
    /// a loop that stops one past the limit, so a value nested however deep is
    /// judged at once and without recursion.
    /// </summary>
    static member public TakesSidesTooDeep(targeting: Targeting) : bool =
        let mutable depth = 0
        let mutable rest = targeting
        let mutable counting = true

        while counting && depth <= Targeting.SidesLimit do
            match rest with
            | TakesSides(_, _, otherwise) ->
                depth <- depth + 1
                rest <- otherwise
            | _ -> counting <- false

        depth > Targeting.SidesLimit

    override self.ToString() : string =
        let side (word: string) (name: string option) =
            match name with
            | Some name -> " " + word + " \"" + name.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
            | None -> ""

        // A link of the chain at a time rather than by recursion, so that any
        // value, however deep, is written in time linear in its length
        let written = System.Text.StringBuilder()
        let mutable rest = Some self

        while rest.IsSome do
            let text, next =
                match rest.Value with
                | ChoosesRandomly -> "ChoosesRandomly", None
                | PlaysSpitefully -> "PlaysSpitefully", None
                | PlaysGreedily(banksAbove: float, deal3sBelow: float) ->
                    $"PlaysGreedily {WriteFloat banksAbove} {WriteFloat deal3sBelow}", None
                | TakesSides(favours, opposes, otherwise) ->
                    "TakesSides" + side "for" favours + side "against" opposes + " otherwise ", Some otherwise
                | ChoosesExternally name -> $"ChoosesExternally {name}", None

            written.Append(text) |> ignore
            rest <- next

        written.ToString()

    static member public Parse(string: string) : Targeting =
        let invalid () =
            raise (System.FormatException $"Invalid targeting string: {string}")

        let readFloat (value: string) =
            match ReadFloat value with
            | Some read -> read
            | None -> invalid ()

        // Whether the string holds the text at the index
        let holds (index: int) (text: string) : bool =
            let mutable matched = index + text.Length <= string.Length
            let mutable offset = 0

            while matched && offset < text.Length do
                matched <- string[index + offset] = text[offset]
                offset <- offset + 1

            matched

        // The name between double quotes at the index, a backslash before any
        // quote or backslash in it, and the index just past it. It is unescaped
        // in one pass, gathering the runs between escapes
        let readName (start: int) : string * int =
            if not (holds start "\"") then
                invalid ()

            let name = System.Text.StringBuilder()
            let mutable run = start + 1
            let mutable index = start + 1
            let mutable closed = false

            while not closed do
                if index >= string.Length then
                    invalid ()
                elif string[index] = '"' then
                    name.Append(string.Substring(run, index - run)) |> ignore
                    closed <- true
                elif string[index] = '\\' then
                    if
                        index + 1 < string.Length
                        && (string[index + 1] = '"' || string[index + 1] = '\\')
                    then
                        name.Append(string.Substring(run, index - run)) |> ignore
                        run <- index + 1
                        index <- index + 2
                    else
                        invalid ()
                else
                    index <- index + 1

            name.ToString(), index + 1

        // The name a side is written with at the index, if the word for it is
        // there, and the index past it
        let readSide (index: int) (word: string) : string option * int =
            let written = " " + word + " "

            if holds index written then
                let name, next = readName (index + written.Length)
                Some name, next
            else
                None, index

        // The sides taken, innermost first, read with a running index so the
        // string is read once, and refused the moment a TakesSides more than
        // SidesLimit deep begins, before the rest of the string is read
        let mutable sides = []
        let mutable depth = 0
        let mutable index = 0

        while holds index "TakesSides" do
            if depth = Targeting.SidesLimit then
                raise (
                    System.FormatException
                        $"Invalid targeting string, which takes sides more than {Targeting.SidesLimit} deep: {string}"
                )

            let favours, next = readSide (index + "TakesSides".Length) "for"
            let opposes, next = readSide next "against"

            if not (holds next " otherwise ") then
                invalid ()

            sides <- (favours, opposes) :: sides
            depth <- depth + 1
            index <- next + " otherwise ".Length

        let policy =
            if holds index "ChoosesExternally " then
                ChoosesExternally(string.Substring(index + "ChoosesExternally ".Length))
            else
                match string.Substring(index).Split ' ' with
                | [| "ChoosesRandomly" |] -> ChoosesRandomly
                | [| "PlaysSpitefully" |] -> PlaysSpitefully
                | [| "PlaysGreedily"; banksAbove; deal3sBelow |] ->
                    PlaysGreedily(readFloat banksAbove, readFloat deal3sBelow)
                | _ -> invalid ()

        sides
        |> List.fold (fun otherwise (favours, opposes) -> TakesSides(favours, opposes, otherwise)) policy

    static member TryParse(string: string) : Targeting option =
        try
            string |> Targeting.Parse |> Some
        with :? System.FormatException ->
            None

type public Player = {
    Name: string
    Strategy: Strategy
    Targeting: Targeting
    FirmScore: uint
    Hand: Hand
} with

    static member Make
        (name: string, strategy: Strategy, ?firmScore: uint, ?hand: Hand, ?targeting: Targeting)
        : Player
        =
        let firmScore = defaultArg firmScore 0u
        let hand = defaultArg hand []
        let targeting = defaultArg targeting Targeting.ChoosesRandomly

        {
            Name = name
            Strategy = strategy
            Targeting = targeting
            FirmScore = firmScore
            Hand = hand
        }

module internal Player =
    /// <summary>
    /// What a player is showing: banked points plus whatever their hand is
    /// currently worth, or just the banked points once they have busted.
    /// </summary>
    let internal Showing (player: Player) : uint =
        if Hand.IsBust player.Hand then
            player.FirmScore
        else
            player.FirmScore + Hand.Score player.Hand
