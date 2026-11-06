// What the library does, printed the same way from .NET and from JavaScript:
// CI runs this program on .NET and, compiled by Fable, on Node, and fails when
// the two print anything different. Whole seeded games cover the deal, every
// strategy and targeting policy and the engine itself, and the rest covers the
// string forms, decks and the exceptions bad input raises. Only what both
// platforms can print exactly is printed: floats as ToString("R") writes them,
// and an exception by the type a match finds, since Fable cannot name a type.
module Parity

open System
open System.Globalization
open Flip7
open FSharp.Control

let private lineups = [
    "pair",
    [
        "Alice", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
        "Bob", Strategy.MaximizesExpectedValue, Targeting.PlaysSpitefully
    ]
    "four",
    [
        "Alice", Strategy.PlaysLikeAHuman 20u, Targeting.PlaysSpitefully
        "Bob", Strategy.SoftHitUntilScore(25u, 0.5), Targeting.PlaysGreedily(0.8, 0.2)
        "Chloe",
        Strategy.HitUntilBustProbability 0.3,
        Targeting.TakesSides(Some "Alice", None, Targeting.ChoosesRandomly)
        "Dave", Strategy.RandomWithProbability 0.6, Targeting.ChoosesRandomly
    ]
    "eight",
    [
        "Ann", Strategy.ChasesFlip7(15u, 5u), Targeting.PlaysSpitefully
        "Ben", Strategy.EmboldenedBySecondChance 18u, Targeting.PlaysGreedily(0.5, 0.3)
        "Cal", Strategy.HitWhileBehindLeader 30u, Targeting.TakesSides(None, Some "Ann", Targeting.PlaysSpitefully)
        "Dee", Strategy.StandsAfterTurn 3u, Targeting.ChoosesRandomly
        "Eve", Strategy.HitUntilUniqueValues 4u, Targeting.PlaysGreedily(0.9, 0.1)
        "Fay", Strategy.HitUntilTotal 22u, Targeting.ChoosesRandomly
        "Gus", Strategy.HitUntilNaiveBustProbability 0.35, Targeting.PlaysSpitefully
        "Hal", Strategy.HitUntilNumCards 3u, Targeting.ChoosesRandomly
    ]
]

let private seeds = [ 0..99 ]

let private written (value: float) =
    value.ToString("R", CultureInfo.InvariantCulture)

let private parsed (read: 'T option) =
    match read with
    | Some value -> string value
    | None -> "refused"

// A string on one line, whatever it holds
let private shown (text: string) =
    text.Replace("\n", "\\n").Replace("\t", "\\t")

let private floats = [
    0.0
    -0.0
    0.1 + 0.2
    1.0 / 3.0
    1e-7
    1.5e-5
    0.0001
    0.00015
    1e16
    12345678901234567.0
    1e17
    1e21
    123456789012345678901.0
    -2.5e-300
    Double.Epsilon
    Double.MaxValue
    nan
    infinity
    -infinity
]

let private numbers = [
    ""
    "-"
    "."
    "+0.5"
    "--1"
    "1,5"
    "0x10"
    "1_0"
    " 0.5"
    "0.5\n"
    "１"
    "1e"
    "1e+"
    "1.2.3"
    "nan"
    "Infinity0"
    "∞"
    "1E-05"
    "0.00001"
    "1e-5"
    "5."
    ".5"
    "-0"
    "1e+21"
    "1e999"
    "NaN"
    "-Infinity"
    "01"
    "4294967295"
    "4294967296"
]

// The type of an exception, as far as a match can tell it on both platforms
let private kind (error: exn) =
    match error with
    | :? FormatException -> "FormatException"
    | :? ArgumentException as error -> $"ArgumentException for {error.ParamName}"
    | :? InvalidOperationException -> "InvalidOperationException"
    | _ -> "another exception"

let private alice = "Alice", Strategy.AlwaysHits, Targeting.ChoosesRandomly
let private bob = "Bob", Strategy.AlwaysStands, Targeting.ChoosesRandomly

let private refusals: (string * (unit -> Async<unit>)) list = [
    "an unknown card", fun () -> async { Card.Parse "Joker" |> ignore }
    "a strategy with a negative count", fun () -> async { Strategy.Parse "HitUntilScore -1" |> ignore }
    "a targeting with words for numbers", fun () -> async { Targeting.Parse "PlaysGreedily a b" |> ignore }
    "a deck with a fractional count",
    fun () -> async {
        Deck.Deserialize(Array.append [| "Deal3: 1.5" |] (Deck.Serialize Deck.Full |> Array.tail))
        |> ignore
    }
    "an unknown event", fun () -> async { Event.Deserialize [ "Shuffled" ] |> ignore }
    "a draw from two empty piles", fun () -> async { Deck.Draw1With (Random 1) (Deck.Empty, Deck.Empty) |> ignore }
    "a name seated twice", fun () -> Timeline.SimulateWith (Random 1) [ alice; alice ] |> AsyncSeq.iter ignore
    "a strategy decided externally",
    fun () ->
        Timeline.SimulateWith (Random 1) [ "Alice", Strategy.Custom "Prompt", Targeting.ChoosesRandomly; bob ]
        |> AsyncSeq.iter ignore
    "a study with no strategies",
    fun () -> async {
        let! _ = Study.OfTimelinesWith [] [ Targeting.ChoosesRandomly ] []
        return ()
    }
    "a study continued at an epsilon of 0",
    fun () -> async {
        let! _ = Study.ContinueWith 0.0 [] Study.Nothing
        return ()
    }
]

let private report = async {
    for name, lineup in lineups do
        for seed in seeds do
            let! instants = Timeline.SimulateWith (Random seed) lineup |> AsyncSeq.toListAsync

            let events =
                instants
                |> List.map (fun instant -> Event.Serialize instant.Event |> String.concat " ")
                |> String.concat " | "

            printfn $"game {name} {seed}: {events}"

    for value in floats do
        let strategy = Strategy.SoftHitUntilScore(20u, value)
        let targeting =
            Targeting.TakesSides(Some "Alice", None, Targeting.PlaysGreedily(value, 0.25))
        printfn $"written {written value}: {strategy} reads {parsed (Strategy.TryParse(string strategy))}"
        printfn $"written {written value}: {targeting} reads {parsed (Targeting.TryParse(string targeting))}"

    for text in numbers do
        let probability = Strategy.TryParse("RandomWithProbability " + text)
        let count = Strategy.TryParse("HitUntilScore " + text)
        let banksAbove = Targeting.TryParse("PlaysGreedily " + text + " 0.25")
        printfn $"read {shown text}: {parsed probability}, {parsed count}, {parsed banksAbove}"

    let decks =
        ("full", (Deck.Full, Deck.Empty))
        :: [ for seed in 0..9 -> $"random {seed}", Deck.RandomWith(Random seed) ]

    for name, (deck, discards) in decks do
        let pdf =
            Deck.pdf deck
            |> Map.toList
            |> List.map (fun (card, probability) -> $"{card} {written probability}")
            |> String.concat ", "

        let lines = Deck.Serialize deck |> String.concat ", "
        printfn $"deck {name}: {lines} / {Deck.Count discards} discarded"
        printfn $"deck {name}: pdf {pdf}"
        printfn $"deck {name}: ev {written (Deck.ev deck)}, var {written (Deck.var deck)}, ec {Deck.ec deck}"

    for name, refused in refusals do
        let! outcome = Async.Catch(async.Delay refused)

        match outcome with
        | Choice1Of2() -> printfn $"refused {name}: accepted"
        | Choice2Of2 error -> printfn $"refused {name}: {kind error}: {error.Message}"
}

// Fable has no Async.RunSynchronously, and Node runs until the report is done
#if FABLE_COMPILER
Async.StartImmediate report
#else
Async.RunSynchronously report
#endif
