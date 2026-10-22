// What the tests use of xunit, FsCheck and the .NET runtime that Fable lacks,
// so the same test files compile to JavaScript: Fact, Theory and InlineData
// mark the tests FableEntry.fsx writes the entry for, Assert checks as xunit's
// does, a seeded property runner draws from generators written as FsCheck's
// are, with no shrinking, SHA-256 and hex take the digests of seeded games,
// and Async.RunSynchronously drains the async trampoline in a loop, which
// holds for the engine because it never waits on a timer or a promise. Last
// comes the runner the entry hands every test to, which knows the tests that
// fail in JavaScript and why. Compiled only under Fable.
//
// What JavaScript cannot run stays in #if !FABLE_COMPILER, or has an
// #if FABLE_COMPILER of its own: a Recording, since Fable cannot inherit
// System.Random, and whatever plays on one, a task's timeout, the GC,
// ParamName and an exception's type. LookaheadTests and PersistenceTests test
// what the library compiles only for .NET.

namespace Xunit

open System

type FactAttribute() =
    inherit Attribute()

type TheoryAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
type InlineDataAttribute([<ParamArray>] data: obj array) =
    inherit Attribute()

type Assert =
    static member Fail(message: string) : unit = failwith message

    static member Equal<'T>(expected: 'T, actual: 'T) : unit =
        if not (Unchecked.equals expected actual) then
            failwithf "Assert.Equal failed\nexpected: %A\nactual:   %A" expected actual

    static member Equal(expected: float, actual: float, tolerance: float) : unit =
        if not (abs (expected - actual) <= tolerance) then
            failwithf "Assert.Equal failed\nexpected: %g\nactual:   %g (within %g)" expected actual tolerance

    static member True(condition: bool, ?message: string) : unit =
        if not condition then
            failwithf "Assert.True failed: %s" (defaultArg message "")

    static member False(condition: bool, ?message: string) : unit =
        if condition then
            failwithf "Assert.False failed: %s" (defaultArg message "")

    static member Contains(expectedSubstring: string, actualString: string) : unit =
        if not (actualString.Contains expectedSubstring) then
            failwithf "Assert.Contains failed: %A not in %A" expectedSubstring actualString

    static member Contains<'T>(collection: 'T seq, filter: 'T -> bool) : unit =
        if not (Seq.exists filter collection) then
            failwith "Assert.Contains failed: no item matched"

    static member DoesNotContain<'T>(expected: 'T, collection: 'T seq) : unit =
        if Seq.exists (Unchecked.equals expected) collection then
            failwithf "Assert.DoesNotContain failed: %A found" expected

    static member DoesNotContain<'T>(collection: 'T seq, filter: 'T -> bool) : unit =
        if Seq.exists filter collection then
            failwith "Assert.DoesNotContain failed: an item matched"

    static member Empty<'T>(collection: 'T seq) : unit =
        if not (Seq.isEmpty collection) then
            failwithf "Assert.Empty failed: %A" (List.ofSeq collection)

    static member Single<'T>(collection: 'T seq) : 'T =
        match List.ofSeq collection with
        | [ only ] -> only
        | many -> failwithf "Assert.Single failed: %d items" many.Length

    static member InRange<'T when 'T: comparison>(actual: 'T, low: 'T, high: 'T) : unit =
        if actual < low || actual > high then
            failwithf "Assert.InRange failed: %A not in [%A, %A]" actual low high

    // The exception asked for passes, as in xunit, and so does one derived
    // from it, since Fable cannot name an exception's own type
    static member inline Throws<'T when 'T :> exn>(testCode: Action) : 'T =
        match Record.Exception testCode with
        | :? 'T as error -> error
        | null -> failwith "Assert.Throws failed: nothing was thrown"
        | error -> failwithf "Assert.Throws failed: the wrong exception was thrown: %s" error.Message

and Record =
    static member Exception(testCode: Action) : exn =
        try
            testCode.Invoke()
            null
        with error ->
            error

namespace FsCheck

open System

// A generator draws a value of the given size from a Random
type Gen<'T> = Gen of (int -> Random -> 'T)

type Arbitrary<'T> = { Generator: Gen<'T> }

// A property either holds or fails, with the labels of the parts that failed
// and why
type Outcome =
    | Holds
    | Fails of Labels: string list * Reason: string

type Property = Property of (int -> Random -> Outcome)

// From a fixed seed, so a run tries the same values every time
type Config = {
    MaxTest: int
    Seed: int
} with

    static member QuickThrowOnFailure = { MaxTest = 100; Seed = 7 }
    member config.WithMaxTest(maxTest: int) = { config with MaxTest = maxTest }

module Testable =
    // What a property's body returned: a bool, a Property, or unit when it
    // asserts as it goes
    let outcome (testable: obj) (size: int) (random: Random) : Outcome =
        match testable with
        | :? bool as holds -> if holds then Holds else Fails([], "false")
        | :? Property as property ->
            let (Property run) = property
            run size random
        | null -> Holds
        | other -> failwithf "not a testable: %A" other

type Check =
    // Tries the property at sizes growing from 1 to 100, as FsCheck does
    static member One(config: Config, testable: 'Testable) : unit =
        let random = Random config.Seed

        for test in 0 .. config.MaxTest - 1 do
            let size = 1 + 99 * test / max 1 (config.MaxTest - 1)

            match Testable.outcome (box testable) size random with
            | Holds -> ()
            | Fails(labels, reason) ->
                let labels = labels |> List.map (sprintf "\nLabel: %s") |> String.concat ""
                failwithf
                    "Falsifiable after %d tests (seed %d, size %d)%s\n%s"
                    (test + 1)
                    config.Seed
                    size
                    labels
                    reason

    static member QuickThrowOnFailure(testable: 'Testable) : unit =
        Check.One(Config.QuickThrowOnFailure, testable)

namespace FsCheck.FSharp

open FsCheck

module Gen =
    let internal run (Gen generate) size random = generate size random

    let constant (value: 'T) : Gen<'T> = Gen(fun _ _ -> value)

    let map (f: 'T -> 'U) (gen: Gen<'T>) : Gen<'U> =
        Gen(fun size random -> f (run gen size random))

    let map2 f (a: Gen<'A>) (b: Gen<'B>) : Gen<'C> =
        Gen(fun size random ->
            let a = run a size random
            f a (run b size random)
        )

    let map3 f (a: Gen<'A>) (b: Gen<'B>) (c: Gen<'C>) : Gen<'D> =
        Gen(fun size random ->
            let a = run a size random
            let b = run b size random
            f a b (run c size random)
        )

    let bind (f: 'T -> Gen<'U>) (gen: Gen<'T>) : Gen<'U> =
        Gen(fun size random -> run (f (run gen size random)) size random)

    let zip a b = map2 (fun a b -> a, b) a b
    let zip3 a b c = map3 (fun a b c -> a, b, c) a b c

    let choose (low: int, high: int) : Gen<int> =
        Gen(fun _ random -> random.Next(low, high + 1))

    let elements (items: 'T seq) : Gen<'T> =
        let items = Array.ofSeq items
        Gen(fun _ random -> items[random.Next items.Length])

    let oneof (gens: Gen<'T> seq) : Gen<'T> =
        let gens = Array.ofSeq gens
        Gen(fun size random -> run gens[random.Next gens.Length] size random)

    let frequency (weighted: (int * Gen<'T>) seq) : Gen<'T> =
        let weighted = List.ofSeq weighted

        let rec pick roll =
            function
            | [ _, gen ] -> gen
            | (weight, gen) :: rest -> if roll < weight then gen else pick (roll - weight) rest
            | [] -> failwith "Gen.frequency of nothing"

        Gen(fun size random -> run (pick (random.Next(List.sumBy fst weighted)) weighted) size random)

    let listOfLength (count: int) (gen: Gen<'T>) : Gen<'T list> =
        Gen(fun size random -> List.init count (fun _ -> run gen size random))

    // Lists as long as the size at most, the second never empty
    let listOf (gen: Gen<'T>) : Gen<'T list> =
        Gen(fun size random -> run (listOfLength (random.Next(0, size + 1)) gen) size random)

    let nonEmptyListOf (gen: Gen<'T>) : Gen<'T list> =
        Gen(fun size random -> run (listOfLength (random.Next(1, max 1 size + 1)) gen) size random)

    let resize (size: int) (gen: Gen<'T>) : Gen<'T> =
        Gen(fun _ random -> run gen size random)

type GenBuilder() =
    member _.Return(value: 'T) = Gen.constant value
    member _.ReturnFrom(gen: Gen<'T>) = gen
    member _.Bind(gen: Gen<'T>, f: 'T -> Gen<'U>) = Gen.bind f gen
    member _.Delay(f: unit -> Gen<'T>) =
        Gen(fun size random -> Gen.run (f ()) size random)

[<AutoOpen>]
module GenBuilder =
    let gen = GenBuilder()

module Arb =
    let fromGen (gen: Gen<'T>) : Arbitrary<'T> = { Generator = gen }

module Prop =
    let ofTestable (testable: 'Testable) : Property =
        Property(fun size random -> Testable.outcome (box testable) size random)

    // A body that raises fails, and a failure shows the value it was tried on
    let forAll (arbitrary: Arbitrary<'Value>) (body: 'Value -> 'Testable) : Property =
        Property(fun size random ->
            let value = Gen.run arbitrary.Generator size random

            let outcome =
                try
                    Testable.outcome (box (body value)) size random
                with error ->
                    Fails([], $"threw {error.Message}")

            match outcome with
            | Holds -> Holds
            | Fails(labels, reason) -> Fails(labels, $"{reason}\nOriginal: %A{value}")
        )

    let label (name: string) (testable: 'Testable) : Property =
        Property(fun size random ->
            match Testable.outcome (box testable) size random with
            | Holds -> Holds
            | Fails(labels, reason) -> Fails(name :: labels, reason)
        )

[<AutoOpen>]
module PropOperators =
    let (.&.) (left: 'Left) (right: 'Right) : Property =
        Property(fun size random ->
            match Testable.outcome (box left) size random with
            | Holds -> Testable.outcome (box right) size random
            | failed -> failed
        )

namespace System

type Convert =
    static member ToHexString(data: byte array) : string =
        data |> Array.map (sprintf "%02X") |> String.concat ""

namespace System.Security.Cryptography

type SHA256 =
    static member HashData(data: byte array) : byte array =
        // The first 32 bits of the fractional parts of the square roots of the
        // first 8 primes, and of the cube roots of the first 64
        let primes =
            [| 2..311 |]
            |> Array.where (fun n -> Seq.forall (fun d -> n % d <> 0) (seq { 2 .. n - 1 }))

        let fraction (root: float -> float) (prime: int) =
            uint32 ((root (float prime) % 1.0) * 4294967296.0)

        let h = primes[..7] |> Array.map (fraction sqrt)
        let k = primes |> Array.map (fraction (fun x -> x ** (1.0 / 3.0)))

        // The message, a 1 bit, zeros, and its length in bits, to a multiple of
        // 64 bytes
        let padded = Array.zeroCreate<byte>((data.Length + 9 + 63) / 64 * 64)
        Array.blit data 0 padded 0 data.Length
        padded[data.Length] <- 0x80uy
        let bits = uint64 data.Length * 8UL

        for i in 0..7 do
            padded[padded.Length - 1 - i] <- byte (bits >>> (8 * i))

        let rotr (x: uint32) (n: int) = (x >>> n) ||| (x <<< (32 - n))
        let w = Array.zeroCreate<uint32> 64

        for chunk in 0..64 .. padded.Length - 1 do
            for t in 0..15 do
                let at i = uint32 padded[chunk + 4 * t + i]
                w[t] <- (at 0 <<< 24) ||| (at 1 <<< 16) ||| (at 2 <<< 8) ||| at 3

            for t in 16..63 do
                let s0 = rotr w[t - 15] 7 ^^^ rotr w[t - 15] 18 ^^^ (w[t - 15] >>> 3)
                let s1 = rotr w[t - 2] 17 ^^^ rotr w[t - 2] 19 ^^^ (w[t - 2] >>> 10)
                w[t] <- w[t - 16] + s0 + w[t - 7] + s1

            let v = Array.copy h

            for t in 0..63 do
                let s1 = rotr v[4] 6 ^^^ rotr v[4] 11 ^^^ rotr v[4] 25
                let choice = (v[4] &&& v[5]) ^^^ (~~~v[4] &&& v[6])
                let first = v[7] + s1 + choice + k[t] + w[t]
                let s0 = rotr v[0] 2 ^^^ rotr v[0] 13 ^^^ rotr v[0] 22
                let majority = (v[0] &&& v[1]) ^^^ (v[0] &&& v[2]) ^^^ (v[1] &&& v[2])

                for i in 7..-1..1 do
                    v[i] <- v[i - 1]

                v[4] <- v[4] + first
                v[0] <- first + s0 + majority

            for i in 0..7 do
                h[i] <- h[i] + v[i]

        h
        |> Array.collect (fun x -> [| byte (x >>> 24); byte (x >>> 16); byte (x >>> 8); byte x |])

namespace global

[<AutoOpen>]
module FableAsync =
    open Fable.Core

    // Runs a computation that only ever waits on the trampoline, draining what
    // it would hand to setTimeout in a loop instead
    [<Literal>]
    let private Drain =
        """(() => {
  const queue = [];
  let done = false, ok = false, value, error;
  const trampoline = { callCount: 0, completed: false,
    incrementAndCheck() { return this.callCount++ > 2000; },
    hijack(f) { this.callCount = 0; queue.push(f); } };
  const finish = (k) => (x) => { trampoline.completed = true; done = true; k(x); };
  $0({ onSuccess: finish(x => { ok = true; value = x; }), onError: finish(e => { error = e; }),
       onCancel: finish(e => { error = e; }), cancelToken: $1, trampoline });
  while (!done && queue.length > 0) queue.shift()();
  if (!done) throw new Error("Async.RunSynchronously: the computation waits on a timer or a promise");
  if (!ok) throw error;
  return value;
})()"""

    module Async =
        [<Emit(Drain)>]
        let private drain (_computation: Async<'T>) (_token: System.Threading.CancellationToken) : 'T = jsNative

        let RunSynchronously (computation: Async<'T>) : 'T =
            drain computation (new System.Threading.CancellationTokenSource()).Token

module Runner =
    // The tests that fail in JavaScript, in every case they have, at an
    // Assert.Throws: Fable compiles invalidArg and invalidOp to a plain
    // Exception, so where the library refuses a call with them, an
    // Assert.Throws<ArgumentException> or
    // Assert.Throws<InvalidOperationException> is shown the wrong exception
    let private failsInJavaScript =
        set [
            "TimelineTests: seating the same name twice is refused"
            "TimelineTests: seating a player who takes sides four deep is refused, and three deep plays"
            "TimelineTests: continuing from a state with a repeated name is refused"
            "TimelineTests: continuing with a seating that is not the table is refused"
            "TimelineTests: an aim at a player who is not a candidate is refused"
            "InferenceTests: a study refuses an empty grid, a strategy or policy decided externally, or a policy that takes sides"
            "InferenceTests: a study continues only at an epsilon above 0 and at most 1"
            "AimTests: aim evidence folded one aim at a time is the evidence scored at once"
            "AimTests: carrying evidence into the next game moves only the sides, keeping each with the given probability"
        ]

    // Runs every case of every test, a Theory's once per InlineData, and
    // raises at the end if any failed other than as listed, or any listed test
    // passed or was not run
    let run (cases: (string * string * (unit -> unit)) list) : unit =
        let mutable passed = 0
        let mutable listed = 0
        let mutable wrong = 0
        let indented (text: string) = text.Replace("\n", "\n    ")

        for test, data, case in cases do
            let name = if data = "" then test else $"{test} ({data})"

            let error =
                try
                    case ()
                    None
                with error ->
                    Some error.Message

            match error, failsInJavaScript.Contains test with
            | None, false ->
                passed <- passed + 1
                printfn $"pass {name}"
            | Some message, true when message.StartsWith "Assert.Throws failed: the wrong exception was thrown" ->
                listed <- listed + 1
                printfn $"fail {name}, as listed"
            | None, true ->
                wrong <- wrong + 1
                printfn $"FAIL {name}\n    passed, but is listed as failing in JavaScript"
            | Some message, _ ->
                wrong <- wrong + 1
                printfn $"FAIL {name}\n    {indented message}"

        let tests = cases |> List.map (fun (test, _, _) -> test) |> set

        for test in failsInJavaScript - tests do
            wrong <- wrong + 1
            printfn $"FAIL {test}\n    is listed as failing in JavaScript, but was not run"

        printfn $"{passed} passed, {listed} failed as listed, {wrong} wrong"

        if wrong > 0 then
            failwith $"{wrong} wrong in JavaScript"
