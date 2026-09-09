module PropertyTests

open System
open FSharp.Control
open FsCheck
open FsCheck.FSharp
open Xunit

open Flip7

// Property-based tests: FsCheck generates hundreds of cases per fact and
// shrinks any failure to a minimal counterexample. Generators stay inside the
// domain's own invariants: decks are sub-decks of the full 94 cards, strategy
// numbers use the tame magnitudes real games use (keeping floats finite, so
// round-trips compare equal), and names draw from an alphabet that includes
// the separators the serialization formats have to survive - spaces and
// colons.

let private genCard: Gen<Card> =
    Deck.Empty |> Map.toList |> List.map fst |> Gen.elements

let private genHand: Gen<Hand> = Gen.listOf genCard

let private genSeed: Gen<int> = Gen.choose (0, 1000000)

let private genName: Gen<string> =
    [ 'a' .. 'z' ] @ [ ' '; ':'; '+' ]
    |> Gen.elements
    |> Gen.nonEmptyListOf
    |> Gen.map (List.toArray >> String)

let private genUint: Gen<uint> = Gen.choose (0, 300) |> Gen.map uint

let private genProbability: Gen<float> =
    Gen.choose (0, 100) |> Gen.map (fun percent -> float percent / 100.0)

// Every strategy DecideWith can evaluate: Custom is decided by an injected
// decider and raises there, so it stays out of engine draws
let private genEngineStrategy: Gen<Strategy> =
    Gen.oneof [
        Gen.constant AlwaysHits
        Gen.constant AlwaysStands
        Gen.map RandomWithProbability genProbability
        Gen.map HitUntilScore genUint
        Gen.map HitUntilNumCards genUint
        Gen.map HitUntilBustProbability genProbability
        Gen.map HitUntilNaiveBustProbability genProbability
        Gen.map2 (fun threshold temperature -> SoftHitUntilScore(threshold, temperature)) genUint genProbability
        Gen.map HitUntilTotal genUint
        Gen.map HitUntilUniqueValues genUint
        Gen.map2 (fun score uniques -> ChasesFlip7(score, uniques)) genUint genUint
        Gen.map EmboldenedBySecondChance genUint
        Gen.map HitWhileBehindLeader genUint
        Gen.map StandsAfterTurn genUint
        Gen.constant MaximizesExpectedValue
    ]

let private genStrategy: Gen<Strategy> =
    Gen.frequency [ 15, genEngineStrategy; 1, Gen.map Custom genName ]

let private genDeck: Gen<Deck> = gen {
    let cards = Deck.Full |> Map.toList
    let! counts = Gen.listOfLength cards.Length (Gen.choose (0, 12))

    return
        List.zip cards counts
        |> List.map (fun ((card, full), count) -> card, uint (min count (int full)))
        |> Map.ofList
}

let private genEvent: Gen<Event> =
    Gen.oneof [
        Gen.map2 (fun name card -> Drew(name, card)) genName genCard
        Gen.map Stood genName
        Gen.map2 (fun name card -> Busted(name, card)) genName genCard
        Gen.map2 (fun source target -> Froze(source, target)) genName genName
        Gen.map2 (fun source target -> SecondChancePassed(source, target)) genName genName
        Gen.map SecondChanceDiscarded genName
        Gen.map3 (fun source target cards -> Dealt3(source, target, cards)) genName genName (Gen.listOf genCard)
        Gen.map Flip7Achieved genName
        Gen.map Edited genName
        Gen.map2 (fun name score -> name, score) genName genUint
        |> Gen.listOf
        |> Gen.map (Map.ofList >> RoundEnded)
    ]

// One to five uniquely named players on any engine strategies
let private genLineup: Gen<(string * Strategy) list> = gen {
    let! count = Gen.choose (1, 5)
    let! strategies = Gen.listOfLength count genEngineStrategy
    return List.zip (List.take count [ "Alice"; "Bob"; "Carol"; "Dave"; "Eve" ]) strategies
}

// Deals a hand card by card out of the given deck, keeping at most one
// SecondChance per hand and - unless busting is allowed - skipping duplicate
// value cards, so an active player can never start busted. Action cards can
// land in hands: the play editor really produces such splices.
let rec private dealHand (allowBust: bool) (remaining: int) (hand: Hand) (deck: Deck) : Gen<Hand * Deck> = gen {
    if remaining = 0 then
        return hand, deck
    else
        let candidates =
            deck
            |> Map.toList
            |> List.filter (fun (card, count) ->
                count > 0u
                && (card <> ActionCard Card.SecondChance
                    || not (List.contains (ActionCard Card.SecondChance) hand))
                && (allowBust
                    || match card with
                       | ValueCard _ -> not (List.contains card hand)
                       | _ -> true)
            )
            |> List.map fst

        match candidates with
        | [] -> return hand, deck
        | _ ->
            let! card = Gen.elements candidates
            return! dealHand allowBust (remaining - 1) (card :: hand) (Deck.Decrement deck card)
}

// A legal mid-round state for ContinueWith: hands dealt out of the full deck
// so the 94-card invariant holds by construction, no active player busted,
// what remains split between the deck and the discards, and a turn counted
// for anyone already holding cards
let private genSplice: Gen<uint * Map<string, uint> * Player list * Player list * (Deck * Deck)> = gen {
    let! activeCount = Gen.choose (1, 3)
    let! finishedCount = Gen.choose (0, 2)
    let count = activeCount + finishedCount
    let names = List.take count [ "Alice"; "Bob"; "Carol"; "Dave"; "Eve" ]

    let! strategies = Gen.listOfLength count genEngineStrategy
    let! firmScores = Gen.listOfLength count (Gen.map uint (Gen.choose (0, 190)))
    let! handSizes = Gen.listOfLength count (Gen.choose (0, 6))

    let rec dealHands index hands deck = gen {
        if index = count then
            return List.rev hands, deck
        else
            let! hand, deck' =
                dealHand (index >= activeCount) (List.item index handSizes) [] deck
            return! dealHands (index + 1) (hand :: hands) deck'
    }

    let! hands, undealt = dealHands 0 [] Deck.Full

    let! splits = Gen.listOfLength (Map.count undealt) (Gen.choose (0, 12))

    let deck =
        List.zip (Map.toList undealt) splits
        |> List.map (fun ((card, remaining), split) -> card, min (uint split) remaining)
        |> Map.ofList

    let discards =
        undealt |> Map.map (fun card remaining -> remaining - Map.find card deck)

    let players =
        names
        |> List.mapi (fun i name ->
            Player.Make(name, List.item i strategies, List.item i firmScores, List.item i hands)
        )

    let active = List.take activeCount players
    let finished = List.skip activeCount players

    let turnsTaken =
        active
        |> List.map (fun player -> player.Name, (if List.isEmpty player.Hand then 0u else 1u))
        |> Map.ofList

    let! round = Gen.map uint (Gen.choose (1, 10))
    return round, turnsTaken, active, finished, (deck, discards)
}

let private conservesEveryCard (timeline: Instant list) : bool =
    timeline
    |> List.forall (fun instant ->
        instant.Players
        |> List.map (fun player -> player.Hand)
        |> Simulation.Issues instant.Deck instant.Discards
        |> Seq.isEmpty
    )

let private endsAtTwoHundred (timeline: Instant list) : bool =
    let finalInstant = List.last timeline

    finalInstant.Event.IsRoundEnded
    && finalInstant.Players |> List.exists (fun player -> player.FirmScore >= 200u)

[<Fact>]
let ``a card round-trips through its string form`` () =
    Prop.forAll (Arb.fromGen genCard) (fun card -> Card.Parse(string card) = card)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``a strategy round-trips through its string form`` () =
    Prop.forAll (Arb.fromGen genStrategy) (fun strategy -> Strategy.Parse(string strategy) = strategy)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``a hand round-trips through serialization`` () =
    Prop.forAll (Arb.fromGen genHand) (fun hand -> Hand.Deserialize(Hand.Serialize hand) = hand)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``a deck round-trips through serialization`` () =
    Prop.forAll (Arb.fromGen genDeck) (fun deck -> Deck.Deserialize(Deck.Serialize deck) = deck)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``an event round-trips through serialization`` () =
    Prop.forAll (Arb.fromGen genEvent) (fun event -> Event.Deserialize(Event.Serialize event) = event)
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``Reduce conserves every card`` () =
    Prop.forAll
        (Arb.fromGen genHand)
        (fun hand ->
            let _, reduced, removed = Hand.Reduce hand
            List.sort (reduced @ removed) = List.sort hand
        )
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``Reduce agrees with IsBust`` () =
    Prop.forAll
        (Arb.fromGen genHand)
        (fun hand ->
            let isBust, _, _ = Hand.Reduce hand
            isBust = Hand.IsBust hand
        )
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``a hand that survives Reduce has no duplicate value cards`` () =
    Prop.forAll
        (Arb.fromGen genHand)
        (fun hand ->
            let isBust, reduced, _ = Hand.Reduce hand
            let values = reduced |> List.filter (fun card -> card.IsValueCard)
            isBust || List.distinct values = values
        )
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``drawing conserves every card across the deck and discards`` () =
    Prop.forAll
        (Arb.fromGen (Gen.map2 (fun seed deck -> seed, deck) genSeed genDeck))
        (fun (seed, deck) ->
            // The discards complete the deck to the full 94 cards, so the pair
            // can never be exhausted mid-draw
            let discards = Deck.Full |> Map.map (fun card count -> count - Map.find card deck)

            let (deck', discards'), drawn = Deck.Draw1With (Random seed) (deck, discards)

            Deck.Full
            |> Map.forall (fun card _ ->
                let before = Map.find card deck + Map.find card discards

                let after =
                    Map.find card deck'
                    + Map.find card discards'
                    + (if card = drawn then 1u else 0u)

                before = after
            )
        )
    |> Check.QuickThrowOnFailure

// A whole game per case, so these run fewer, heavier cases than the default
[<Fact>]
let ``every simulated game conserves all 94 cards and ends at 200`` () =
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.map2 (fun seed lineup -> seed, lineup) genSeed genLineup))
            (fun (seed, lineup) ->
                let timeline =
                    Timeline.SimulateWith (Random seed) lineup
                    |> AsyncSeq.toListAsync
                    |> Async.RunSynchronously

                conservesEveryCard timeline && endsAtTwoHundred timeline
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

[<Fact>]
let ``continuing from any legal mid-round state conserves all 94 cards and finishes`` () =
    let property =
        Prop.forAll
            (Arb.fromGen (Gen.map2 (fun seed splice -> seed, splice) genSeed genSplice))
            (fun (seed, (round, turnsTaken, active, finished, decks)) ->
                let timeline =
                    Timeline.ContinueWith
                        (Random seed)
                        (Strategy.DecideWith(Random seed))
                        round
                        turnsTaken
                        active
                        finished
                        decks
                    |> AsyncSeq.toListAsync
                    |> Async.RunSynchronously

                conservesEveryCard timeline && endsAtTwoHundred timeline
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 20, property)

[<Fact>]
let ``the same seed always produces the same timeline`` () =
    let property =
        Prop.forAll
            (Arb.fromGen genSeed)
            (fun seed ->
                let simulate () =
                    Timeline.SimulateWith (Random seed) [ "Alice", Strategy.Random; "Bob", HitUntilScore 25u ]
                    |> AsyncSeq.toListAsync
                    |> Async.RunSynchronously

                simulate () = simulate ()
            )

    Check.One(Config.QuickThrowOnFailure.WithMaxTest 10, property)
