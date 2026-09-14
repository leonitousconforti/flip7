module SageTests

open FSharp.Control
open Xunit
open Flip7

let private simulate (seed: int) (players: list<string * Strategy * Targeting>) : Instant list =
    Timeline.SimulateWith (System.Random seed) players
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously

[<Fact>]
let ``Sage models players from past games`` () =
    let history =
        [ 1; 2; 3 ]
        |> List.map (fun seed ->
            simulate seed [
                "You", Strategy.HitUntilScore 24u, Targeting.ChoosesRandomly
                "Rival", Strategy.HitUntilNumCards 4u, Targeting.ChoosesRandomly
            ]
        )

    let sage = Sage history
    Assert.Equal(
        Strategy.HitUntilScore 24u,
        Inference.MostLikely (sage.ModelOf("You", Strategy.HitUntilScore 24u)).Value
    )

    Assert.Equal(
        Strategy.HitUntilNumCards 4u,
        Inference.MostLikely (sage.ModelOf("Rival", Strategy.HitUntilNumCards 4u)).Value
    )

[<Fact>]
let ``Sage folded over a game models it exactly like history it has studied`` () =
    let players = [
        "You", Strategy.HitUntilScore 24u, Targeting.ChoosesRandomly
        "Rival", Strategy.HitUntilNumCards 4u, Targeting.ChoosesRandomly
    ]

    let earlier = simulate 1 players
    let current = simulate 2 players

    let folded =
        current
        |> List.fold (fun sage instant -> Sage(instant, sage)) (Sage [ earlier ])

    let studied = Sage [ earlier; current ]

    Assert.Equal(studied.ModelOf("You", Strategy.HitUntilScore 24u), folded.ModelOf("You", Strategy.HitUntilScore 24u))

    Assert.Equal(
        studied.ModelOf("Rival", Strategy.HitUntilNumCards 4u),
        folded.ModelOf("Rival", Strategy.HitUntilNumCards 4u)
    )

let private teach (strategy: Strategy) (seeds: int list) : Instant list list =
    seeds
    |> List.map (fun seed ->
        simulate seed [
            "Rival", strategy, Targeting.ChoosesRandomly
            "Foil", Strategy.HitUntilNumCards 3u, Targeting.ChoosesRandomly
        ]
    )

[<Fact>]
let ``Sage stands when standing wins the game outright`` () =
    // Sage can bank 202 right now; the modeled rival races to 30 a round from
    // 150, so hitting only risks busting into a losing endgame
    let sage = Sage(teach (Strategy.HitUntilScore 30u) [ 1; 2 ])

    let me =
        Player.Make(
            "Sage",
            Strategy.Random,
            firmScore = 170u,
            hand = [ ValueCard Card.Twelve; ValueCard Card.Eleven; ValueCard Card.Nine ]
        )

    let rival = Player.Make("Rival", Strategy.HitUntilScore 30u, firmScore = 150u)
    let decks = me.Hand |> List.fold Deck.Decrement Deck.Full, Deck.Empty

    Assert.Equal(
        Strategy.Stand,
        sage.Decide (System.Random 1) (Strategy.Custom "Adaptive") 5u 3u me [ rival ] [] decks
        |> Async.RunSynchronously
    )

[<Fact>]
let ``Sage hits when its model shows that standing concedes the game`` () =
    // The modeled rival always stands, locking 202 this round; Sage at 195
    // loses every rollout by standing, while hitting can still pass 202
    let sage = Sage(teach Strategy.AlwaysStands [ 1; 2 ])

    let me =
        Player.Make(
            "Sage",
            Strategy.Random,
            firmScore = 165u,
            hand = [ ValueCard Card.Twelve; ValueCard Card.Eleven; ValueCard Card.Seven ]
        )

    let rival =
        Player.Make("Rival", Strategy.AlwaysStands, firmScore = 190u, hand = [ ValueCard Card.Twelve ])

    let decks = me.Hand @ rival.Hand |> List.fold Deck.Decrement Deck.Full, Deck.Empty

    Assert.Equal(
        Strategy.Hit,
        sage.Decide (System.Random 1) (Strategy.Custom "Adaptive") 5u 3u me [ rival ] [] decks
        |> Async.RunSynchronously
    )

// The property the benchmark rests on: Sage seeds its rollouts from the
// position, so however much it consults itself, the engine goes on dealing
// exactly the cards it would have dealt anyway. Two Randoms started alike stay
// in step across a decision, which is what lets one run be measured against
// another
let private inStep (consult: System.Random -> unit) : bool =
    let used = System.Random 7
    let untouched = System.Random 7
    consult used
    List.init 8 (fun _ -> used.Next()) = List.init 8 (fun _ -> untouched.Next())

let private table = [
    Player.Make("Sage", Strategy.Custom "Adaptive", firmScore = 100u, hand = [ ValueCard Card.Five ])
    Player.Make("Rival", Strategy.HitUntilScore 20u, firmScore = 150u, hand = [ ValueCard Card.Nine ])
    Player.Make("Foil", Strategy.HitUntilScore 18u, firmScore = 40u, hand = [ ValueCard Card.Two ])
]

let private undealt =
    table
    |> List.collect (fun player -> player.Hand)
    |> List.fold Deck.Decrement Deck.Full

// At a hit-or-stand ask every card is in the deck, the discards or a hand
let private tableDecks = undealt, Deck.Empty

// A Sage that has watched a game, so the rollout paths are the ones taken
let private watching =
    simulate 5 [
        "Sage", Strategy.HitUntilScore 20u, Targeting.ChoosesRandomly
        "Rival", Strategy.HitUntilScore 24u, Targeting.ChoosesRandomly
        "Foil", Strategy.HitUntilScore 18u, Targeting.ChoosesRandomly
    ]
    |> List.takeWhile (fun instant -> not instant.Event.IsRoundEnded)
    |> List.fold (fun sage instant -> Sage(instant, sage)) (Sage([], rollouts = 20))

[<Fact>]
let ``Deciding draws none of the game's randomness`` () =
    Assert.True(
        inStep (fun random ->
            watching.Decide random (Strategy.Custom "Adaptive") 3u 2u table[0] [ table[1]; table[2] ] [] tableDecks
            |> Async.RunSynchronously
            |> ignore
        )
    )

[<Fact>]
let ``The same position always gets the same answer`` () =
    let decide () =
        watching.Decide
            (System.Random 1)
            (Strategy.Custom "Adaptive")
            3u
            2u
            table[0]
            [ table[1]; table[2] ]
            []
            tableDecks
        |> Async.RunSynchronously

    Assert.Equal(decide (), decide ())

// The within-round search is exact, so it can be checked against arithmetic
// rather than against a tolerance
[<Fact>]
let ``One card of sight is exactly what expected value already sees`` () =
    let hands = [
        []
        [ ValueCard Card.Five ]
        [ ValueCard Card.Twelve; ValueCard Card.Eleven ]
        [ ValueCard Card.Twelve; ValueCard Card.Eleven; ValueCard Card.Ten ]
        [ ActionCard Card.SecondChance; ValueCard Card.Seven ]
    ]

    for hand in hands do
        let deck = hand |> List.fold Deck.Decrement Deck.Full

        Assert.Equal(Simulation.expectedValueOfHit deck Deck.Empty hand, Lookahead.GainFromHitting 1 deck hand, 10)

[<Fact>]
let ``Sight is worth something, and a hand already past seven is worth banking`` () =
    let hand = [ ValueCard Card.Five ]
    let deck = hand |> List.fold Deck.Decrement Deck.Full

    let byDepth =
        [ 1..4 ] |> List.map (fun depth -> Lookahead.GainFromHitting depth deck hand)

    // Seeing further can only find more worth in a hand that may be played on,
    // because the hand can always decline what it finds
    Assert.Equal<float list>(byDepth |> List.sort, byDepth)
    Assert.True(List.last byDepth > List.head byDepth, "sight should be worth something")

    // Seven distinct cards ends the round, so there is nothing left to play for
    let flipped =
        [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]
        |> List.map ValueCard

    Assert.Equal(0.0, Lookahead.GainFromHitting 3 (flipped |> List.fold Deck.Decrement Deck.Full) flipped)

[<Fact>]
let ``A hand that cannot survive a card is worth standing on`` () =
    // Every value card in the deck is one this hand already holds
    let hand = [ ValueCard Card.One; ValueCard Card.Two ]

    let deck =
        Deck.Empty
        |> fun d -> Deck.Increment d (ValueCard Card.One)
        |> fun d -> Deck.Increment d (ValueCard Card.Two)

    Assert.True(Lookahead.GainFromHitting 3 deck hand < 0.0, "certain bust is never worth it")
