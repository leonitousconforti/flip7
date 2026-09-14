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

// At a freeze ask the engine is holding the freeze card the chooser drew: it
// is in neither the deck nor a hand, and giving it out puts it in the
// target's, which is what the rollout has to reproduce
let private freezeDecks =
    Deck.Decrement undealt (ActionCard Card.Freeze), Deck.Empty

// A deal3 card has already gone to the discards by the time the ask is made
let private deal3Decks =
    Deck.Decrement undealt (ActionCard Card.Deal3), Deck.Increment Deck.Empty (ActionCard Card.Deal3)

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
let ``Aiming draws none of the game's randomness`` () =
    let aim (sage: Sage) (ask: Strategy.Ask) (decks: Deck * Deck) (random: System.Random) =
        sage.Aim random (Targeting.ChoosesExternally "Adaptive") ask table[0] table [] decks
        |> Async.RunSynchronously
        |> ignore

    // Both the rollout paths and the spiteful ones they fall back to
    Assert.True(inStep (aim watching Strategy.Ask.WhoToFreeze freezeDecks), "freeze by rollout")
    Assert.True(inStep (aim watching Strategy.Ask.WhoReceivesDeal3 deal3Decks), "deal3 by rollout")

    Assert.True(inStep (aim watching Strategy.Ask.WhoReceivesSecondChance tableDecks), "second chance, spiteful")

    Assert.True(inStep (aim (Sage([], rollouts = 20)) Strategy.Ask.WhoToFreeze freezeDecks), "freeze without a scan")

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
