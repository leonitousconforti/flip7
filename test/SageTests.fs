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
    Assert.Equal(Strategy.HitUntilScore 24u, Inference.MostLikely (sage.ModelOf "You").Value)
    Assert.Equal(Strategy.HitUntilNumCards 4u, Inference.MostLikely (sage.ModelOf "Rival").Value)

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

    let rival = Player.Make("Rival", Strategy.Random, firmScore = 150u)
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
        Player.Make("Rival", Strategy.Random, firmScore = 190u, hand = [ ValueCard Card.Twelve ])

    let decks = me.Hand @ rival.Hand |> List.fold Deck.Decrement Deck.Full, Deck.Empty

    Assert.Equal(
        Strategy.Hit,
        sage.Decide (System.Random 1) (Strategy.Custom "Adaptive") 5u 3u me [ rival ] [] decks
        |> Async.RunSynchronously
    )
