module LookaheadTests

open Xunit
open Flip7

let private priced (result: Result<float, LookaheadError>) : float =
    match result with
    | Ok worth -> worth
    | Error error -> failwith $"%A{error}"

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

    use searcher = new Lookahead(1, hands)

    for hand in hands do
        let deck = hand |> List.fold Deck.Decrement Deck.Full
        let gain = searcher.GainFromHitting hand |> Async.RunSynchronously |> priced
        Assert.Equal(Simulation.expectedValueOfHit deck Deck.Empty hand, gain, 10)

[<Fact>]
let ``Sight is worth something, and a hand already past seven is worth banking`` () =
    let hand = [ ValueCard Card.Five ]

    let byDepth =
        [ 1..4 ]
        |> List.map (fun depth ->
            use searcher = new Lookahead(depth, [ hand ])
            searcher.GainFromHitting hand |> Async.RunSynchronously |> priced
        )

    // Seeing further can only find more worth in a hand that may be played on,
    // because the hand can always decline what it finds
    Assert.Equal<float list>(byDepth |> List.sort, byDepth)
    Assert.True(List.last byDepth > List.head byDepth, "sight should be worth something")

    // Seven distinct cards ends the round, so there is nothing left to play for
    let flipped =
        [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]
        |> List.map ValueCard

    use searcher = new Lookahead(3, [ flipped ])
    let gain = searcher.GainFromHitting flipped |> Async.RunSynchronously |> priced
    Assert.Equal(0.0, gain)

[<Fact>]
let ``The searcher only answers for the hands it was given`` () =
    let hand = [ ValueCard Card.One; ValueCard Card.Two ]
    use searcher = new Lookahead(2, [ hand ])

    Assert.True(searcher.GainFromHitting hand |> Async.RunSynchronously |> Result.isOk)

    // A hand the searcher never swept from is a question it never priced, and
    // no deck could ever have dealt two x2s
    Assert.Equal<Result<float, LookaheadError>>(
        Error(UnpricedHand [ ValueCard Card.Three ]),
        searcher.GainFromHitting [ ValueCard Card.Three ] |> Async.RunSynchronously
    )

    Assert.Equal<Result<float, LookaheadError>>(
        Error(ImpossibleHand [ ModifierCard Card.Double; ModifierCard Card.Double ]),
        searcher.GainFromHitting [ ModifierCard Card.Double; ModifierCard Card.Double ]
        |> Async.RunSynchronously
    )

[<Fact>]
let ``A draw that busts is worth nothing played on`` () =
    let hand = [ ValueCard Card.One; ValueCard Card.Two ]
    use searcher = new Lookahead(2, [ hand ])

    // The deck still holds a second Two, and drawing it busts a hand with no
    // second chance; the only One is already in the hand, so it cannot come
    Assert.Equal<Result<float, LookaheadError>>(
        Ok 0.0,
        searcher.WorthAfter(hand, ValueCard Card.Two) |> Async.RunSynchronously
    )

    Assert.Equal<Result<float, LookaheadError>>(
        Error(NoCopiesLeft(ValueCard Card.One)),
        searcher.WorthAfter(hand, ValueCard Card.One) |> Async.RunSynchronously
    )

[<Fact>]
let ``A hand that cannot survive a card is worth standing on`` () =
    // Dealt from a deck of one One and two Twos, this hand leaves a single
    // drawable card, and it always busts: hitting throws the hand away
    let hand = [ ValueCard Card.One; ValueCard Card.Two ]

    let dealtFrom =
        Deck.Empty
        |> fun deck -> Deck.Increment deck (ValueCard Card.One)
        |> fun deck -> Deck.Increment deck (ValueCard Card.Two)
        |> fun deck -> Deck.Increment deck (ValueCard Card.Two)

    use searcher = new Lookahead(3, [ hand ], deck = dealtFrom)

    Assert.Equal<Result<float, LookaheadError>>(Ok(-3.0), searcher.GainFromHitting hand |> Async.RunSynchronously)

[<Fact>]
let ``A cancelled searcher stops answering`` () =
    let hand = [ ValueCard Card.Five ]

    // A token already cancelled means the table never prices at all, so the
    // outcome does not race the (fast) search
    use source = new System.Threading.CancellationTokenSource()
    source.Cancel()

    use searcher = new Lookahead(3, [ hand ], cancellation = source.Token)

    Assert.ThrowsAny<System.OperationCanceledException>(fun () ->
        searcher.GainFromHitting hand |> Async.RunSynchronously |> ignore
    )
    |> ignore
