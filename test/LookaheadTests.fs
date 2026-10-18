module LookaheadTests

open System
open System.Threading
open Xunit
open Flip7
open Support

// What hitting is worth over standing for a hand the searcher was given, and
// what the hand a draw leaves is worth, each awaited on the test's thread
let private gainOf (searcher: Lookahead) (hand: Hand) : Result<float, LookaheadError> =
    searcher.GainFromHitting hand |> Async.RunSynchronously

let private worthAfter (searcher: Lookahead) (hand: Hand) (card: Card) : Result<float, LookaheadError> =
    searcher.WorthAfter(hand, card) |> Async.RunSynchronously

let private priced (result: Result<float, LookaheadError>) : float =
    match result with
    | Ok worth -> worth
    | Error error -> failwith $"%A{error}"

// ---- what a hand gains from hitting ----

// The search is exact, so it is checked against arithmetic. Comparing to ten
// places only absorbs the different orders the two sides sum their terms in
[<Fact>]
let ``one card of sight is exactly what expected value already sees`` () =
    let hands = [
        // Every card is new to an empty hand
        []
        // A single value, which only its own copies bust
        values [ Card.Five ]
        // High values, which the most plentiful cards bust
        values [ Card.Twelve; Card.Eleven ]
        values [ Card.Twelve; Card.Eleven; Card.Ten ]
        // A second chance, which covers a dup
        [ ActionCard Card.SecondChance; ValueCard Card.Seven ]
        // The x2, which doubles the values drawn onto it
        [ ModifierCard Card.Double; ValueCard Card.Nine ]
        // Six values, so a Seven flips seven and earns the bonus
        values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
    ]

    use searcher = new Lookahead(1, hands)

    for hand in hands do
        let deck = hand |> List.fold Deck.Decrement Deck.Full
        Assert.Equal(Simulation.expectedValueOfHit deck Deck.Empty hand, gainOf searcher hand |> priced, 10)

[<Fact>]
let ``one card of sight from a mid-game deck is what expected value sees in the cards left to draw`` () =
    let hand = values [ Card.Seven; Card.Ten ]

    // A live drawable deck: the hand is out of it, and so is a spread of
    // cards held by other players or discarded
    let elsewhere = [
        ValueCard Card.Twelve
        ValueCard Card.Twelve
        ValueCard Card.Eleven
        ValueCard Card.Nine
        ValueCard Card.Five
        ValueCard Card.Two
        ActionCard Card.Freeze
        ActionCard Card.SecondChance
        ModifierCard Card.Plus10
    ]

    // A searcher prices a hand against its deck missing that hand, so it is
    // dealt the drawable cards and the hand together
    let drawable = List.fold Deck.Decrement Deck.Full (hand @ elsewhere)
    let dealtFrom = hand |> List.fold Deck.Increment drawable

    use searcher = new Lookahead(1, [ hand ], deck = dealtFrom)

    Assert.Equal(Simulation.expectedValueOfHit drawable Deck.Empty hand, gainOf searcher hand |> priced, 10)

// A plain expectimax over the deck and the hand themselves, with none of the
// search's packed states or generations: what a hand is worth held with some
// sight left, what playing on is worth, the average over every card that could
// come, and what each draw leaves, a bust worth nothing
let rec private worthHeld (deck: Deck) (hand: Hand) (sight: int) : float =
    let banked = float (Hand.Score hand)

    if sight = 0 || Hand.HasFlip7Bonus hand then
        banked
    else
        max banked (worthHitting deck hand sight)

and private worthHitting (deck: Deck) (hand: Hand) (sight: int) : float =
    let total = float (Deck.Count deck)

    deck
    |> Map.toList
    |> List.filter (fun (_, count) -> count > 0u)
    |> List.sumBy (fun (card, count) -> float count / total * worthDrawing deck hand card (sight - 1))

// A second chance drawn onto a hand that holds one is passed on or discarded,
// as the engine does, so the hand stays as it was while the deck loses the card
and private worthDrawing (deck: Deck) (hand: Hand) (card: Card) (sight: int) : float =
    let isBust, reduced, _ = Hand.Reduce(card :: hand)

    let kept =
        if card = ActionCard Card.SecondChance && List.contains card hand then
            hand
        else
            reduced

    if isBust then
        0.0
    else
        worthHeld (Deck.Decrement deck card) kept sight

[<Fact>]
let ``the search agrees with a plain expectimax on a small deck at every depth`` () =
    // Seven distinct values, so a hand can flip seven below the root; three
    // Twelves, so a hand holding one can draw two dups; and two second chances,
    // so the root holding one can draw the other, which it passes on, and then
    // both dups, the second of which busts it
    let dealtFrom =
        deckOf [
            yield! values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]
            yield! List.replicate 3 (ValueCard Card.Twelve)
            ModifierCard Card.Plus4
            ModifierCard Card.Double
            yield! List.replicate 2 (ActionCard Card.SecondChance)
            ActionCard Card.Freeze
        ]

    let roots = [
        []
        values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five ]
        [ ActionCard Card.SecondChance; ValueCard Card.Twelve ]
        [ ModifierCard Card.Double; ValueCard Card.Seven ]
    ]

    // Both sides sum the same terms in different orders, so the last places
    // may differ. Each root's gain is followed by what each card its deck
    // still holds leaves, and every row is labelled with its depth, root and
    // card, so a failure names them
    let row (depth: int) (root: Hand) (card: string) (worth: float) : int * string * string * float =
        depth, (root |> List.map string |> String.concat " "), card, Math.Round(worth, 10)

    let drawable (root: Hand) : Card list =
        root
        |> List.fold Deck.Decrement dealtFrom
        |> Map.toList
        |> List.filter (fun (_, count) -> count > 0u)
        |> List.map fst

    let expected = [|
        for depth in 1..4 do
            for root in roots do
                let deck = root |> List.fold Deck.Decrement dealtFrom
                row depth root "hit" (worthHitting deck root depth - float (Hand.Score root))

                for card in drawable root do
                    row depth root (string card) (worthDrawing deck root card (depth - 1))
    |]

    let actual = [|
        for depth in 1..4 do
            use searcher = new Lookahead(depth, roots, deck = dealtFrom)

            for root in roots do
                row depth root "hit" (gainOf searcher root |> priced)

                for card in drawable root do
                    row depth root (string card) (worthAfter searcher root card |> priced)
    |]

    Assert.Equal<(int * string * string * float)[]>(expected, actual)

[<Fact>]
let ``seeing further never lowers a hand's gain, and four cards of sight beat one`` () =
    let hand = values [ Card.Five ]

    let byDepth =
        [ 1..4 ]
        |> List.map (fun depth ->
            use searcher = new Lookahead(depth, [ hand ])
            gainOf searcher hand |> priced
        )

    // Seeing further can only find more worth in a hand that may be played on,
    // because the hand can always decline what it finds
    Assert.Equal<float list>(byDepth |> List.sort, byDepth)
    Assert.True(List.last byDepth > List.head byDepth, "four cards of sight should find more worth than one")

[<Fact>]
let ``a hand with no draw worth taking: one that has flipped seven gains nothing, and one every draw busts loses its whole score``
    ()
    =
    // Each searcher sees three cards ahead, more than either hand could use.
    // Seven distinct values end the round, so there is nothing left to play for
    let flipped =
        values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven ]

    use searcher = new Lookahead(3, [ flipped ])
    Assert.Equal(Ok 0.0, gainOf searcher flipped)

    // Dealt from a deck of one One and two Twos, this hand leaves a single
    // drawable card, and it always busts: hitting throws away the 1 + 2 the
    // hand would bank
    let doomed = values [ Card.One; Card.Two ]
    let dealtFrom = deckOf (values [ Card.One; Card.Two; Card.Two ])

    use searcher = new Lookahead(3, [ doomed ], deck = dealtFrom)
    Assert.Equal(Ok(-(1.0 + 2.0)), gainOf searcher doomed)

// ---- what a draw leaves ----

[<Fact>]
let ``a draw that busts is worth nothing, and a card with no copies left is refused`` () =
    let hand = values [ Card.One; Card.Two ]
    use searcher = new Lookahead(2, [ hand ])

    // The deck still holds a second Two, and drawing it busts a hand with no
    // second chance; the only One is already in the hand, so it cannot come
    Assert.Equal(Ok 0.0, worthAfter searcher hand (ValueCard Card.Two))
    Assert.Equal(Error(NoCopiesLeft(ValueCard Card.One)), worthAfter searcher hand (ValueCard Card.One))

[<Fact>]
let ``a draw that completes seven numbers is worth its bonus played on`` () =
    // With two cards of sight the hand the Seven leaves still has one to
    // spend, but seven distinct values end the round, so it banks what it
    // holds and the search never prices a draw from it
    let hand = values [ Card.One; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six ]
    use searcher = new Lookahead(2, [ hand ])

    // 1 + 2 + 3 + 4 + 5 + 6 = 21 already held, the Seven's 7, and the 15
    // point bonus for flipping seven
    Assert.Equal(Ok(21.0 + 7.0 + 15.0), worthAfter searcher hand (ValueCard Card.Seven))

// ---- refusals ----

[<Fact>]
let ``a searcher cannot be made with no sight, a bust hand, an oversized deck, or a hand its deck could not deal`` () =
    // Each refusal names the argument at fault, and a refused hand is shown
    let refused (argument: string) (culprit: string) (build: unit -> Lookahead) =
        let error = Assert.Throws<ArgumentException>(fun () -> build () |> ignore)
        Assert.Equal(argument, error.ParamName)
        Assert.Contains(culprit, error.Message)

    refused "depth" "at least one card of sight" (fun () -> new Lookahead(0, [ [] ]))
    refused "hands" "[5 5] is already bust" (fun () -> new Lookahead(2, [ values [ Card.Five; Card.Five ] ]))

    // The full deck holds a single Zero, so a Zero's count is packed into a
    // single bit, which a second Zero would overflow
    let twoZeros = Deck.Increment Deck.Full (ValueCard Card.Zero)
    refused "deck" "more copies of a card than the full deck" (fun () -> new Lookahead(2, [ [] ], deck = twoZeros))

    // Two x2s do not bust a hand, so this is the deck check refusing them: the
    // full deck holds only one
    let twoDoubles = [ ModifierCard Card.Double; ModifierCard Card.Double ]
    refused "hands" "[x2 x2] holds more copies than its deck" (fun () -> new Lookahead(2, [ twoDoubles ]))

[<Fact>]
let ``a question about a hand the searcher never priced, or one its deck could never have dealt, is answered with an error``
    ()
    =
    let hand = values [ Card.One; Card.Two ]
    use searcher = new Lookahead(2, [ hand ])

    Assert.True(gainOf searcher hand |> Result.isOk)

    // A hand the searcher never swept from is a question it never priced, and
    // no deck could ever have dealt two x2s
    let twoDoubles = [ ModifierCard Card.Double; ModifierCard Card.Double ]
    Assert.Equal(Error(UnpricedHand(values [ Card.Three ])), gainOf searcher (values [ Card.Three ]))
    Assert.Equal(Error(ImpossibleHand twoDoubles), gainOf searcher twoDoubles)

// ---- cancellation ----

[<Fact>]
let ``a searcher given an already cancelled token raises instead of answering`` () =
    let hand = values [ Card.Five ]

    // A token already cancelled means the table never prices at all, so the
    // outcome does not race the (fast) search
    use source = new CancellationTokenSource()
    source.Cancel()

    use searcher = new Lookahead(3, [ hand ], cancellation = source.Token)

    Assert.ThrowsAny<OperationCanceledException>(fun () -> gainOf searcher hand |> ignore)
    |> ignore

[<Fact>]
let ``disposing the searcher cancels the pricing it has not finished`` () =
    let hand = values [ Card.Five ]

    // The search reports after its first sweep and then holds there until the
    // searcher is disposed, so the dispose lands while the pricing is under
    // way, with two more sweeps and every pricing pass still to run, rather
    // than racing a search that may already be done. Each wait is bounded, so
    // a broken search fails rather than hangs
    let bound = TimeSpan.FromSeconds 10.0
    use swept = new ManualResetEventSlim()
    use disposed = new ManualResetEventSlim()

    let progress (_: string) =
        swept.Set()
        disposed.Wait(bound) |> ignore

    use searcher = new Lookahead(3, [ hand ], progress = progress)
    Assert.True(swept.Wait(bound), "the search should report its first sweep")
    (searcher :> IDisposable).Dispose()
    disposed.Set()

    Assert.ThrowsAny<OperationCanceledException>(fun () -> gainOf searcher hand |> ignore)
    |> ignore
