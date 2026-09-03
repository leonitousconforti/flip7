module EventTests

open Xunit
open Flip7

[<Fact>]
let ``Serialize and Deserialize round-trip every event`` () =
    [
        Drew("Alice", ValueCard Card.Seven)
        Stood "Alice"
        Busted("Alice", ValueCard Card.Twelve)
        Froze("Alice", "Bob")
        SecondChancePassed("Alice", "Bob")
        SecondChanceDiscarded "Alice"
        Dealt3("Alice", "Bob", [ ValueCard Card.One; ModifierCard Card.Double; ActionCard Card.SecondChance ])
        Dealt3("Alice", "Alice", [])
        Flip7Achieved "Alice"
        RoundEnded(Map.ofList [ "Alice", 45u; "Bob", 0u ])
        RoundEnded Map.empty
        GameEnded "Alice"
    ]
    |> List.iter (fun event -> Assert.Equal(event, event |> Event.Serialize |> Event.Deserialize))

[<Fact>]
let ``Deserialize raises FormatException for invalid lines`` () =
    Assert.Throws<System.FormatException>(fun () -> Event.Deserialize [ "Bogus" ] |> ignore)
    |> ignore

    Assert.Throws<System.FormatException>(fun () -> Event.Deserialize [ "Drew"; "Alice" ] |> ignore)
    |> ignore
