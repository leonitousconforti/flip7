[<AutoOpen>]
module public Sparkline

let inline public normalizeDistribution (series: (^a * float) list) : (^a * float) list =
    let maxProb = series |> List.map snd |> List.max
    series |> List.map (fun (label, prob) -> label, prob / maxProb)

let inline public sparkline< ^a when ^a: equality>
    (rows: int)
    (cursor: ^a option)
    (series: (^a * float) list)
    : string list =
    let blockChar f =
        if f >= 7.0 / 8.0 then "█"
        elif f >= 6.0 / 8.0 then "▇"
        elif f >= 5.0 / 8.0 then "▆"
        elif f >= 4.0 / 8.0 then "▅"
        elif f >= 3.0 / 8.0 then "▄"
        elif f >= 2.0 / 8.0 then "▃"
        elif f >= 1.0 / 8.0 then "▂"
        elif f >= 0.0 / 8.0 then "▁"
        else " "

    let barCell (f: float) (r: int) =
        let scaled = f * float rows
        if scaled >= float r + 1.0 then "█"
        elif scaled > float r then blockChar (scaled - float r)
        else " "

    [ rows - 1 .. -1 .. 0 ]
    |> List.map (fun row ->
        series
        |> List.map (fun (label, value) ->
            let cell = barCell value row
            if cursor = Some label then
                styled [ Ansi.BrightGreen ] cell
            else
                cell
        )
        |> String.concat ""
    )
