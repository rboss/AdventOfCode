let input =
    "./day10_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let charToInt = System.Globalization.CharUnicodeInfo.GetDigitValue

let adjacent (y, x) =
    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]

let topoMap =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), charToInt v))
    |> Array.collect id
    |> Map.ofArray

let trailheads = topoMap |> Map.filter (fun key v -> v = 0) |> Map.toList

let trailscore topoMap startPos =
    let findPos pos =
        Map.tryFind pos topoMap |> Option.map (fun v -> pos, v)

    let rec nextStep steps acc =
        match steps with
        | [] -> acc
        | (pos, height) :: rest when height = 9 -> nextStep rest (pos :: acc)
        | (pos, height) :: rest ->
            let adjacentSteps =
                pos
                |> adjacent
                |> List.choose findPos
                |> List.filter (fun (_, v) -> v = height + 1)

            nextStep (adjacentSteps @ rest) acc

    nextStep [ startPos ] []

trailheads
|> List.map (trailscore topoMap)
|> List.map (List.countBy id >> List.length)
|> List.sum
|> printfn "Day 10a : %i"

trailheads
|> List.map (trailscore topoMap)
|> List.map (List.length)
|> List.sum
|> printfn "Day 10b : %i"
