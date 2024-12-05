let input = "./day4_input.txt" |> System.IO.File.ReadAllLines
let example = "./day4_example.txt" |> System.IO.File.ReadAllLines

let deltas =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let wordsMap =
    input
    |> Array.map Seq.toArray
    |> Array.mapi (fun x row -> row |> Array.mapi (fun y v -> (x, y), v))
    |> Array.collect id
    |> Map.ofArray

let addDelta (y, x) (dY, dX) = y + dY, x + dX

let rec findWord posMap delta word pos =
    let tryFind pos = Map.tryFind pos posMap

    let rec matchNextChar delta charList pos =
        match charList with
        | [] -> true
        | head :: tail ->
            match tryFind pos with
            | Some x when x = head ->
                let nextPos = addDelta pos delta
                matchNextChar delta tail nextPos
            | _ -> false

    matchNextChar delta (word |> Seq.toList) pos

wordsMap
|> Map.filter (fun _ v -> v = 'X')
|> Map.toList
|> List.collect (fun (key, _) -> deltas |> List.map (fun delta -> findWord wordsMap delta "XMAS" key))
|> List.filter id
|> List.length
|> printfn "Day 4a : %i"

let deltaValues posMap pos deltas =
    deltas
    |> List.map (fun delta -> Map.tryFind (addDelta pos delta) posMap |> Option.defaultValue '_')
    |> List.sort

let wordsDeltaValues = deltaValues wordsMap

let hasDiagonalMas pos =
    match wordsDeltaValues pos [ (-1, -1); (1, 1) ], wordsDeltaValues pos [ (-1, 1); (1, -1) ] with
    | [ 'M'; 'S' ], [ 'M'; 'S' ] -> true
    | x, v -> false

wordsMap
|> Map.filter (fun _ v -> v = 'A')
|> Map.toList
|> List.filter (fun (key, _) -> hasDiagonalMas key)
|> List.length
|> printfn "Day 4b : %i"
