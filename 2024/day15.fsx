let example = "./day15_example.txt" |> System.IO.File.ReadAllLines
let input = "./day15_input.txt" |> System.IO.File.ReadAllLines

let warehouseMap, directions =
    let splitIndex = Array.findIndex (fun t -> t = "") input
    let mapLines, directionsLines = Array.splitAt splitIndex example

    let map =
        mapLines
        |> Array.map Seq.toArray
        |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
        |> Array.collect id
        |> Map.ofArray

    let directions = directionsLines |> Array.skip 1 |> Seq.collect id |> Seq.toList
    map, directions

let robotStartPos = Map.findKey (fun k v -> v = '@') warehouseMap

let directionDelta =
    function
    | '<' -> (0, -1)
    | '>' -> (0, 1)
    | '^' -> (-1, 0)
    | 'v' -> (1, 0)

let add (y, x) (dy, dx) = y + dy, x + dx

let rec tryMove pos delta map =
    // printfn "trying to move pos %A with delta %A" pos delta
    let movePos = add pos delta
    let currentVal = Map.find pos map

    match Map.tryFind movePos map with
    | None
    | Some '#' -> None
    | Some '.' ->
        // printfn "moving pos %A (%A) to %A" pos currentVal movePos
        let updatedMap = map |> Map.add movePos currentVal |> Map.add pos '.'
        Some(updatedMap, movePos)
    | Some 'O' ->
        match tryMove movePos delta map with
        | Some(newMap, _) ->
            // printfn "moving pos %A (%A) to %A" pos currentVal movePos
            let updatedMap = newMap |> Map.add movePos currentVal |> Map.add pos '.'
            Some(updatedMap, movePos)
        | None -> None
    | Some '@' -> failwith "can't move ontop of robot"

let rec executeDirection map robotPos directions =
    match directions with
    | [] -> map
    | direction :: rest ->
        let delta = directionDelta direction

        match tryMove robotPos delta map with
        | Some(newMap, newPos) -> executeDirection newMap newPos rest
        | None -> executeDirection map robotPos rest

let printMap map =
    for y in 0..9 do
        for x in 0..9 do
            printf "%c" (Map.find (y, x) map)

        printfn ""

    map

printMap warehouseMap

// let move1 = warehouseMap |> tryMove robotStartPos (directionDelta '<')
// // |> Option.map printMap

let resultMap = executeDirection warehouseMap robotStartPos directions
resultMap |> printMap

resultMap
|> Map.filter (fun k v -> v = 'O')
|> Map.map (fun (y, x) _ -> y * 100 + x)
|> Map.values
|> Seq.sum


// let move robot direction =
//     let delta = directionDelta direction

//     delta



// executeDirections input
// |> Array2D.mapi (fun y x c -> c, y * 100 + x)
// |> Array2D.flatten
// |> Array.filter (fun (c, score) -> c = 'O')
// |> Array.map snd
// |> Array.sum
