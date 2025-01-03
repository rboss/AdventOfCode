let input = "./day15_input.txt" |> System.IO.File.ReadAllLines

type MaybeBuidler() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Option.Some x

let maybe = new MaybeBuidler()

let directionDelta =
    function
    | '<' -> (0, -1)
    | '>' -> (0, 1)
    | '^' -> (-1, 0)
    | 'v' -> (1, 0)

let add (y, x) (dy, dx) = y + dy, x + dx

let printMap map =
    let maxY = Map.fold (fun acc (y, _) _ -> if y > acc then y else acc) 0 map
    let maxX = Map.fold (fun acc (_, x) _ -> if x > acc then x else acc) 0 map

    for y in 0..maxY do
        for x in 0..maxX do
            printf "%c" (Map.find (y, x) map)

        printfn ""

    map

let rec tryMove pos delta map =
    let moveTo = add pos delta
    let tileChar = Map.find pos map

    match Map.tryFind moveTo map with
    | None
    | Some '#' -> None
    | Some '.' -> Some(map |> Map.add moveTo tileChar |> Map.add pos '.', moveTo)
    | Some 'O' ->
        maybe {
            let! newMap, _ = tryMove moveTo delta map
            let updatedMap = newMap |> Map.add moveTo tileChar |> Map.add pos '.'
            return updatedMap, moveTo
        }
    | Some '['
    | Some ']' as c ->
        if delta = (-1, 0) || delta = (1, 0) then
            maybe {
                let! newMap, _ = tryMove moveTo delta map
                let otherSidePos = add moveTo (if c.Value = '[' then (0, 1) else (0, -1))
                let! newMap2, _ = tryMove otherSidePos delta newMap
                return newMap2 |> Map.add moveTo tileChar |> Map.add pos '.', moveTo
            }
        else
            maybe {
                let! newMap, _ = tryMove moveTo delta map
                let updatedMap = newMap |> Map.add moveTo tileChar |> Map.add pos '.'
                return updatedMap, moveTo
            }
    | c -> failwithf "Unexpected tile found : %A" c

let rec runDirections map robotPos directions =
    match directions with
    | [] -> map
    | direction :: rest ->
        let delta = directionDelta direction

        match tryMove robotPos delta map with
        | Some(newMap, newPos) -> runDirections newMap newPos rest
        | None -> runDirections map robotPos rest

let run rowF input =
    let warehouseMap, directions =
        let splitIndex = Array.findIndex (fun t -> t = "") input
        let mapLines, directionsLines = Array.splitAt splitIndex input

        let map =
            mapLines
            |> Array.map rowF
            |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
            |> Array.collect id
            |> Map.ofArray

        let directions = directionsLines |> Array.skip 1 |> Seq.collect id |> Seq.toList
        map, directions

    let robotStartPos = Map.findKey (fun k v -> v = '@') warehouseMap

    runDirections warehouseMap robotStartPos directions

run Seq.toArray input
|> Map.filter (fun k v -> v = 'O')
|> Map.map (fun (y, x) _ -> y * 100 + x)
|> Map.values
|> Seq.sum
|> printfn "Day 15a : %i"

let duplicateTile =
    function
    | '@' -> [ '@'; '.' ]
    | '.' -> [ '.'; '.' ]
    | 'O' -> [ '['; ']' ]
    | '#' -> [ '#'; '#' ]

run (Seq.collect duplicateTile >> Seq.toArray) input
|> Map.filter (fun k v -> v = '[')
|> Map.map (fun (y, x) _ -> y * 100 + x)
|> Map.values
|> Seq.sum
|> printfn "Day 15b : %i"
