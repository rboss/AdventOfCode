let example =
    "./day12_example.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let example2 =
    "./day12_example2.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let input =
    "./day12_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let gardenMap =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
    |> Array.collect id
    |> Map.ofArray

let adjacent (y, x) =
    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]

let findGroup map (initialPos, gType) =
    let findPos pos =
        Map.tryFind pos map
        |> Option.filter (fun v -> v = gType)
        |> Option.map (fun _ -> pos)

    let rec nextPlot plots (positions, sides) =
        match plots with
        | [] -> (positions, sides)
        | pos :: tail when Set.contains pos positions -> nextPlot tail (positions, sides)
        | pos :: tail ->
            let newSet = Set.add pos positions

            let adjacentPlots = pos |> adjacent |> List.choose findPos

            let sidesCount = 4 - (List.length adjacentPlots)

            let newAdjacent =
                adjacentPlots |> List.filter (fun adjPos -> not (Set.contains adjPos newSet))

            nextPlot (newAdjacent @ tail) (newSet, sides + sidesCount)

    nextPlot [ initialPos ] (Set.empty, 0)


let rec findNextGroup (gardenMap: Map<(int * int), char>) groups =
    match Seq.tryHead gardenMap with
    | Some s ->
        let (positions, sides) = findGroup gardenMap (s.Key, s.Value)

        let remainingGarden =
            gardenMap |> Map.filter (fun k v -> not (Set.contains k positions))

        findNextGroup remainingGarden ((positions, sides) :: groups)

    | None -> groups

findNextGroup gardenMap []
|> List.map (fun (positions, sides) -> Set.count positions * sides)
|> List.sum
