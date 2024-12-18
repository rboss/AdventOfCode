let input =
    "./day12_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let gardenMap =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
    |> Array.collect id
    |> Map.ofArray

type FenceDirection =
    | Top of int
    | Right of int
    | Bottom of int
    | Left of int

let sides map (y, x) =
    [ Map.tryFind (y - 1, x) map |> Option.map (fun _ -> Top y, x)
      Map.tryFind (y + 1, x) map |> Option.map (fun _ -> Bottom x, y + 1)
      Map.tryFind (y, x - 1) map |> Option.map (fun _ -> Left x, y)
      Map.tryFind (y, x + 1) map |> Option.map (fun _ -> Right(x + 1), y) ]
    |> List.choose id

let adjacent (y, x) =
    [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]

let plotNeighbours map gType pos =
    let findPos pos =
        Map.tryFind pos map
        |> Option.filter (fun c -> c = gType)
        |> Option.map (fun _ -> pos)

    adjacent pos |> List.choose findPos

let findGroup map (initialPos, gType) =
    let findSides = sides map
    let findPlotNeighbours = plotNeighbours map gType

    let rec nextPlot plots (positions, sides) =
        match plots with
        | [] -> (positions, sides)
        | pos :: tail when Set.contains pos positions -> nextPlot tail (positions, sides)
        | pos :: tail ->
            let newSet = Set.add pos positions

            let fences = findSides pos
            let plotNeighbours = findPlotNeighbours pos

            let newAdjacent =
                plotNeighbours |> List.filter (fun (adjPos) -> not (Set.contains adjPos newSet))

            nextPlot (newAdjacent @ tail) (newSet, Set.union sides (fences |> Set.ofList))

    nextPlot [ initialPos ] (Set.empty, Set.empty)


let rec findNextGroup (gardenMap: Map<(int * int), char>) groups =
    match Seq.tryHead gardenMap with
    | Some s ->
        let (positions, sides) = findGroup gardenMap (s.Key, s.Value)

        let remainingGarden =
            gardenMap |> Map.filter (fun k v -> not (Set.contains k positions))

        findNextGroup remainingGarden ((positions, sides) :: groups)

    | None -> groups

let countSequentialNum (numbers: int seq) : int =
    numbers
    |> Seq.sort
    |> Seq.fold (fun (prev, acc) curr -> (curr, (if curr = prev + 1 then acc else acc + 1))) (System.Int32.MinValue, 0)
    |> snd

let calculateFenceCost (sides: Set<(FenceDirection * int)>) =
    Set.toList sides
    |> List.groupBy fst
    |> List.map (fun (_, cords) -> cords |> List.map snd |> countSequentialNum)
    |> List.sum

findNextGroup gardenMap []
|> List.map (fun (positions, sides) -> Set.count positions * Set.count sides)
|> List.sum
|> printfn "Day 12a : %i"

findNextGroup gardenMap []
|> List.map (fun (positions, sides) -> Set.count positions * (calculateFenceCost sides))
|> List.sum
|> printfn "Day 12b : %i"
