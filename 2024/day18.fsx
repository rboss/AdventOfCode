let input = "./input/day18_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day18_example.txt" |> System.IO.File.ReadAllLines

let allValidPos =
    let obstacles =
        input
        |> Array.take 1024
        |> Array.map (fun row -> row.Split(","))
        |> Array.map (fun ([| X; Y |]) -> (int Y, int X))

    let allPos =
        [ for y in 0..70 do
              for x in 0..70 -> y, x ]

    allPos |> List.toArray |> Array.except obstacles |> Set.ofArray

let validPos pos = Set.contains pos allValidPos

let adjacentPos (Y, X) =
    [ (Y + 1), X; (Y - 1), X; (Y), X + 1; (Y), X - 1 ]

let neighbours pos =
    pos |> adjacentPos |> List.filter validPos

let tryDequeueLowest initState fCost priorityMap =
    let keepMinCost =
        (fun (minKey, minValue) key value ->
            if fCost value < fCost minValue then
                key, value
            else
                minKey, minValue)

    let foldMinCost = Map.fold keepMinCost initState

    if Map.isEmpty priorityMap then
        None
    else
        let (lowestKey, _lowestCost) = foldMinCost priorityMap
        Some((lowestKey, Map.find lowestKey priorityMap), Map.remove lowestKey priorityMap)

let rec dijikstra map startPosition =
    let tryDequeueLowestCost = tryDequeueLowest ((0, 0), System.Int32.MaxValue) id

    let notVisited visited list =
        list |> List.filter (fun (pos) -> Map.containsKey pos visited |> not)

    let rec nextNode visitedMap queue =
        match tryDequeueLowestCost queue with
        | None -> visitedMap
        | Some(lowestQueue, newMap) ->
            let ((pos), (cost)) = lowestQueue
            let neighbours = neighbours pos
            let neighbourCost = cost + 1

            let foldNeighbours =
                fun queue (neighbourNode) ->
                    match Map.tryFind neighbourNode queue with
                    | None -> Map.add neighbourNode neighbourCost queue
                    | Some(queuedCost) ->
                        if neighbourCost < queuedCost then
                            Map.add neighbourNode neighbourCost queue
                        else
                            queue

            let newQueue =
                neighbours |> notVisited visitedMap |> List.fold foldNeighbours newMap

            let newVisited = Map.add (pos) (cost) visitedMap

            nextNode newVisited newQueue

    let initialQueue = Map.empty |> Map.add startPosition 0
    nextNode Map.empty initialQueue

dijikstra () (0, 0) |> Map.find (70, 70) |> printfn "Day 18a : %i"
