let input = "./input/day18_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day18_example.txt" |> System.IO.File.ReadAllLines

let validPos =
    let obstacles =
        example
        |> Array.map (fun row -> row.Split(","))
        |> Array.map (fun ([| X; Y |]) -> (int Y, int X))

    let allPos =
        [ for y in 0..70 do
              for x in 0..70 -> y, x ]

    allPos |> List.toArray |> Array.except obstacles |> Set.ofArray

let neighbours (Y, X) =
    [ (Y + 1), X; (Y - 1), X; (Y), X + 1; (Y), X - 1 ]

let tryDequeueLowest initState fCost priorityMap =
    let keepMinCost =
        (fun (minKey, minValue) key value ->
            if fCost value < fCost minValue then
                key, value
            else
                minKey, minValue)

    let foldMinCost = Map.fold keepMinCost initState

    if Map.count priorityMap = 0 then
        None
    else
        let (lowestKey, _lowestCost) = foldMinCost priorityMap
        Some((lowestKey, Map.find lowestKey priorityMap), Map.remove lowestKey priorityMap)

let rec dijikstra map startPosition =
    let tryDequeueLowestCost =
        tryDequeueLowest (((0, 0)), (System.Int32.MaxValue, [])) (fun (cost, _) -> cost)

    let isValidMapPos list =
        list |> List.filter (fun ((pos), _) -> Set.contains pos map)

    let notVisited visited list =
        list |> List.filter (fun (v, _) -> Map.containsKey v visited |> not)

    let rec nextNode visitedMap queue =
        match tryDequeueLowestCost queue with
        | None -> visitedMap
        | Some(lowestQueue, newMap) ->
            let ((pos), (cost, path)) = lowestQueue
            let neighbours = neighbours pos

            let foldNeighbours =
                fun queue (neighbourNode, neighbourCost) ->
                    match Map.tryFind neighbourNode queue with
                    | None -> Map.add neighbourNode (neighbourCost, (pos :: path)) queue
                    | Some(queuedCost, queuedPath) ->
                        if neighbourCost = queuedCost then
                            Map.add neighbourNode (neighbourCost, path @ queuedPath) queue
                        else if neighbourCost < queuedCost then
                            Map.add neighbourNode (neighbourCost, pos :: path) queue
                        else
                            queue

            let newQueue =
                neighbours
                |> isValidMapPos
                |> notVisited visitedMap
                |> List.fold foldNeighbours newMap

            let newVisited = Map.add (pos) (cost, path) visitedMap

            nextNode newVisited newQueue

    let initialQueue = Map.empty |> Map.add startPosition (0, [])
    nextNode Map.empty initialQueue
