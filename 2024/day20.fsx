let input = "./input/day20_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day20_example.txt" |> System.IO.File.ReadAllLines

let map =
    input
    |> Array.mapi (fun y row -> row |> Seq.toArray |> Array.mapi (fun x c -> (y, x), c))
    |> Array.collect id
    // |> Array.filter (fun (_, c) -> c <> '#')
    |> Map.ofArray

let height = map |> Map.keys |> Seq.maxBy fst
let width = map |> Map.keys |> Seq.maxBy snd
let startPos = Map.findKey (fun key c -> c = 'S') map
let goalPos = Map.findKey (fun key c -> c = 'E') map

let adjacentPos (Y, X) =
    [ (Y + 1), X; (Y - 1), X; (Y), X + 1; (Y), X - 1 ]

let neighbours map pos =
    pos
    |> adjacentPos
    |> List.choose (fun pos -> Map.tryFind pos map |> Option.map (fun v -> pos, v))
// |> List.filter (fun (_, v) -> v <> '#')


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

let rec dijikstra neighboursF startPosition =
    let tryDequeueLowestCost = tryDequeueLowest ((0, 0), System.Int32.MaxValue) id

    let notVisited visited list =
        list |> List.filter (fun (pos) -> Map.containsKey pos visited |> not)

    let rec nextNode visitedMap queue =
        match tryDequeueLowestCost queue with
        | None -> visitedMap
        | Some(lowestQueue, newMap) ->
            let ((pos), (cost)) = lowestQueue
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
                neighboursF pos |> notVisited visitedMap |> List.fold foldNeighbours newMap

            let newVisited = Map.add (pos) (cost) visitedMap

            nextNode newVisited newQueue

    let initialQueue = Map.empty |> Map.add startPosition 0
    nextNode Map.empty initialQueue

let neighboursF = neighbours map >> List.filter (fun (_, v) -> v <> '#')
let distances = dijikstra (neighboursF >> List.map fst) goalPos


let findShortcuts map startPos =
    neighbours map startPos
    |> List.filter (fun (_, v) -> v = '#')
    |> List.map fst
    |> List.collect (neighbours map)
    |> List.filter (fun (_, v) -> v = '.' || v = 'E')
    |> List.filter (fun (endPos, _) -> endPos <> startPos)
    |> List.map (fun (endPos, _) -> (startPos, endPos))

let calculateSavings distances (startPos, endPos) =
    let startCost = Map.find startPos distances
    let endCost = Map.find endPos distances
    -1 * (endCost - startCost + 2)

let shortcutStartPos =
    map |> Map.filter (fun _ c -> c = '.' || c = 'S') |> Map.toList |> List.map fst

shortcutStartPos
|> List.collect (findShortcuts map)
|> List.map (calculateSavings distances)
|> List.filter ((<=) 100)
|> List.length
