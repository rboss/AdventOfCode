let input = "./day16_input.txt" |> System.IO.File.ReadAllLines

let map =
    input
    |> Array.mapi (fun y row -> row |> Seq.toArray |> Array.mapi (fun x c -> (y, x), c))
    |> Array.collect id
    |> Array.filter (fun (_, c) -> c <> '#')
    |> Map.ofArray

let startPos = Map.findKey (fun key c -> c = 'S') map
let goalPos = Map.findKey (fun key c -> c = 'E') map

type Node = Pos * Direction
and Pos = int * int
and Direction = int * int
type Cost = int

let add (y, x) (dy, dx) = y + dy, x + dx
let rotateRight (y, x) = (x, y * -1)
let rotateLeft (y, x) = (x * -1, y)
let rotate180 (y, x) = (y * -1, x * -1)

let getNeighbours pos dir (cost) : (Node * Cost) list =
    let straight = add pos dir
    let left = add pos (rotateLeft dir)
    let right = add pos (rotateRight dir)
    let back = add pos (rotate180 dir)

    [ (straight, dir), cost + 1
      (left, (rotateLeft dir)), cost + 1001
      (right, (rotateRight dir)), cost + 1001
      (back, (rotate180 dir)), cost + 2001 ]

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
        tryDequeueLowest (((0, 0), (0, 0)), (System.Int32.MaxValue, [])) (fun (cost, _) -> cost)

    let isValidMapPos list =
        list |> List.filter (fun ((pos, _), _) -> Map.containsKey pos map)

    let notVisited visited list =
        list |> List.filter (fun (v, _) -> Map.containsKey v visited |> not)

    let rec nextNode visitedMap queue =
        match tryDequeueLowestCost queue with
        | None -> visitedMap
        | Some(lowestQueue, newMap) ->
            let ((pos, dir), (cost, path)) = lowestQueue
            let neighbours = getNeighbours pos dir cost

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

            let newVisited = Map.add (pos, dir) (cost, path) visitedMap

            nextNode newVisited newQueue

    let initialQueue = Map.empty |> Map.add startPosition (0, [])
    nextNode Map.empty initialQueue

dijikstra map (startPos, (0, 1))
|> Map.filter (fun (pos, dir) _ -> pos = goalPos)
|> Map.toList
|> List.map (fun (_, (cost, _)) -> cost)
|> List.min
|> printfn "Day 16a : %i"

dijikstra map (startPos, (0, 1))
|> Map.filter (fun (pos, dir) _ -> pos = goalPos)
|> Map.toList
|> List.minBy (fun (_, (cost, _)) -> cost)
|> (snd >> snd)
|> List.distinct
|> List.length
|> (+) 1
|> printfn "Day 16b : %i"
