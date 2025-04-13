let input = "./input/day18_input.txt" |> System.IO.File.ReadAllLines

let allPos =
    [ for y in 0..70 do
          for x in 0..70 -> y, x ]
    |> List.toArray

let bytes =
    input
    |> Array.map (fun row -> row.Split(","))
    |> Array.map (fun ([| X; Y |]) -> (int Y, int X))

let validPosAfterBytes byteNr =
    allPos |> Array.except (Array.take byteNr bytes) |> Set.ofArray

let adjacentPos (Y, X) =
    [ (Y + 1), X; (Y - 1), X; (Y), X + 1; (Y), X - 1 ]

let neighbours validPos pos =
    pos |> adjacentPos |> List.filter (fun pos -> Set.contains pos validPos)

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

let rec dijikstra neighboursF startPosition goal =
    let tryDequeueLowestCost = tryDequeueLowest ((0, 0), System.Int32.MaxValue) id

    let notVisited visited list =
        list |> List.filter (fun (pos) -> Map.containsKey pos visited |> not)

    let rec nextNode visitedMap queue =
        match tryDequeueLowestCost queue with
        | None -> None
        | Some((pos, cost), _) when pos = goal -> Some(cost)
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

let runForBytes bytes =
    dijikstra (validPosAfterBytes bytes |> neighbours) (0, 0) (70, 70)

runForBytes 1024 |> printfn "Day 18a : %A"

let rec bisect f check initMin initMax =
    let rec run min max =
        let next = (min + max) / 2

        if next = min then
            next
        else
            match check (f next) with
            | false -> run min next
            | true -> run next max

    run initMin initMax

// brute force test
seq [ 1024..4000 ]
|> Seq.map (fun i -> i, runForBytes i)
|> Seq.skipWhile (fun (_, res) -> Option.isSome res)
|> Seq.take 1

bisect runForBytes Option.isSome 1024 bytes.Length
|> Array.get bytes
|> fun (f, s) -> s, f
|> printfn "Day 18b : %A"
