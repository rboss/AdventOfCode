let example = "./day16_example.txt" |> System.IO.File.ReadAllLines
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
type Path = Cost * Pos list

let add (y, x) (dy, dx) = y + dy, x + dx
let rotateRight (y, x) = (x, y * -1)
let rotateLeft (y, x) = (x * -1, y)
let rotate180 (y, x) = (y * -1, x * -1)

let getNeighboursCost pos dir (cost) : (Node * Cost) list =
    let straight = add pos dir
    let left = add pos (rotateLeft dir)
    let right = add pos (rotateRight dir)
    let back = add pos (rotate180 dir)

    [ (straight, dir), cost + 1
      (left, (rotateLeft dir)), cost + 1001
      (right, (rotateRight dir)), cost + 1001
      (back, (rotate180 dir)), cost + 2001 ]

let tryDequeueLowest priorityMap =
    if Map.count priorityMap = 0 then
        None
    else
        let lowestItem =
            Map.fold
                (fun (minK, (minV, minPath)) key (v, path) ->
                    if v < minV then
                        (key, (v, path))
                    else
                        (minK, (minV, minPath)))
                (((0, 0), (0, 0)), (System.Int32.MaxValue, []))
                priorityMap

        Some(lowestItem, Map.remove (fst lowestItem) priorityMap)

let rec dijikstra map (que: Map<Node, Path>) vis =
    match tryDequeueLowest que with
    | None -> vis
    | Some(((pos, dir), (cost, path)), newMap) ->
        // printfn "visiting %A dir %A" pos dir
        let newVisited = Map.add (pos, dir) (cost, path) vis
        let neighbours = getNeighboursCost pos dir cost
        // printfn "neighbours %A" neighbours
        let newQue =
            neighbours
            |> List.filter (fun ((pos, _), _) -> Map.containsKey pos map)
            |> List.filter (fun (v, _) -> Map.containsKey v vis |> not)
            |> List.fold
                (fun acc (node, _cost) ->
                    match Map.tryFind node acc with
                    | None -> acc |> Map.add node (_cost, (pos :: path))
                    | Some(eCost, currPath) when _cost = eCost -> acc |> Map.add node (_cost, path @ currPath)
                    | Some(eCost, _) when _cost < eCost -> acc |> Map.add node (_cost, pos :: path)
                    | _ -> acc)
                newMap

        // printfn "newQue %A" newQue
        // printfn "visited %A" newVisited

        dijikstra map newQue newVisited


let distances =
    map |> Map.map (fun _ _ -> System.Int32.MaxValue) |> Map.add startPos 0

let visited = Map.empty<Node, Path>
let queue = Map.empty<Node, Path> |> Map.add (startPos, (0, 1)) (0, [])

let res = dijikstra map queue visited


res
|> Map.filter (fun (pos, dir) _ -> pos = goalPos)
|> Map.toList
|> List.minBy (fun (_, (cost, _)) -> cost)
|> (snd >> snd)
|> List.distinct
|> List.length

Map.find (((1, 13), (-1, 0))) res |> snd |> List.distinct |> List.length


// dijikstra queue visited distances
