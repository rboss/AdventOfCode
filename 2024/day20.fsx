let input = "./input/day20_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day20_example.txt" |> System.IO.File.ReadAllLines

let map =
    input
    |> Array.mapi (fun y row -> row |> Seq.toArray |> Array.mapi (fun x c -> (y, x), c))
    |> Array.collect id
    |> Map.ofArray

let adjacentPos (Y, X) =
    [ (Y + 1), X; (Y - 1), X; (Y), X + 1; (Y), X - 1 ]

let neighbours map pos =
    pos
    |> adjacentPos
    |> List.choose (fun pos -> Map.tryFind pos map |> Option.map (fun v -> pos, v))

let calcDistances posMap =
    let startPos = Map.findKey (fun _ c -> c = 'S') map
    let goalPos = Map.findKey (fun _ c -> c = 'E') map
    let neighboursF = neighbours posMap

    let next (cost, prev, curr) =
        if curr = startPos then
            None
        else
            let nextCost = cost + 1

            let nextPos =
                neighboursF curr
                |> List.filter (fun (pos, v) -> pos <> prev && v <> '#')
                |> List.head
                |> fst

            Some((nextPos, nextCost), (nextCost, curr, nextPos))

    (goalPos, 0) :: List.unfold next (0, (-1, -1), goalPos)

let distanceArray = calcDistances map |> List.toArray |> Array.sortBy snd

let manhattanDistance (x2, y2) (x1, y1) = abs (y1 - y2) + abs (x1 - x2)

type DistanceArray = ((int * int) * int) array

let shortCutsForPos (distances: DistanceArray) maxCheatDistance minSave =
    fun index (startPos, startCost) ->
        distances[(index + 1 + minSave) ..]
        |> Seq.map (fun (pos, cost) -> pos, cost, manhattanDistance startPos pos)
        |> Seq.filter (fun (_, _, cheatDistance) -> cheatDistance <= maxCheatDistance)
        |> Seq.filter (fun (pos, cost, cheatDistance) -> cost - startCost - cheatDistance >= minSave)

let findShortcuts (distances: DistanceArray) cheatDistance minSave =
    let startDomain = distances[0 .. distances.Length - minSave]

    startDomain
    |> Seq.mapi (shortCutsForPos distances cheatDistance minSave)
    |> Seq.collect id

findShortcuts distanceArray 2 100 |> Seq.length |> sprintf "Day 20a : %i"

findShortcuts distanceArray 20 100 |> Seq.length |> sprintf "Day 20b : %i"
