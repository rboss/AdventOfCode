let example = "./day15_example.txt" |> System.IO.File.ReadAllLines |> Array.toList
let input = "./day15_input.txt" |> System.IO.File.ReadAllLines |> Array.toList

module Array2D =
    let tryGet (arr2d: 'a[,]) (y, x) =
        if (y >= 0 && x >= 0 && y < Array2D.length1 arr2d && x < Array2D.length2 arr2d) then
            Some arr2d[y, x]
        else
            None

    let tryFind needle (arr: 'a[,]) =
        seq {
            for i in 0 .. (arr.GetLength 0 - 1) do
                for j in 0 .. (arr.GetLength 1 - 1) do
                    if arr.[i, j] = needle then yield Some(i, j) else yield None
        }
        |> Seq.tryPick id

    let find needle (arr: 'a[,]) =
        seq {
            for i in 0 .. (arr.GetLength 0 - 1) do
                for j in 0 .. (arr.GetLength 1 - 1) do
                    if arr.[i, j] = needle then yield Some(i, j) else yield None
        }
        |> Seq.pick id


    let flatten (arr2d: 'a[,]) = arr2d |> Seq.cast<'a> |> Seq.toArray

let findEmptySpot =
    Array.takeWhile (fun v -> v <> '#') >> Array.tryFindIndex (fun v -> v = '.')

let executeDirections input =
    let map, directions =
        let splitIndex = List.findIndex (fun t -> t = "") input
        let mapLines, directionsLines = List.splitAt splitIndex input
        let map = array2D mapLines
        let directions = directionsLines |> List.skip 1 |> Seq.collect id |> Seq.toList
        map, directions

    let robotStartPos = Array2D.find '@' map

    let rec runMove ((robotY, robotX): int * int) moves =
        match moves with
        | [] -> map
        | move :: rest ->
            let canMove =
                match move with
                | '<' ->
                    let emptySpace = map[robotY, 0 .. (robotX - 1)] |> Array.rev |> findEmptySpot

                    emptySpace |> Option.map (fun steps -> (0, -1), steps)
                | '>' ->
                    let emptySpace = map[robotY, robotX + 1 ..] |> findEmptySpot

                    emptySpace |> Option.map (fun steps -> (0, 1), steps)
                | '^' ->
                    let emptySpace = map[0 .. robotY - 1, robotX] |> Array.rev |> findEmptySpot

                    emptySpace |> Option.map (fun steps -> (-1, 0), steps)

                | 'v' ->
                    let emptySpace = map[robotY + 1 .., robotX] |> findEmptySpot

                    emptySpace |> Option.map (fun steps -> (1, 0), steps)

            match canMove with
            | Some((dy, dx), steps) ->
                map[robotY, robotX] <- '.'
                map[robotY + dy, robotX + dx] <- '@'

                if steps > 0 then
                    map[robotY + (dy * (steps + 1)), robotX + (dx * (steps + 1))] <- 'O'

                runMove (robotY + dy, robotX + dx) rest
            | None -> runMove (robotY, robotX) rest

    runMove robotStartPos directions


executeDirections input
|> Array2D.mapi (fun y x c -> c, y * 100 + x)
|> Array2D.flatten
|> Array.filter (fun (c, score) -> c = 'O')
|> Array.map snd
|> Array.sum
