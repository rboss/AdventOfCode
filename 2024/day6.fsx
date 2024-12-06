let input =
    "./day6_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let labMap =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
    |> Array.collect id
    |> Map.ofArray

let startingDirection = (-1, 0)
let movePos (y, x) (dY, dX) = (y + dY, x + dX)
let rotateRight (y, x) = (x, y * -1)
let startingPos = Map.findKey (fun _ v -> v = '^') labMap

type Result =
    | Loop
    | Exited of Set<(int * int)>

let simulateGuard pos startDir map =
    let rec step visited currDir currentPos =

        let newPos = movePos currentPos currDir

        match Map.tryFind newPos map with
        | None -> Exited(visited |> Set.map fst)
        | Some('#') ->
            let newDir = rotateRight currDir
            step visited newDir currentPos
        | _ ->
            if (Set.contains (newPos, currDir) visited) then
                Loop
            else
                step (Set.add (newPos, currDir) visited) currDir newPos

    step (Set.ofList [ (pos, startDir) ]) startDir pos

let visitedGuard =
    labMap
    |> simulateGuard startingPos startingDirection
    |> function
        | Loop -> failwith "unexpected stuck in a loop"
        | Exited visited ->
            visited |> Set.count |> printfn "Day 6a : %i"
            visited

#time

visitedGuard
|> Set.toArray
|> Array.Parallel.map (fun pos -> simulateGuard startingPos startingDirection (Map.add pos '#' labMap))
|> Array.filter ((=) Loop)
|> Array.length
|> printfn "Day 6b : %i"
