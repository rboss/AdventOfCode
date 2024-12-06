let input =
    "./day6_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let example =
    "./day6_example.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let labMap =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
    |> Array.collect id
    |> Map.ofArray

let startingPos = Map.findKey (fun _ v -> v = '^') labMap
let startingDirection = (-1, 0)
let addDelta (y, x) (dY, dX) = (y + dY, x + dX)
let rotateRight (y, x) = (x, y * -1)


let simulateGuard pos direction map =
    let rec step acc currDir currentPos =

        let newPos = addDelta currentPos currDir

        match Map.tryFind newPos map with
        | None -> acc
        | Some('#') ->
            let newDir = rotateRight currDir
            step acc newDir currentPos
        | _ -> step (newPos :: acc) currDir newPos

    step [ pos ] direction pos

simulateGuard startingPos startingDirection labMap
|> List.distinct
|> List.length
|> printfn "Day 6a : %i"
