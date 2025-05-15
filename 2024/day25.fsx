open System
let example = "./input/day25_example.txt" |> IO.File.ReadAllText
let input = "./input/day25_input.txt" |> IO.File.ReadAllText

type Heights = int array

type Schematic =
    | Lock of Heights
    | Key of Heights

let parseSchematic (rows: string array) : Schematic =
    let heights =
        rows
        |> Array.map (fun s -> s |> Seq.mapi (fun i v -> i, v) |> Seq.toArray)
        |> Array.collect id
        |> Array.groupBy fst
        |> Array.map (snd >> Array.filter (snd >> (=) '#') >> Array.length)

    if rows |> Array.head |> (=) "#####" then
        Lock heights
    else
        Key heights

let keyAndLockMatch lock key =
    Array.zip lock key |> Array.exists (fun (k, l) -> k + l > 7) |> not

let isLock =
    function
    | Lock _ -> true
    | _ -> false

let allMatchingKeysAndLocks ((locks), keys) =
    locks
    |> Array.map (fun (Lock l) -> keys |> Array.filter (fun (Key k) -> keyAndLockMatch l k))

let schematics =
    input.Split(Environment.NewLine + Environment.NewLine)
    |> Array.map (fun s -> s.Split(Environment.NewLine))
    |> Array.map parseSchematic
    |> Array.partition isLock
    |> allMatchingKeysAndLocks
    |> Array.collect id
    |> Array.length
    |> printfn "day25: %i"
