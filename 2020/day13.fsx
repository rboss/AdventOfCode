#load "helpers.fsx"

open System
open Helpers

let input = System.IO.File.ReadAllLines "./day13_input.txt" |> Array.toList
let testInput = [ "939"; "7,13,x,x,59,x,31,19" ]
let testInput2 = [ "939"; "17,x,13,19" ]

let parseInput list =
    match list with
    | Integer earliest :: strBuses :: [] ->
        earliest,
        strBuses.Split(',')
        |> Array.map (function
            | Integer v -> Some v
            | _ -> None)
    | x -> failwithf "could not match input %A" x

let calcBusWaitTime earliest busStartTime =
    busStartTime - (earliest % busStartTime)

let (earliestTime, buses) = parseInput input

buses
|> Array.choose id
|> Array.map (fun bus -> bus, calcBusWaitTime earliestTime bus)
|> Array.minBy snd
|> (fun (x1, x2) -> x1 * x2)
|> tracePrint "problem1: %i"


let stopsAtTime (time: int64) (busId: int64) = time % busId = 0

let allBusesStartSubsequent buses time =
    printfn "time %u" time
    buses |> Array.forall (fun (i, b) -> stopsAtTime (time + (int64) i) b)

let findFirstSubsequentStartTime buses =
    let _, firstBusTime = Array.head buses
    let subsequentBuses = Array.tail buses

    let allBusesStart = allBusesStartSubsequent subsequentBuses

    { firstBusTime..firstBusTime .. Int64.MaxValue } |> Seq.tryFind allBusesStart

parseInput input
|> snd
|> Array.mapi (fun i b -> i, b)
|> Array.choose (function
    | _, None -> None
    | i, Some b -> Some((int64) i, (int64) b))
|> findFirstSubsequentStartTime
|> Option.get
|> tracePrint "problem2: %u"
