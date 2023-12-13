#load "Shared.fsx"

open System
open Shared

let exampleInput = [ (7, 9.0); (15, 40.0); (30, 200.0) ]
let input = [ (48, 296.0); (93, 1928.0); (85, 1236.0); (95, 1391.0) ]
let exampleInput2 = [ (71530, 940200.0) ]
let input2 = [ (48_938_595, 296_192_812_361_391.0) ]

let quadratic (b, c) =
    let a = 1.0
    let squareRoot = (Math.Sqrt(b * b - 4.0 * a * c))
    let dividend = (-1.0 * b)
    let divisor = 2.0 * a
    (dividend - squareRoot) / divisor, (dividend + squareRoot) / divisor

let waysToWin (lower, upper) =
    [| ceil lower .. floor upper |] |> Array.except [ lower; upper ] |> Array.length

let problem1 data =
    data
    |> List.map (fun (time, (distance: int64)) -> -1.0 * float time, float distance)
    |> List.map quadratic
    |> List.map waysToWin
    |> List.reduce (*)

let problem2 data =
    data
    |> List.map (fun (time, (distance: int64)) -> -1.0 * float time, float distance)
    |> List.map quadratic
    |> List.map waysToWin
    |> List.reduce (*)

problem1 exampleInput |> tracePrint "Day6 1.ex %i"
problem1 input |> tracePrint "Day6 1.ex %i"

problem1 exampleInput2 |> tracePrint "Day6 2.ex %i"
problem1 input2 |> tracePrint "Day6 2.ex %i"
