let exampleInput = [ (7, 9.0); (15, 40.0); (30, 200.0) ]
let input = [ (48, 296.0); (93, 1928.0); (85, 1236.0); (95, 1391.0) ]
let exampleInput2 = [ (71530, 940200.0) ]
let input2 = [ (48_938_595, 296_192_812_361_391.0) ]

let quadratic (b, c) =
    let a = 1.0
    let squareRoot = (System.Math.Sqrt(b * b - 4.0 * a * c))
    let dividend = (-1.0 * b)
    let divisor = 2.0 * a
    (dividend - squareRoot) / divisor, (dividend + squareRoot) / divisor

let waysToWin (lower, upper) =
    int (ceil upper - floor lower  - 1.0)

let problem data =
    data
    |> List.map (fun (time, distance) -> -1.0 * float time, distance)
    |> List.map quadratic
    |> List.map waysToWin
    |> List.reduce (*)

problem exampleInput |> printfn "Day6 1.ex %i"
problem input |> printfn "Day6 1.ex %i"

problem exampleInput2 |> printfn "Day6 2.ex %i"
problem input2 |> printfn "Day6 2.ex %i"