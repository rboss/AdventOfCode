#load "Shared.fsx"

let input = "./day13_input.txt" |> System.IO.File.ReadAllLines |> Array.toList

open Shared

let parseMachineInput rows =
    match rows with
    | RegexInt "Button .: X\+(\d+), Y\+(\d+)" [ a1; a2 ] :: RegexInt "Button .: X\+(\d+), Y\+(\d+)" [ b1; b2 ] :: RegexInt "Prize: X=(\d+), Y=(\d+)" [ c1
                                                                                                                                                       c2 ] :: _ ->
        (int64 a1, int64 a2), (int64 b1, int64 b2), (int64 c1, int64 c2)

let cramerCalc ((a1, a2), (b1, b2), (c1, c2)) =
    let x = (c1 * b2 - b1 * c2) / (a1 * b2 - b1 * a2)
    let y = (a1 * c2 - c1 * a2) / (a1 * b2 - b1 * a2)

    if x * a1 + y * b1 = c1 && x * a2 + y * b2 = c2 then
        Some(x, y)
    else
        None

let calcTokens (x, y) = x * 3L + y

input
|> List.chunkBySize 4
|> List.map parseMachineInput
|> Seq.choose cramerCalc
|> Seq.map calcTokens
|> Seq.sum
|> printfn "Day 13a : %i"

input
|> List.chunkBySize 4
|> List.map parseMachineInput
|> List.map (fun (a, b, (c1, c2)) -> a, b, (c1 + 10000000000000L, c2 + 10000000000000L))
|> Seq.choose cramerCalc
|> Seq.map calcTokens
|> Seq.sum
|> printfn "Day 13b : %i"
