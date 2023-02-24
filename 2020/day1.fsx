#load "helpers.fsx"

open Helpers

let input = System.IO.File.ReadAllLines "./day1_input"
let parsedInput = input |> Array.map (fun v -> int v)

seq {
    for x in parsedInput do
        for y in parsedInput do
            yield (x, y)
}
|> Seq.find (fun (x, y) -> x + y = 2020)
|> fun (x, y) -> x * y
|> tracePrint "problem1 %A" // 996075

seq {
    for x in parsedInput do
        for y in parsedInput do
            for z in parsedInput do
                yield [ x; y; z ]
}
|> Seq.find (fun list -> (list |> List.fold (+) 0) = 2020)
|> fun list -> list |> List.fold (*) 1
|> tracePrint "problem2 %A" // 51810360
