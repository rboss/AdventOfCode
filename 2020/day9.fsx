#load "helpers.fsx"

open Helpers

let input = System.IO.File.ReadAllLines "./day9_input.txt" |> Array.map (int64)

let testInput =
    [| 35
       20
       15
       25
       47
       40
       62
       55
       65
       95
       102
       117
       150
       182
       127
       219
       299
       277
       309
       576 |]
    |> Array.map (int64)


let findNonSumValue (values: int64 array) preambleSize =
    let rec valueHasSum checkIndex =
        seq {
            for x in values.[checkIndex - preambleSize .. checkIndex - 1] do
                for y in values.[checkIndex - preambleSize .. checkIndex - 1] do
                    if x <> y then yield x + y
        }
        |> Seq.contains values.[checkIndex]
        |> function
            | true -> valueHasSum (checkIndex + 1)
            | false -> checkIndex, values.[checkIndex]

    valueHasSum preambleSize

findNonSumValue input 25 |> tracePrint "problem1: %A"

let findContinousSet (values: int64 list) target =
    let rec pickNextValue list (acc, sequence) =
        match list with
        | nextValue :: _ when nextValue + acc = target-> 
            (sequence @ [v])
        | nextValue :: _ when nextValue + acc > target-> 
            match sequence with 
            | [] -> pickNextValue list (acc, [])
            | head :: [] -> pickNextValue list (acc - head, [])
            | head :: tail -> pickNextValue list (acc - head, tail)
        | v :: rest -> 
            pickNextValue rest (v + acc, sequence @ [v])
        | [] -> failwith "did not find continous sum value in list"


    pickNextValue values (0L, [])

let continousSet = findContinousSet (Array.toList input) 41682220L
tracePrint "problem2: %A" ((List.min continousSet) + (List.max  continousSet))