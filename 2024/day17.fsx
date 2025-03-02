#load "Shared.fsx"
open Shared
let example = "./day17_example.txt" |> System.IO.File.ReadAllLines
let example2 = "./day17_example.txt" |> System.IO.File.ReadAllLines
let input = "./day17_input.txt" |> System.IO.File.ReadAllLines

let initRegister, instructions =
    let splitIndex = Array.findIndex (fun t -> t = "") input
    let registerLines, instructionsLines = Array.splitAt splitIndex input

    let register =
        match registerLines with
        | [| RegexInt "Register A: (\d+)" [ a ]; RegexInt "Register B: (\d+)" [ b ]; RegexInt "Register C: (\d+)" [ c ] |] ->
            int64 a, int64 b, int64 c
        | _ -> failwith "Could not parse registers"

    let instructions =
        let instructionsStr = instructionsLines[1][8..]
        instructionsStr.Split(',') |> Array.map int

    register, instructions

let comboValue (A, B, C) operand =
    match operand with
    | 4 -> A
    | 5 -> B
    | 6 -> C
    | x -> int64 x

let runInstructions (instructions: int array) initRegister =
    let rec next (register: int64 * int64 * int64) pointer =
        seq {
            if pointer >= instructions.Length then
                yield! []
            else
                let (A, B, C) = register

                match instructions[pointer], instructions[pointer + 1] with
                | 0, opcode ->
                    let opCombo = comboValue register opcode
                    yield! next (A / (pown 2L (int opCombo)), B, C) (pointer + 2)
                | 1, opcode -> yield! next (A, B ^^^ opcode, C) (pointer + 2)
                | 2, opcode -> yield! next (A, (comboValue register opcode) &&& 7, C) (pointer + 2)
                | 3, opcode ->
                    match A with
                    | 0L -> yield! next register (pointer + 2)
                    | _ -> yield! next register opcode
                | 4, _ -> yield! next (A, B ^^^ C, C) (pointer + 2)
                | 5, opcode ->
                    yield (comboValue register opcode) &&& 7
                    yield! next register (pointer + 2)
                | 6, opcode ->
                    let opCombo = comboValue register opcode
                    yield! next (A, A / (pown 2L (int opCombo)), C) (pointer + 2)
                | 7, opcode ->
                    let opCombo = comboValue register opcode
                    yield! next (A, B, A / (pown 2L (int opCombo))) (pointer + 2)
        }


    next initRegister 0

runInstructions instructions initRegister
|> Seq.toList
|> List.map string
|> List.toSeq
|> String.concat ","
|> fun s ->
    printfn "Day 17a : %A" s
    s
|> (=) "1,5,3,0,2,5,2,5,3"

let findOutputs instructions =
    let runFirstOutput A =
        A, runInstructions instructions (A, 0L, 0L) |> Seq.head

    let rec nextOutput outputs A canBeZero =
        match outputs with
        | [] -> seq [ Some A ]
        | goal :: rest ->
            [ 0L .. 7L ]
            |> Seq.map ((+) (A <<< 3))
            |> Seq.filter (fun nextA -> canBeZero || nextA <> 0L)
            |> Seq.map runFirstOutput
            |> Seq.filter (fun (_, output) -> goal = output)
            |> Seq.collect (fun (nextA, _) -> nextOutput rest nextA true)

    let expectedOutputs = (instructions |> Array.rev |> Array.toList |> List.map int64)

    nextOutput expectedOutputs 0 false

findOutputs instructions |> Seq.head
