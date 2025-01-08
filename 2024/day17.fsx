#load "Shared.fsx"
open Shared
let example = "./day17_example.txt" |> System.IO.File.ReadAllLines
let input = "./day17_input.txt" |> System.IO.File.ReadAllLines

let initRegister, instructions =
    let splitIndex = Array.findIndex (fun t -> t = "") input
    let registerLines, instructionsLines = Array.splitAt splitIndex input

    let register =
        match registerLines with
        | [| RegexInt "Register A: (\d+)" [ a ]; RegexInt "Register B: (\d+)" [ b ]; RegexInt "Register C: (\d+)" [ c ] |] ->
            [ 'A', a; 'B', b; 'C', c ] |> Map.ofList
        | _ -> failwith "Could not parse registers"

    let instructions =
        let instructionsStr = instructionsLines[1][8..]
        instructionsStr.Split(',') |> Array.map int

    register, instructions

let comboValue register operand =
    match operand with
    | 4 -> Map.find 'A' register
    | 5 -> Map.find 'B' register
    | 6 -> Map.find 'C' register
    | x -> x

let run (initRegister: Map<char, int>) (instructions: int array) =
    let rec runInstruction (register: Map<char, int>) pointer out =
        if pointer >= instructions.Length then
            register, List.rev out
        else
            match instructions[pointer], instructions[pointer + 1] with
            | 0, opcode ->
                let opCombo = comboValue register opcode
                runInstruction (Map.add 'A' (register.Item('A') / (pown 2 opCombo)) register) (pointer + 2) out
            | 1, opcode ->
                let newReg = Map.add 'B' (register.Item('B') ^^^ opcode) register
                runInstruction newReg (pointer + 2) out
            | 2, opcode ->
                let newVal = (comboValue register opcode) &&& 7
                let newReg = Map.add 'B' newVal register
                runInstruction newReg (pointer + 2) out
            | 3, opcode ->
                match register.Item('A') with
                | 0 -> runInstruction register (pointer + 2) out
                | x ->
                    let newPointer = opcode
                    runInstruction register newPointer out
            | 4, _ ->
                let newReg = Map.add 'B' (register.Item('B') ^^^ register.Item('C')) register
                runInstruction newReg (pointer + 2) out
            | 5, opcode ->
                let outVal = (comboValue register opcode) &&& 7
                runInstruction register (pointer + 2) (outVal :: out)
            | 6, opcode ->
                let opCombo = comboValue register opcode
                let newReg = Map.add 'B' (register.Item('A') / (pown 2 opCombo)) register

                runInstruction newReg (pointer + 2) out
            | 7, opcode ->
                let opCombo = comboValue register opcode
                let newReg = Map.add 'C' (register.Item('A') / (pown 2 opCombo)) register

                runInstruction newReg (pointer + 2) out

    runInstruction initRegister 0 []

let comboValue2 (regArr: int array) operand =
    match operand with
    | 4 -> regArr[0]
    | 5 -> regArr[1]
    | 6 -> regArr[2]
    | x -> x


let runExpected (instructions: int array) (register: int array) =
    // printfn "running %A" (Map.find 'A' initRegister)

    if (register[0] % 1000000 = 0) then
        printfn "ko %i" register[0] |> ignore

    let rec runInstruction pointer out i =

        if pointer >= instructions.Length then
            printf "Got to end %A - %A" instructions (List.rev out)

            if (instructions |> Array.toList = List.rev out) then
                true
            // Some(register, List.rev out)
            else
                false
        else
            match instructions[pointer], instructions[pointer + 1] with
            | 0, opcode ->
                let opCombo = comboValue2 register opcode
                register[0] <- register[0] / (pown 2 opCombo)
                runInstruction (pointer + 2) out i
            | 1, opcode ->
                register[1] <- (register[1] ^^^ opcode)
                runInstruction (pointer + 2) out i
            | 2, opcode ->
                let newVal = (comboValue2 register opcode) &&& 7
                register[1] <- newVal
                runInstruction (pointer + 2) out i
            | 3, opcode ->
                match register[0] with
                | 0 -> runInstruction (pointer + 2) out i
                | x ->
                    let newPointer = opcode
                    runInstruction newPointer out i
            | 4, _ ->
                register[1] <- (register[1] ^^^ register[2])
                runInstruction (pointer + 2) out i
            | 5, opcode ->
                let outVal = (comboValue2 register opcode) &&& 7

                if (instructions[i] <> outVal) then
                    false
                else
                    runInstruction (pointer + 2) (outVal :: out) (i + 1)
            | 6, opcode ->
                let opCombo = comboValue2 register opcode
                register[1] <- (register[0] / (pown 2 opCombo))

                runInstruction (pointer + 2) out i
            | 7, opcode ->
                let opCombo = comboValue2 register opcode
                register[2] <- (register[0] / (pown 2 opCombo))

                runInstruction (pointer + 2) out i

    runInstruction 0 [] 0



run initRegister instructions
|> snd
|> List.map string
|> List.toSeq
|> String.concat ","
|> printfn "Day 17a : %A"

Seq.initInfinite (fun i -> [| i; 0; 0 |])
|> Seq.map (runExpected instructions)
|> Seq.findIndex ((=) true)

// |> snd
// |> List.map string
// |> List.toSeq
// |> String.concat ","
// |> printfn "Day 17a : %A"


run ([ 'A', 0; 'B', 0; 'C', 9 ] |> Map.ofList) [| 2; 6 |]
run ([ 'A', 10; 'B', 0; 'C', 0 ] |> Map.ofList) [| 5; 0; 5; 1; 5; 4 |]
run ([ 'A', 2024; 'B', 0; 'C', 0 ] |> Map.ofList) [| 0; 1; 5; 4; 3; 0 |]
run ([ 'A', 0; 'B', 29; 'C', 0 ] |> Map.ofList) [| 1; 7 |]
run ([ 'A', 0; 'B', 2024; 'C', 43690 ] |> Map.ofList) [| 4; 0 |]

run ([ 'A', 117440; 'B', 0; 'C', 0 ] |> Map.ofList) [| 0; 3; 5; 4; 3; 0 |]
