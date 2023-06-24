#load "helpers.fsx"

open Helpers

type Program = Instruction array

and Instruction =
    | Nop of int
    | Jmp of int
    | Acc of int

type ProgramResult =
    | Terminated of int
    | LoopDetected of int

let parseInstruction str =
    match str with
    | Regex "nop ([+-]\d+)" [ v ] -> Nop(int v)
    | Regex "acc ([+-]\d+)" [ v ] -> Acc(int v)
    | Regex "jmp ([+-]\d+)" [ v ] -> Jmp(int v)
    | other -> failwith "unexpected input %s" other

let input = System.IO.File.ReadAllLines "./day8_input.txt"

let testInput =
    [| "nop +0"
       "acc +1"
       "jmp +4"
       "acc +3"
       "jmp -3"
       "acc -99"
       "acc +1"
       "jmp -4"
       "acc +6" |]

let executeProgram (instructions: Program) =
    let rec executeInstruction (acc, visited, index) =
        if (index >= instructions.Length) then
            Terminated acc
        elif Set.contains index visited then
            LoopDetected acc
        else
            let newVisited = (Set.add index visited)

            match instructions.[index] with
            | Nop _ -> executeInstruction (acc, newVisited, index + 1)
            | Acc vAcc -> executeInstruction (acc + vAcc, newVisited, index + 1)
            | Jmp vJmp -> executeInstruction (acc, newVisited, index + vJmp)

    executeInstruction (0, Set.empty, 0)

input
|> Array.map parseInstruction
|> executeProgram
|> tracePrint "problem1: %A"


let isNotAcc =
    function
    | (_, Acc _) -> false
    | _ -> true

let findNextModifyIndex (indexedProgram: (int * Instruction) array) startIndex =
    indexedProgram
    |> Array.skip startIndex
    |> Array.find isNotAcc
    |> function
        | index, Nop v -> index, Jmp v
        | index, Jmp v -> index, Nop v
        | index, _ -> failwithf "unexpected trying to modify Acc instruction, index: %i" index

let findAndFixProgram (instructions: Program) =
    let indexedProgram = instructions |> Array.indexed

    let rec modifyAndRun modifyFromIndex =
        let (nextIndex, nextInstruction) =
            findNextModifyIndex indexedProgram modifyFromIndex

        let newProgram = Array.updateAt nextIndex nextInstruction instructions

        match executeProgram newProgram with
        | LoopDetected _ -> modifyAndRun (nextIndex + 1)
        | Terminated acc -> acc

    modifyAndRun 0

input
|> Array.map parseInstruction
|> findAndFixProgram
|> tracePrint "problem2: %i"

// index 163


// modifying index 161, inst Jmp 385
// modifying index 163, inst Jmp 159
