
#load "Shared.fsx"
open Shared
open System
let example = "./input/day24_example2.txt" |> IO.File.ReadAllText
let input = "./input/day24_input.txt" |> IO.File.ReadAllText

type GateInput =
    | And of string * string
    | Or of string * string
    | Xor of string * string
    | Value of int

let parseInput (input : string) = 
  let [|inputs; gates|] = input.Split(Environment.NewLine + Environment.NewLine)

  let valueInputs = 
    inputs.Split(Environment.NewLine) 
    |> Array.map (fun s -> let [|key; v|] = s.Split ' ' in ((key.TrimEnd(':')), v |> int |> Value))

  let gatesInputs = 
    gates.Split(Environment.NewLine)
    |> Array.map (fun line -> 
        match line with
        | Regex @"(\w+) AND (\w+) -> (\w+)" [in1; in2; out] -> out, And (in1, in2)
        | Regex @"(\w+) OR (\w+) -> (\w+)" [in1; in2; out] -> out, Or (in1, in2)
        | Regex @"(\w+) XOR (\w+) -> (\w+)" [in1; in2; out] -> out, Xor (in1, in2)
    )

  Array.concat [valueInputs; gatesInputs] |> Map.ofArray


let rec calcGateValue gatesMap gate = 
  printfn "Calculating gate %s" gate
  match Map.find gate gatesMap with
  | Value v -> v
  | And (in1, in2) -> (calcGateValue gatesMap in1) &&& (calcGateValue gatesMap in2)
  | Or (in1, in2) -> (calcGateValue gatesMap in1) ||| (calcGateValue gatesMap in2)
  | Xor (in1, in2) -> (calcGateValue gatesMap in1) ^^^ (calcGateValue gatesMap in2) 

let binaryToInt (bits: int seq) =
    bits
    |> Seq.mapi (fun i b -> int64 b <<< i)
    |> Seq.sum


let maxZgate (map : Map<string, _>) = 
  map
  |> Map.keys
  |> Seq.filter (fun k -> k.StartsWith "z")
  |> Seq.map (fun k -> k.Substring(1))
  |> Seq.max |> int

let solve1 input = 
  let map = parseInput input
  let calcGate = calcGateValue map 
  let maxZ = maxZgate map
  [0..maxZ]
  |> Seq.map (sprintf "z%02d") 
  |> Seq.map calcGate
  |> binaryToInt

solve1 input
|> printfn "Day 24a : %i"
