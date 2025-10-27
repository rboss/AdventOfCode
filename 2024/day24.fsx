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

let sortIn (a, b) = if a < b then (a, b) else (b, a)

let parseInput (input : string) = 
  let [|inputs; gates|] = input.Split(Environment.NewLine + Environment.NewLine)

  let valueInputs = 
    inputs.Split(Environment.NewLine) 
    |> Array.map (fun s -> let [|key; v|] = s.Split ' ' in ((key.TrimEnd(':')), v |> int |> Value))

  let gatesInputs = 
    gates.Split(Environment.NewLine)
    |> Array.map (fun line -> 
        match line with
        | Regex @"(\w+) AND (\w+) -> (\w+)" [in1; in2; out] -> out, And (sortIn(in1, in2))
        | Regex @"(\w+) OR (\w+) -> (\w+)" [in1; in2; out] -> out, Or (sortIn(in1, in2))
        | Regex @"(\w+) XOR (\w+) -> (\w+)" [in1; in2; out] -> out, Xor (sortIn(in1, in2))
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

let revTuple (x, y) = (y, x)

let revGateMap = 
  Map.toList (parseInput input) 
  |> List.map revTuple 
  |> Map.ofList


let fullAdder map cIn a b =
  let find x = Map.find x map
  let xorAB = find(Xor (sortIn (a, b)))
  let andAB = find(And (sortIn (a, b)))
  let cInXor = find(And (sortIn (xorAB, cIn)))
  let sum = find(Xor (sortIn (xorAB, cIn)))
  let cout = find(Or (sortIn (cInXor, andAB)))

  (sum, cout)

let halfAdder map a b =
  let find x = Map.find x map
  let sum = find(Xor (sortIn (a, b)))
  let cout = find(And (sortIn (a, b)))

  (sum, cout)

let (_, cout0) = halfAdder revGateMap "x00" "y00"

[1..44]
|> List.fold 
  (fun cin i -> 
    printfn "finding full adder for bit %d - %A" i cin
    let (zOut, cout) = fullAdder revGateMap cin (sprintf "x%02d" i) (sprintf "y%02d" i)
    printfn "found: zOut=%s cout=%A" zOut cout
    cout
  ) 
  cout0


let fullAdderOutputs map cIn a b =
  let find x = 
    printfn "Finding gate %A" x
    Map.find x map

  let xorAB = find(Xor (sortIn (a, b)))
  let andAB = find(And (sortIn (a, b)))

  let cInOuts = 
    Map.filter (fun k v -> 
      match k with 
      | And (i1, i2) when i1 = cIn || i2 = cIn -> true 
      | Xor (i1, i2) when i1 = cIn || i2 = cIn -> true 
      | _ -> false ) map
    |> Map.values

  let abXorAndOuts = 
    Map.filter (fun k v -> 
      match k with 
      | Or (i1, i2) when i1 = xorAB || i2 = xorAB -> true 
      | Or (i1, i2) when i1 = andAB || i2 = andAB -> true 
      | Xor (i1, i2) when i1 = xorAB || i2 = xorAB -> true 
      | Xor (i1, i2) when i1 = andAB || i2 = andAB -> true 
      | _ -> false ) map
    |> Map.values

  Seq.concat [seq[andAB]; seq[xorAB]; cInOuts; abXorAndOuts] |> Seq.toList

fullAdderOutputs revGateMap "kcm" "x16" "y16"


let cIn = "kcm"
Map.filter (fun k v -> 
  match k with 
  | And (i1, i2) when i1 = cIn || i2 = cIn -> true 
  | Xor (i1, i2) when i1 = cIn || i2 = cIn -> true 
  | _ -> false ) revGateMap
|> Map.values