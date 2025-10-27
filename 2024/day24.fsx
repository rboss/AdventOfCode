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

let sortInput (a, b) = if a < b then (a, b) else (b, a)

let parseInput (input : string) = 
  let [|inputs; gates|] = input.Split(Environment.NewLine + Environment.NewLine)

  let valueInputs = 
    inputs.Split(Environment.NewLine) 
    |> Array.map (fun s -> let [|key; v|] = s.Split ' ' in ((key.TrimEnd(':')), v |> int |> Value))

  let gatesInputs = 
    gates.Split(Environment.NewLine)
    |> Array.map (fun line -> 
        match line with
        | Regex @"(\w+) AND (\w+) -> (\w+)" [in1; in2; out] -> out, And (sortInput(in1, in2))
        | Regex @"(\w+) OR (\w+) -> (\w+)" [in1; in2; out] -> out, Or (sortInput(in1, in2))
        | Regex @"(\w+) XOR (\w+) -> (\w+)" [in1; in2; out] -> out, Xor (sortInput(in1, in2))
    )

  Array.concat [valueInputs; gatesInputs] |> Map.ofArray

let rec calcGateValue gatesMap gate = 
  printfn "Calculating gate %s" gate
  match Map.find gate gatesMap with
  | Value v -> v
  | And (in1, in2) -> calcGateValue gatesMap in1 &&& calcGateValue gatesMap in2
  | Or (in1, in2) -> calcGateValue gatesMap in1 ||| calcGateValue gatesMap in2
  | Xor (in1, in2) -> calcGateValue gatesMap in1 ^^^ calcGateValue gatesMap in2 

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


let checkFullAdder gateMap cIn a b = maybe {
  let tryFind x = Map.tryFind x gateMap
  let! xorAB = tryFind(Xor (sortInput (a, b)))
  let! andAB = tryFind(And (sortInput (a, b)))
  let! cInXor = tryFind(And (sortInput (xorAB, cIn)))
  let! sum = tryFind(Xor (sortInput (xorAB, cIn)))
  let! cout = tryFind(Or (sortInput (cInXor, andAB)))
  
  if not (sum.StartsWith "z") then
    return! None // sum must be a z register
  else
    return (sum, cout)
}

let halfAdder gateMap a b =
  let find x = Map.find x gateMap
  let sum = find(Xor (sortInput (a, b))) 
  let cout = find(And (sortInput (a, b)))

  (sum, cout)

let fullAdderOutputs map cIn a b =
  let find x = 
    printfn "Finding gate %A" x
    Map.find x map

  let xorAB = find(Xor (sortInput (a, b)))
  let andAB = find(And (sortInput (a, b)))

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

let allPairsSelf list = 
  List.allPairs list list |> List.filter (fun (x, y) -> x <> y)

let invertMap map = map |> Map.toList |> List.map (fun (k, v) -> v, k) |> Map.ofList

let solve2 input =
  let gateMap = parseInput input 
  let maxZ = maxZgate gateMap
  let revGateMap = invertMap gateMap

  let (sum, out0) = halfAdder revGateMap "x00" "y00"
  [1..(maxZ - 1)] |> List.mapFold 
    (fun cin i -> 
      let xInput = sprintf "x%02d" i
      let yInput = sprintf "y%02d" i
      let out = checkFullAdder revGateMap cin xInput yInput
      match out with 
      | Some (zOut, cout) -> 
        (None, cout)
      | None -> 
        let outGates = fullAdderOutputs revGateMap cin xInput yInput
        let possibleSwapGates = allPairsSelf outGates
        let ((sum, cout), swapGates) = 
          possibleSwapGates |> List.pick (fun (g1, g2) -> 
            printfn "Trying swap gates %s and %s" g1 g2
            let tryMap = 
              revGateMap
              |> Map.map (fun k v -> if v = g1 then "_tmp_" else v)
              |> Map.map (fun k v -> if v = g2 then g1 else v)
              |> Map.map (fun k v -> if v = "_tmp_" then g2 else v)

            checkFullAdder tryMap cin xInput yInput
            |> Option.map (fun res -> 
              res, (g1,g2) ))
        (Some swapGates, cout)
    ) out0
  |> fst
  |> List.choose id
  |> List.collect (fun (g1, g2) -> [g1; g2])
  |> List.sort
  |> String.concat ","

solve2 input
|> tracePrint "Day 24b : %s" 
|> (=) "fkb,nnr,rdn,rqf,rrn,z16,z31,z37"