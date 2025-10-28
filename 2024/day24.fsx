#load "Shared.fsx"
open Shared
open System
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

let tryFindGateSorted map = function
  | And (i1, i2) -> Map.tryFind (And (sortInput (i1, i2))) map
  | Or (i1, i2) -> Map.tryFind (Or (sortInput (i1, i2))) map
  | Xor (i1, i2) -> Map.tryFind (Xor (sortInput (i1, i2))) map
  | Value x -> Map.tryFind (Value x) map

let parseFullAdder gateMap cIn a b = maybe {
  let tryFind = tryFindGateSorted gateMap
  let! xorAB = tryFind(Xor (a, b))
  let! andAB = tryFind(And (a, b))
  let! cInXor = tryFind(And (xorAB, cIn))
  let! sum = tryFind(Xor (xorAB, cIn))
  let! cout = tryFind(Or (cInXor, andAB))
  
  if not (sum.StartsWith "z") then
    return! None // sum must be a z register
  else
    return (sum, cout)
}

let findFullAdderOutputs map cIn a b =
  let find x = Map.find x map

  let xorAB = find(Xor (sortInput (a, b)))
  let andAB = find(And (sortInput (a, b)))

  let cInOuts = 
    Map.filter (fun k v -> 
      match k with 
      | And (i1, i2) when i1 = cIn || i2 = cIn -> true 
      | Xor (i1, i2) when i1 = cIn || i2 = cIn -> true 
      | _ -> false ) map
    |> Map.values |> Seq.toList

  let abXorAndOuts = 
    Map.filter (fun k v -> 
      match k with 
      | Or (i1, i2) when i1 = xorAB || i2 = xorAB -> true 
      | Or (i1, i2) when i1 = andAB || i2 = andAB -> true 
      | Xor (i1, i2) when i1 = xorAB || i2 = xorAB -> true 
      | Xor (i1, i2) when i1 = andAB || i2 = andAB -> true 
      | _ -> false ) map
    |> Map.values |> Seq.toList

  xorAB :: andAB :: cInOuts @ abXorAndOuts

let allPairsSingle list = 
  List.allPairs list list |> List.filter (fun (x, y) -> x <> y)

let invertMap map = map |> Map.toList |> List.map (fun (k, v) -> v, k) |> Map.ofList

let swapMapValues map v1 v2 = 
  let v1Key = Map.findKey (fun k v -> v = v1) map
  let v2Key = Map.findKey (fun k v -> v = v2) map

  map 
  |> Map.change (v1Key) (fun _ -> Some v2) 
  |> Map.change (v2Key) (fun _ -> Some v1) 

let solve2 input =
  let gateMap = parseInput input 
  let maxZ = maxZgate gateMap
  let revGateMap = invertMap gateMap

  let gateForPos cin i = 
      let xInput = sprintf "x%02d" i
      let yInput = sprintf "y%02d" i
      let out = parseFullAdder revGateMap cin xInput yInput
      match out with 
      | Some (_, cout) -> [], cout
      | None -> 
        let outputGates = findFullAdderOutputs revGateMap cin xInput yInput
        let possibleSwapGates = allPairsSingle outputGates
        possibleSwapGates |> List.pick (fun (g1, g2) -> 
          let swappedMap =  swapMapValues revGateMap g1 g2
          parseFullAdder swappedMap cin xInput yInput |> Option.map (fun (_, cout) -> [g1; g2], cout))

  let cout0 = Map.find (And (sortInput ("x00", "y00"))) revGateMap

  [1..(maxZ - 1)] 
  |> List.mapFold gateForPos cout0
  |> fst
  |> List.collect id
  |> List.sort
  |> String.concat ","

solve2 input
|> tracePrint "Day 24b : %s" 
|> (=) "fkb,nnr,rdn,rqf,rrn,z16,z31,z37"