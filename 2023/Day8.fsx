let exampleInput =
    [| "RL"
       ""
       "AAA = (BBB, CCC)"
       "BBB = (DDD, EEE)"
       "CCC = (ZZZ, GGG)"
       "DDD = (DDD, DDD)"
       "EEE = (EEE, EEE)"
       "GGG = (GGG, GGG)"
       "ZZZ = (ZZZ, ZZZ)" |]

let exampleInput2 =
    [| "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" |]

let input = System.IO.File.ReadAllLines "day8_input.txt"

let parseNode (line: string) =
    let [| node; strConnections |] = line.Split(" = ")
    // let strReferences = strReferences.Split()
    let [| left; right |] = strConnections.Trim([| '('; ')' |]).Split(", ")

    node, (left, right)

let parseNodes lines =
    lines
    |> Array.map parseNode
    |> Array.fold (fun map (node, connection) -> Map.add node connection map) Map.empty

let parseInput (input: string array) =
    let instructions =
        input[0] |> Seq.map (fun c -> if c = 'L' then fst else snd) |> Seq.toArray

    let nodes = parseNodes (input |> Array.skip 2)
    instructions, nodes

let getInfiniteEnumerator (arr: 'a array) =
    Seq
        .initInfinite(fun index -> (index + 1, arr[index % arr.Length]))
        .GetEnumerator()

let runInstructions input =
    let instructions, nodes = parseInput input

    let instructionsEnumerator = getInfiniteEnumerator instructions

    let rec navigate (step, currentNode) =
        if currentNode = "ZZZ" then
            step
        else
            instructionsEnumerator.MoveNext() |> ignore
            let (nextStep, instruction) = instructionsEnumerator.Current
            navigate (nextStep, Map.find currentNode nodes |> instruction)

    navigate (0, "AAA")

runInstructions exampleInput = 2
runInstructions exampleInput2 = 6
runInstructions input |> printfn "Day8 1: %i" // 19951



let instructions, nodes = parseInput input

let startingNodes =
    nodes
    |> Map.filter (fun key _ -> key.EndsWith('A'))
    |> Map.toList
    |> List.map fst

let instructionsEnumerator = getInfiniteEnumerator instructions

let rec navigate visited (step, currentNode) =
    if Map.exists currentNode visited then
        step
    else
        instructionsEnumerator.MoveNext() |> ignore
        let (nextStep, instruction) = instructionsEnumerator.Current
        navigate visited (nextStep, Map.find currentNode nodes |> instruction)


navigate Map.empty (0, "AAA")
