let input = "./input/day23_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day23_example.txt" |> System.IO.File.ReadAllLines

let parseInput (lines: string array) =
    let connections =
        lines
        |> Array.map (fun line -> line.Split('-'))
        |> Array.collect (fun [| c1; c2 |] -> [| c1, c2; c2, c1 |])
        |> Array.toList

    let allComputers = connections |> List.map fst |> List.distinct

    let connectionsSet c =
        connections |> List.filter (fun (c1, _) -> c = c1) |> List.map snd |> Set.ofList

    allComputers |> List.map (fun c -> c, connectionsSet c) |> Map.ofList

let parseInput2 (lines: string array) =
    let connections =
        lines
        |> Array.map (fun line -> line.Split('-'))
        |> Array.collect (fun [| c1; c2 |] -> [| c1, c2; c2, c1 |])

    let allComputers = connections |> Array.map fst |> Array.distinct

    let getConnections c =
        connections |> Array.filter (fun (c1, _) -> c = c1) |> Array.map snd

    allComputers |> Array.map (fun c -> c, getConnections c) |> readOnlyDict

let part1 (lines: string array) =
    let connectionsDict = lines |> parseInput2

    connectionsDict.Keys
    |> Seq.collect (fun c1 ->
        connectionsDict[c1]
        |> Seq.collect (fun c2 ->
            connectionsDict[c2]
            |> Seq.collect (fun c3 ->
                connectionsDict[c3]
                |> Seq.filter (fun c3_1 -> c3_1 = c1)
                |> Seq.map (fun _ -> [| c1; c2; c3 |] |> Array.sort))))
    |> Seq.distinct
    |> Seq.filter (Array.exists (fun v -> v.StartsWith('t')))
    |> Seq.length


let bronKerbosch1 graphMap =
    let N v = Map.find v graphMap

    let rec kerbosh R P X =

        if Set.isEmpty P && Set.isEmpty X then
            [ R ]
        else
            P
            |> Set.fold
                (fun (acc, P, X) v ->
                    let newAcc =
                        let vN = N v
                        let nextR = Set.add v R
                        let nextP = Set.intersect P vN
                        let nextX = Set.intersect X vN
                        (kerbosh nextR nextP nextX)

                    newAcc @ acc, Set.remove v P, Set.add v X)
                ([], P, X)
            |> (fun (acc, _, _) -> acc)

    let vertices = Map.keys graphMap
    kerbosh Set.empty (Set.ofSeq vertices) Set.empty

let bronKerbosch2 (connectionsDict: System.Collections.Generic.IReadOnlyDictionary<string, array<string>>) =
    let sortedGraph =
        connectionsDict
        |> seq
        |> Seq.sortByDescending (fun pair -> Array.length pair.Value)
        |> Seq.toList


    let neighbours v = connectionsDict[v]

    let choosePivot P U =
        let union = Set.union P U |> Set.toArray
        sortedGraph |> List.find (fun (x, _) -> Array.contains x union) |> fst

    let rec kerbosh R P X =

        if Set.isEmpty P && Set.isEmpty X then
            [ R ]
        else
            let pivot = choosePivot P X

            Set.difference P (neighbours pivot)
            |> Set.fold
                (fun (acc, P, X) v ->
                    let newAcc =
                        let vN = neighbours v
                        let nextR = Set.add v R
                        let nextP = Set.intersect P vN
                        let nextX = Set.intersect X vN
                        (kerbosh nextR nextP nextX)

                    newAcc @ acc, Set.remove v P, Set.add v X)
                ([], P, X)
            |> (fun (acc, _, _) -> acc)

    let vertices = Map.keys graphMap
    kerbosh Set.empty (Set.ofSeq vertices) Set.empty

let part2 lines =
    lines
    |> parseInput2
    |> bronKerbosch2
    |> List.sortByDescending Set.count
    |> List.head
    |> Set.toArray
    |> String.concat (",")


part1 input |> printfn "Day23 a : %i"

part2 input |> printfn "Day23 b : %s"
