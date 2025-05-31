let input = "./input/day23_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day23_example.txt" |> System.IO.File.ReadAllLines




// algorithm BronKerbosch1(R, P, X) is
//     if P and X are both empty then
//         report R as a maximal clique
//     for each vertex v in P do
//         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
//         P := P \ {v}
//         X := X ⋃ {v}

let graph =
    Map.ofList
        [ 6, (Set.ofList [ 4 ])
          5, (Set.ofList [ 4; 2; 1 ])
          4, (Set.ofList [ 6; 5; 3 ])
          3, (Set.ofList [ 4; 2 ])
          2, (Set.ofList [ 3; 5; 1 ])
          1, (Set.ofList [ 5; 2 ]) ]

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

bronKerbosch1 graph
