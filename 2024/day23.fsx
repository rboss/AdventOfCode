let input = "./input/day23_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day23_example.txt" |> System.IO.File.ReadAllLines




// algorithm BronKerbosch1(R, P, X) is
//     if P and X are both empty then
//         report R as a maximal clique
//     for each vertex v in P do
//         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
//         P := P \ {v}
//         X := X ⋃ {v}

let neighboursMap =
    Map.ofList
        [ 6, (Set.ofList [ 4 ])
          5, (Set.ofList [ 4; 2; 1 ])
          4, (Set.ofList [ 6; 5; 3 ])
          3, (Set.ofList [ 4; 2 ])
          2, (Set.ofList [ 3; 5; 1 ])
          1, (Set.ofList [ 5; 2 ]) ]

let neighbours v = Map.find v neighboursMap

let bronKerbosch1 vertices =
    let mutable results: Set<int> list = []

    let rec kerbosh R P X =
        if Set.isEmpty P && Set.isEmpty X then
            results <- R :: results
        else
            let mutable P = P
            let mutable X = X

            for v in P do
                let recR = Set.add v R
                let recP = Set.intersect P (neighbours v)
                let recX = Set.intersect P (neighbours v)
                kerbosh recR recP recX
                P <- (Set.remove v P)
                X <- Set.add v X

    kerbosh Set.empty vertices Set.empty
    results

bronKerbosch1 (Set.ofList [ 1; 2; 3; 4; 5; 6 ])
