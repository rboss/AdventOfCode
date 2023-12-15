let exampleInput =
    [| "RL"

       "AAA = (BBB, CCC)"
       "BBB = (DDD, EEE)"
       "CCC = (ZZZ, GGG)"
       "DDD = (DDD, DDD)"
       "EEE = (EEE, EEE)"
       "GGG = (GGG, GGG)"
       "ZZZ = (ZZZ, ZZZ)" |]

let parseNode line =
    lines
    |>

    let parseInput input =
        let instructions = input[0]
        let nodes = parseNodes (input |> Array.skip 2)
        instructions in


    parseInput exampleInput
