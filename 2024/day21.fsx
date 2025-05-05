let input = "./input/day21_input.txt" |> System.IO.File.ReadAllLines
let example = "./input/day21_example.txt" |> System.IO.File.ReadAllLines

let numericPath =
    function
    | ('A', '0') -> [ [ '<' ] ]
    | ('0', '2') -> [ [ '^' ] ]
    | ('2', '9') -> [ [ '>'; '^'; '^' ]; [ '^'; '^'; '>' ]; [ '^'; '>'; '^' ] ]
    | ('9', 'A') -> [ [ 'v'; 'v'; 'v' ] ]
    | ('A', '9') -> [ [ '^'; '^'; '^' ] ]
    | ('9', '8') -> [ [ '<' ] ]
    | ('8', '0') -> [ [ 'v'; 'v'; 'v' ] ]
    | ('0', 'A') -> [ [ '>' ] ]
    | ('A', '1') -> [ [ '<'; '^'; '<' ]; [ '^'; '<'; '<' ] ]
    | ('1', '7') -> [ [ '^'; '^' ] ]
    | ('7', '9') -> [ [ '>'; '>' ] ]
    | ('A', '4') ->
        [ [ '<'; '^'; '<'; '^' ]
          [ '<'; '^'; '^'; '<' ]
          [ '^'; '<'; '<'; '^' ]
          [ '^'; '<'; '^'; '<' ]
          [ '^'; '^'; '<'; '<' ] ]
    | ('4', '0') -> [ [ '>'; 'v'; 'v' ]; [ 'v'; '>'; 'v' ] ]
    | ('4', '5') -> [ [ '>' ] ]
    | ('5', '4') -> [ [ '<' ] ]
    | ('5', '6') -> [ [ '>' ] ]
    | ('6', 'A') -> [ [ 'v'; 'v' ] ]
    | ('A', '3') -> [ [ '^' ] ]
    | ('A', '5') -> [ [ '^'; '^'; '<' ]; [ '^'; '<'; '^' ]; [ '<'; '^'; '^' ] ]
    | ('3', '7') ->
        [ [ '<'; '^'; '<'; '^' ]
          [ '<'; '^'; '^'; '<' ]
          [ '<'; '<'; '^'; '^' ]
          [ '^'; '<'; '<'; '^' ]
          [ '^'; '<'; '^'; '<' ]
          [ '^'; '^'; '<'; '<' ] ]
    | ('A', '7') ->
        [ [ '^'; '^'; '^'; '<'; '<' ]
          [ '^'; '^'; '<'; '^'; '<' ]
          [ '^'; '^'; '<'; '<'; '^' ]
          [ '^'; '<'; '^'; '^'; '<' ]
          [ '^'; '<'; '^'; '<'; '^' ]
          [ '^'; '<'; '<'; '^'; '^' ]
          [ '<'; '^'; '<'; '^'; '^' ]
          [ '<'; '^'; '^'; '<'; '^' ]
          [ '<'; '^'; '^'; '^'; '<' ] ]
    | ('1', '2') -> [ [ '>' ] ]
    | ('7', '8') -> [ [ '>' ] ]
    | ('8', '9') -> [ [ '>' ] ]
    | ('9', '6') -> [ [ 'v' ] ]
    | ('5', '8') -> [ [ '^' ] ]
    | ('8', '2') -> [ [ 'v'; 'v' ] ]
    | ('2', 'A') -> [ [ '>'; 'v' ]; [ 'v'; '>' ] ]
    | ('5', '9') -> [ [ '>'; '^' ]; [ '^'; '>' ] ]
    | x -> failwithf "invalid match case %A" x

let directionalPath =
    function
    | ('A', '^') -> [ [ '<' ] ]
    | ('A', 'v') -> [ [ 'v'; '<' ]; [ '<'; 'v' ] ]
    | ('A', '>') -> [ [ 'v' ] ]
    | ('A', '<') -> [ [ 'v'; '<'; '<' ]; [ '<'; 'v'; '<' ] ]
    | ('^', 'A') -> [ [ '>' ] ]
    | ('^', '>') -> [ [ '>'; 'v' ]; [ 'v'; '>' ] ]
    | ('^', 'v') -> [ [ 'v' ] ]
    | ('^', '<') -> [ [ 'v'; '<' ] ]
    | ('v', '<') -> [ [ '<' ] ]
    | ('v', '^') -> [ [ '^' ] ]
    | ('v', '>') -> [ [ '>' ] ]
    | ('v', 'A') -> [ [ '>'; '^' ]; [ '^'; '>' ] ]
    | ('>', 'v') -> [ [ '<' ] ]
    | ('>', 'A') -> [ [ '^' ] ]
    | ('>', '^') -> [ [ '^'; '<' ]; [ '<'; '^' ] ]
    | ('>', '<') -> [ [ '<'; '<' ] ]
    | ('<', '^') -> [ [ '>'; '^' ] ]
    | ('<', '>') -> [ [ '>'; '>' ] ]
    | ('<', 'A') -> [ [ '>'; '>'; '^' ]; [ '>'; '^'; '>' ] ]
    | ('<', 'v') -> [ [ '>' ] ]
    | x, v when x = v -> [ [] ]
    | x -> failwithf "invalid match case %A" x

let directionalFold current next =
    let result = directionalPath (current, next) |> List.map (fun v -> v @ [ 'A' ])
    (result, next)

let numericalFold current next =
    let result = numericPath (current, next) |> List.map (fun v -> v @ [ 'A' ])
    (result, next)

let numericKeypad seq =
    seq |> Seq.mapFold numericalFold 'A' |> fst

let directionKeypad seq =
    seq |> Seq.mapFold directionalFold 'A' |> fst

let shortestDirectionKeypad recF n seq =
    if n = 1 then
        seq
        |> directionKeypad
        |> Seq.map (Seq.minBy Seq.length)
        |> Seq.collect id
        |> Seq.length
        |> uint64
    else
        seq
        |> directionKeypad
        |> Seq.map (fun alts -> alts |> Seq.map (recF (n - 1)) |> Seq.min)
        |> Seq.sum

let memShortest =
    let dict = System.Collections.Generic.Dictionary<_, _>()

    let rec step n seq =
        let key = sprintf "%i - %A" n seq
        let exist, value = dict.TryGetValue(key)

        match exist with
        | true -> value
        | _ ->
            let value = shortestDirectionKeypad step n seq
            dict.Add(key, value)
            value

    step

let findShortestSequence keypads seq =
    seq
    |> numericKeypad
    |> Seq.map (fun alt -> alt |> Seq.map (memShortest keypads) |> Seq.min)
    |> Seq.sum

let codeToNumeric (code: string) = code[0 .. (code.Length - 2)] |> int

let calcCodeComplexity keypads code =
    let minLength = findShortestSequence keypads code
    let codeNumeric = codeToNumeric code |> uint64
    minLength * codeNumeric

example |> Array.map (calcCodeComplexity 2) |> Array.sum

input |> Array.map (calcCodeComplexity 2) |> Array.sum |> sprintf "Day 21a : %i"

input
|> Array.map (calcCodeComplexity 25)
|> Array.sum
|> sprintf "Day 21b : %i"
