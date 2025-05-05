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

let memoize f =
    let dict = System.Collections.Generic.Dictionary<_, _>()

    fun c ->
        let exist, value = dict.TryGetValue c

        match exist with
        | true -> value
        | _ ->
            let value = f c
            dict.Add(c, value)
            value

let directionalFold current next =
    let result = directionalPath (current, next) |> List.map (fun v -> v @ [ 'A' ])
    (result, next)

let numericalFold current next =
    let result = numericPath (current, next) |> List.map (fun v -> v @ [ 'A' ])
    (result, next)

let rec permute listsOfLists =
    match listsOfLists with
    | list :: rest -> list |> List.collect (fun v -> permute rest |> List.map (fun k -> v :: k))
    | [] -> [ [] ]

let numericKeypad2 seq =
    seq
    |> Seq.mapFold numericalFold 'A'
    |> fst
    |> Seq.toList
    |> List.map (List.collect id)

numericKeypad2 "029" |> List.map (Seq.map string >> String.concat "")

let directionKeypad2 seq =
    seq
    |> Seq.mapFold directionalFold 'A'
    |> fst
    |> Seq.toList
    |> permute
    |> List.map (List.collect id)

let mutable (counter: int) = 0
let mutable (counter2: int) = 0

let shortestDirectionKeypad seq =
    counter2 <- counter2 + 1
    printfn "Seq %A" seq
    seq |> directionKeypad2 |> Seq.minBy List.length

let (memShortestDir: char seq -> char list) = memoize shortestDirectionKeypad

let shortestDirectionKeypad2 seq =
    counter <- counter + 1
    seq |> directionKeypad2 |> Seq.map memShortestDir |> Seq.minBy List.length

let (memShortestDir2: char seq -> char list) = memoize shortestDirectionKeypad2

let findShortestSequence seq =
    seq
    |> numericKeypad2
    |> List.map (fun v -> v |> Seq.map memShortestDir2)
    |> List.map (fun v -> v |> Seq.toList |> List.collect id)
    |> List.minBy List.length

let codeToNumeric (code: string) = code[0 .. (code.Length - 2)] |> int

// "<" |> directionKeypad2
// "029A"
// |> numericKeypad2

let calcCodeComplexity code =
    let minLength = findShortestSequence code |> List.length
    let codeNumeric = codeToNumeric code
    minLength * codeNumeric

[ "029A" ] |> List.map calcCodeComplexity

input |> Array.map numericKeypad2
// example |> Array.map calcCodeComplexity |> Array.sum


input |> Array.map calcCodeComplexity |> Array.sum |> sprintf "Day 21a : %i" //184180
