let input = "./day7_input.txt" |> System.IO.File.ReadAllLines

let isSet x i = x &&& (1 <<< i) <> 0

let tryFindEquation (answer, operands) =
    let maxIterations = int (2.0 ** (Array.length operands)) - 1

    seq { 0..maxIterations }
    |> Seq.map (fun iteration ->

        let fold =
            fun (index, acc) v -> index + 1, (if (isSet iteration index) then (*) else (+)) acc v

        operands |> Array.fold fold (0, 0L))
    |> Seq.map snd
    |> Seq.tryFind ((=) answer)

#time

input
|> Array.map (fun row -> row.Split(": "))
|> Array.map (fun [| answerStr; operandsStr |] -> (int64) answerStr, operandsStr.Split(' ') |> Array.map int64)
|> Array.choose tryFindEquation
|> Array.sum

let rec tryFindEquationRec (goal, operands) =
    let rec calc =
        function
        | [] -> None
        | calculation :: rest ->
            match calculation with
            | total, [] when total = goal -> Some total
            | _, [] -> calc rest
            | total, v :: tail -> calc ([ (total + v, tail); (total * v, tail); (int64 $"{total}{v}", tail) ] @ rest)

    calc [ (0, operands) ]

input
|> Array.map (fun row -> row.Split(": "))
|> Array.map (fun [| answerStr; operandsStr |] ->
    (int64) answerStr, operandsStr.Split(' ') |> Array.map int64 |> Array.toList)
|> Array.Parallel.choose tryFindEquationRec
|> Array.sum
