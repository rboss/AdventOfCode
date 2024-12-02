let input =
    "./day2_input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (fun row -> row.Split(' ') |> Array.map int)

let ascCond = fun x -> x > 0 && x <= 3
let descCond = fun x -> x < 0 && x >= -3

let isRowSafe (levels: int array) =
    let condition = if levels[0] < levels[1] then ascCond else descCond

    levels |> Array.windowed 2 |> Array.forall (fun [| l; r |] -> condition (r - l))

input |> Array.filter isRowSafe |> Array.length |> printfn "Day 2a : %i"

let isRowSafeVariants (levels: int array) =
    seq {
        for i in [ 0 .. levels.Length - 1 ] do
            yield Array.removeAt i levels
    }
    |> Seq.exists isRowSafe


input |> Array.filter isRowSafeVariants |> Array.length |> printfn "Day 2b : %i"
