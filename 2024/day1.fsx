let input =
    "./day1_input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line -> line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun [| l; r |] -> int l, int r)
    |> Array.unzip

input
||> (fun l r -> Array.sort l, Array.sort r)
||> Array.zip
|> Array.map (fun (l, r) -> System.Math.Abs(l - r))
|> Array.sum
|> printfn "Day 1a : %i"


input
||> (fun arrL arrR ->
    let countMap = arrR |> Array.countBy id |> Map.ofArray

    let getCount v =
        Map.tryFind v countMap |> Option.defaultValue 0

    arrL |> Array.map (fun v -> getCount v * v))
|> Array.sum
|> printfn "Day 1b : %i"
