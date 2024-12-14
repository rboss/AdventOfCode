let input =
    "6571 0 5851763 526746 23 69822 9 989".Split(' ')
    |> Array.map int64
    |> Array.toList

let splitNumber num =
    let str = string num
    let len = str.Length
    int64 str[0 .. len / 2 - 1], int64 str[len / 2 ..]

let blinkCached =
    let dict = System.Collections.Generic.Dictionary<_, _>()

    let rec blink iteration stone =
        match dict.TryGetValue((iteration, stone)) with
        | true, value -> value
        | false, _ ->
            let value =
                if iteration = 0 then
                    1L
                else
                    match stone with
                    | 0L -> blink (iteration - 1) 1L
                    | x when (string x).Length % 2 = 0 ->
                        let (l, r) = splitNumber x
                        blink (iteration - 1) l + blink (iteration - 1) r
                    | x -> blink (iteration - 1) (x * 2024L)

            dict.Add((iteration, stone), value)
            value

    blink

input |> List.map (blinkCached 25) |> List.sum |> printfn "Day 11a : %i"

input |> List.map (blinkCached 75) |> List.sum |> printfn "Day 11b : %i"
