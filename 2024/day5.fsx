let input = "./day5_input.txt" |> System.IO.File.ReadAllLines

let (pageOrdersRaw, pageUpdatesRaw) =
    let splitIndex = Array.findIndex (fun v -> v = "") input
    Array.splitAt splitIndex input

let orderSet = Set.ofArray pageOrdersRaw

let pageUpdates =
    pageUpdatesRaw |> Array.skip 1 |> Array.map (fun row -> row.Split(','))

let pageComparer p1 p2 =
    let key = sprintf "%s|%s" p1 p2
    if Set.contains key orderSet then -1 else 1

let isCorrectOrdering pages =
    let sorted = pages |> Array.sortWith pageComparer
    sorted = pages

pageUpdates
|> Array.filter isCorrectOrdering
|> Array.map (fun arr -> arr[arr.Length / 2])
|> Array.map int
|> Array.sum
|> printfn "Day 5a : %i"

pageUpdates
|> Array.filter (fun pages -> not (isCorrectOrdering pages))
|> Array.map (Array.sortWith pageComparer)
|> Array.map (fun arr -> arr[arr.Length / 2])
|> Array.map int
|> Array.sum
|> printfn "Day 5b : %i"
