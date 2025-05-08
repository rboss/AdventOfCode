let example = "./input/day22_example.txt" |> System.IO.File.ReadAllLines
let input = "./input/day22_input.txt" |> System.IO.File.ReadAllLines

let process f secret =
    secret |> f |> (fun m -> secret ^^^ m) |> (fun m -> m % 16777216UL)

let nextSecret =
    let p1 = process ((*) 64UL)
    let p2 = process (fun m -> m / 32UL)
    let p3 = process ((*) 2048UL)

    p1 >> p2 >> p3

let secretGenerator secret =
    Seq.unfold (fun sec -> Some(nextSecret sec, nextSecret sec)) secret

nextSecret 123UL |> nextSecret
nextSecret 123UL |> nextSecret |> nextSecret
nextSecret 16495136UL
secretGenerator 123UL |> Seq.skip 9 |> Seq.take 1

example
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 1999 >> Seq.head)


input
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 1999 >> Seq.head)
|> Seq.sum
