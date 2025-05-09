let example = "./input/day22_example.txt" |> System.IO.File.ReadAllLines
let example2 = "./input/day22_example2.txt" |> System.IO.File.ReadAllLines
let input = "./input/day22_input.txt" |> System.IO.File.ReadAllLines

type PriceHistory =
    { Secret: uint64
      Price: int
      ChangeHistory: int list }

let process f secret =
    secret |> f |> (fun m -> secret ^^^ m) |> (fun m -> m % 16777216UL)

let nextSecret =
    let p1 = process ((*) 64UL)
    let p2 = process (fun m -> m / 32UL)
    let p3 = process ((*) 2048UL)

    p1 >> p2 >> p3

let secretGenerator secret =
    Seq.unfold
        (fun
            { Secret = prevSecret
              Price = prevPrice
              ChangeHistory = [ h1; h2; h3; _ ] } ->
            let newSec = nextSecret prevSecret
            let newPrice = newSec % 10UL |> int

            let newHistory =
                { Secret = newSec
                  Price = newPrice
                  ChangeHistory = (newPrice - prevPrice) :: [ h1; h2; h3 ] }

            Some(newHistory, newHistory))
        { Secret = secret
          Price = 3
          ChangeHistory = [ 0; 0; 0; 0 ] }

nextSecret 123UL |> nextSecret
nextSecret 123UL |> nextSecret |> nextSecret
nextSecret 16495136UL
secretGenerator 123UL |> Seq.skip 3 |> Seq.take 6 |> Seq.toList

example
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 1999 >> Seq.head)

input
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 1999 >> Seq.head)
|> Seq.map (_.Secret)
|> Seq.sum
|> sprintf "Day 22a : %i"


input
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 3 >> Seq.take 1996)
|> Seq.map (fun s -> s |> Seq.groupBy _.ChangeHistory |> Seq.map (snd >> Seq.head))
|> Seq.collect id
|> Seq.groupBy (_.ChangeHistory)
|> Seq.maxBy (fun (_, historySeq) -> historySeq |> Seq.sumBy ( _.Price ))
|> (fun (_, historySeq) -> historySeq |> Seq.sumBy ( _.Price ))
|> sprintf "Day 22b : %i"

