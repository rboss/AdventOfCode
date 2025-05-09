let input = "./input/day22_input.txt" |> System.IO.File.ReadAllLines
let example2 = "./input/day22_example2.txt" |> System.IO.File.ReadAllLines

type PriceHistory =
    { Secret: uint64
      Price: int
      History: int; }

let process f secret =
    ((f secret) ^^^ secret) % 16777216UL

let nextSecret =
    let p1 = process (fun s -> s <<< 6)
    let p2 = process (fun s -> s >>> 5)
    let p3 = process (fun s -> s <<< 11)

    p1 >> p2 >> p3

let secretGenerator secret =
    Seq.unfold
        (fun
            { Secret = prevSecret
              Price = prevPrice
              History = prevHistory } ->
            let newSec = nextSecret prevSecret
            let newPrice = newSec % 10UL |> int
            let newHistory = (prevHistory <<< 5) ||| (newPrice + 9 - prevPrice) &&& 0xFFFFF

            let newHistory =
                { Secret = newSec
                  Price = newPrice
                  History = newHistory }

            Some(newHistory, newHistory))
        { Secret = secret
          Price = 3
          History = 0 }

input
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 1999 >> Seq.head)
|> Seq.map (_.Secret)
|> Seq.sum
|> sprintf "Day 22a : %i"

input
|> Seq.map (uint64)
|> Seq.map (secretGenerator >> Seq.skip 3 >> Seq.take 1996)
|> Seq.map (Seq.groupBy _.History >> Seq.map (snd >> Seq.head))
|> Seq.collect id
|> Seq.groupBy (_.History)
|> Seq.maxBy (snd >> Seq.sumBy ( _.Price ))
|> (snd >> Seq.sumBy ( _.Price ))
|> sprintf "Day 22b : %i"
