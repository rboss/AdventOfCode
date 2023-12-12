#load "Shared.fsx"

open Shared

let exampleInput =
    [| "seeds: 79 14 55 13"
       ""
       "seed-to-soil map:"
       "50 98 2"
       "52 50 48"
       ""
       "soil-to-fertilizer map:"
       "0 15 37"
       "37 52 2"
       "39 0 15"
       ""
       "fertilizer-to-water map:"
       "49 53 8"
       "0 11 42"
       "42 0 7"
       "57 7 4"
       ""
       "water-to-light map:"
       "88 18 7"
       "18 25 70"
       ""
       "light-to-temperature map:"
       "45 77 23"
       "81 45 19"
       "68 64 13"
       ""
       "temperature-to-humidity map:"
       "0 69 1"
       "1 0 69"
       ""
       "humidity-to-location map:"
       "60 56 37"
       "56 93 4" |]

let input = System.IO.File.ReadAllText "day5_input.txt"

open Parser

type Alamanac =
    { seeds: Seeds
      seedToSoil: ConvertionMap
      soilToFertilizer: ConvertionMap
      fertilizerToWater: ConvertionMap
      waterToLight: ConvertionMap
      lightToTemperature: ConvertionMap
      temperatureToHumidity: ConvertionMap
      humidityToLocation: ConvertionMap }

and Seeds = int64 list
and ConvertionMap = int64 list list

let pnewline = pstring System.Environment.NewLine
let pnumbers = sepBy1 pint64 whitespaceChar
let pSeeds = pstring "seeds: " >>. pnumbers .>> pnewline .>> pnewline

let mappingParser title =
    pstring title >>. pnewline >>. sepBy1 pnumbers pnewline
    .>> pnewline
    .>> pnewline

let pSeedsToSoil = mappingParser "seed-to-soil map:"
let pSoilToFertilizer = mappingParser "soil-to-fertilizer map:"
let pFertilizerToWater = mappingParser "fertilizer-to-water map:"
let pWaterToLight = mappingParser "water-to-light map:"
let pLightToTemperature = mappingParser "light-to-temperature map:"
let pTemperatureToHumidity = mappingParser "temperature-to-humidity map:"

let pHumidityToLocation =
    pstring "humidity-to-location map:" >>. pnewline >>. sepBy1 pnumbers pnewline

let pAlmanac =
    pSeeds
    .>>. pSeedsToSoil
    .>>. pSoilToFertilizer
    .>>. pFertilizerToWater
    .>>. pWaterToLight
    .>>. pLightToTemperature
    .>>. pTemperatureToHumidity
    .>>. pHumidityToLocation
    |>> (fun (((((((seeds, toSoil), toFertilizer), toWater), toLight), toTemperature), toHumidity), toLocation) ->
        { seeds = seeds
          seedToSoil = toSoil
          soilToFertilizer = toFertilizer
          fertilizerToWater = toWater
          waterToLight = toLight
          lightToTemperature = toTemperature
          temperatureToHumidity = toHumidity
          humidityToLocation = toLocation })

let defaultValue def opt = defaultArg opt def

let applyConvertion (convertionMap: ConvertionMap) number =
    convertionMap
    |> List.tryFind (fun [ _; source; steps ] -> number >= source && number < source + steps)
    |> Option.map (fun [ destination; source; _ ] -> number + (destination - source))
    |> defaultValue number

let calculateLocation almanac seed =
    seed
    |> applyConvertion almanac.seedToSoil
    |> applyConvertion almanac.soilToFertilizer
    |> applyConvertion almanac.fertilizerToWater
    |> applyConvertion almanac.waterToLight
    |> applyConvertion almanac.lightToTemperature
    |> applyConvertion almanac.temperatureToHumidity
    |> applyConvertion almanac.humidityToLocation

let problem1 data =
    let almanac =
        match Parser.run pAlmanac input with
        | Success(result, rest) -> result
        | Failure f -> failwith "failed parsing almanac %s" f

    almanac.seeds |> List.map (calculateLocation almanac) |> List.min

problem1 exampleInput |> tracePrint "Day5 1.ex: %i"
problem1 input |> tracePrint "Day5 1.1: %i"
