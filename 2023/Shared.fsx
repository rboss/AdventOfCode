
module Parser

open System

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

let satisfy predicate =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if predicate first then
                let remaining = str.[1..]
                Success (first, remaining)
            else
                let msg = sprintf "Unexpected '%c'" first
                Failure msg
    Parser innerFn

let pchar charToMatch =
  let predicate ch = charToMatch = ch
  satisfy predicate

let run parser input =
  // unwrap parser to get inner function
  let (Parser innerFn) = parser
  // call inner function with input
  innerFn input

let andThen parser1 parser2 =
    let innerFn input =
        // run parser1 with the input
        let result1 = run parser1 input

        // test the result for Failure/Success
        match result1 with
        | Failure err -> Failure err
        | Success (value1,remaining1) ->
            // run parser2 with the remaining input
            let result2 =  run parser2 remaining1

            // test the result for Failure/Success
            match result2 with
            | Failure err ->
                // return error from parser2
                Failure err

            | Success (value2,remaining2) ->
                // combine both values as a pair
                let newValue = (value1,value2)
                // return remaining input after parser2
                Success (newValue,remaining2)

    // return the inner function
    Parser innerFn

let orElse parser1 parser2 = 
    let innerFn input =
        let result1 = run parser1 input
        match result1 with 
        | Success _ -> result1
        | Failure err ->
            let result2 = run parser2 input
            result2
    Parser innerFn

let mapP f parser = 
    let innerFn input =
        let result = run parser input

        match result with 
        | Success (value, remaining) -> 
            let newValue = f value 
            Success(newValue, remaining)
        | Failure err -> Failure err

    Parser innerFn

let pint =
    // helper
    let resultToInt (sign,charList) =
        let i = charList |> List.toArray |> System.String |> int
        match sign with
        | Some ch -> -i  // negate the int
        | None -> i

    // define parser for one digit
    let digit = anyOf ['0'..'9']

    // define parser for one or more digits
    let digits = many1 digit

    // parse and convert
    opt (pchar '-') .>>. digits
    |>> resultToInt
