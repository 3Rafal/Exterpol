#r @"./bin/Debug/netcoreapp3.1/FParsecCS.dll"
#r @"./bin/Debug/netcoreapp3.1/FParsec.dll"
#load "Parsing.fs"
#load "DataGeneration.fs"

open App
open Parser
open DataGenerator
open FParsec


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
type t =
    | Int of int
    | Float of float
let parseInt : Parser<t,unit> =
    pchar '(' >>. pint32 |>> Int .>> pchar ')'

let parseFloat : Parser<t,unit> =
    pchar '(' >>. pfloat |>> Float .>> pchar ')'

let tes = (attempt parseInt) <|> parseFloat