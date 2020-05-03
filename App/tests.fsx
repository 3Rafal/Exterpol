#r @"./bin/Debug/netcoreapp3.1/FParsecCS.dll"
#r @"./bin/Debug/netcoreapp3.1/FParsec.dll"
#load "Domain.fs"
#load "Parsing.fs"
#load "Validation.fs"
#load "DataGeneration.fs"

open App
open RowParser
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

open System.IO

let numCsv start count =
    let header = "SplitId,ParentId,A701001(string(20)),A701002(int),A701003(datetime),Rate(Q2014-2015)"
    let csv = header
              :: [for i in start..(start + count - 1) -> sprintf "10: *,%d,*,*,*,10" i]
              |> String.concat System.Environment.NewLine 
    File.WriteAllText (@"App/data.txt", csv)
    
    
