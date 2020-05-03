open System.IO

open App
open FileParser
open Validator
open CSVGenerator

let generate txt = txt |> parseData |> validate |> createCSV
    
[<EntryPoint>]
let main argv =
    let csv = File.ReadAllText @"..\..\..\data.txt"
              |> generate
    File.WriteAllText (@"..\..\..\data.csv", csv)
    0
    

