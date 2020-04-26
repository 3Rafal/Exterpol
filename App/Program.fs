open System.IO
open App
open Parser
open DataGenerator

[<EntryPoint>]
let main argv =
    let csv = File.ReadAllText @"C:/Users/Rafal.Gwozdzinski/Documents/data.txt"
              |> parseData
              |> createCSV
    File.WriteAllText (@"C:/Users/Rafal.Gwozdzinski/data.csv", csv)
    0
