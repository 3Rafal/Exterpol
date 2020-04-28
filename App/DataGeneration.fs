module App.DataGenerator

open System
open Parser

let annualVolumes range =
    [for x in range.start .. range.finish -> sprintf "Y%d_volume" x]

let quarterlyVolumes range =
    [for y in range.start .. range.finish do
     for q in 1 .. 4
     -> sprintf "Q%d_%d_volume" q y]

let monthlyVolumes range =
    [for y in range.start .. range.finish do
     for m in 1 .. 12 
     -> sprintf "M%d_%d_volume" m y]

let volumeNames volume =
    match volume.frequency with
    | Annual -> annualVolumes volume.range
    | Quarterly -> quarterlyVolumes volume.range
    | Monthly -> monthlyVolumes volume.range
    
let columnNames column =
    match column with
    | Column.SplitId -> ["SplitId"]
    | Column.Attribute a -> [a.name]
    | Column.Volume v -> volumeNames v 
    
let header data =
    data.columns
    |> List.map columnNames
    |> List.concat
    |> String.concat ","

let volumesInYear freq =
    match freq with
    | Annual -> 1
    | Quarterly -> 4
    | Monthly -> 12
    
let randomString n : string = 
    let r = System.Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    new String(Array.init n (fun _ -> chars.[r.Next sz]))

let dateTimeString (dateTime : System.DateTime) = dateTime.ToString("MM/dd/yyyy")
let randomAttribute attributeType =
    match attributeType with
    | AttributeType.String len -> randomString len
    | AttributeType.Integer -> System.Random().Next() |> string
    | AttributeType.Numeric -> System.Random().NextDouble() |> (*) 1000000.0 |> fun x -> Math.Round(x, 6) |> string
    | AttributeType.DateTime -> System.DateTime.Now |> dateTimeString
        
let volumeCount volume =
    let years = volume.range.finish - volume.range.start
    years * (volumesInYear volume.frequency)
    
let multiplyVolume volume value =
    let count = volumeCount volume
    seq { for _ in 1..count -> value}
    |> String.concat ","
let randomColumnValue column =
    match column with
    | Column.SplitId -> 0 |> string
    | Column.Attribute a -> randomAttribute a.dataType
    | Column.Volume v -> System.Random().Next() |> string |> multiplyVolume v

let valueToString column value =
    match value with
    | Integer i -> i |> string
    | Numeric n -> n |> string
    | String s -> s
    | DateTime d -> d |> dateTimeString
    | CellValue.Random -> randomColumnValue column

let allRandom columns =
    columns |> List.map (fun x -> valueToString x CellValue.Random)
    
let row columns row =
    (match row with
    | Random -> allRandom columns
    | CellValues c -> List.zip columns c
                      |> List.map (fun (x,y)-> valueToString x y)) |> String.concat ","
    
let newRowSplitId = 0 |> string
let newRowAllRandom columns =
    columns
    |> List.map (fun x -> match x with
                          | SplitId -> newRowSplitId
                          | _ -> valueToString x CellValue.Random )
let newRow columns row =
    (match row with
    | Random -> newRowAllRandom columns
    | CellValues c -> List.zip columns c
                      |> List.map (fun (x,y)-> match x with
                                               | SplitId -> newRowSplitId
                                               | _ -> valueToString x y)) |> String.concat ","
    
let rowString data =
    data.rows
    |> List.map (fun x -> match x with
                          | NewRow n -> [for _ in 1..n.count -> (newRow data.columns n.cells)]
                          | ExistingRow e -> [row data.columns e])
    |> List.concat

let createCSV data = (header data) :: (rowString data) |> String.concat "\n"
