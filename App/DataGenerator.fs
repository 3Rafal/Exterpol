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
    
let volumeNames frequency range =
    match frequency with
    | Annual -> annualVolumes range
    | Quarterly -> quarterlyVolumes range
    | Monthly -> monthlyVolumes range
    
let columnNames column =
    match column with
    | Column.SplitId -> ["SplitId"]
    | Column.Attribute (a, _) -> [a]
    | Column.Volume (f,r) -> volumeNames f r
    
let header data =
    data.meta.columns
    |> List.map columnNames
    |> List.concat
    |> String.concat ","

let randomString n : string = 
    let r = new Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    new String(Array.init n (fun _ -> chars.[r.Next sz]))
    
let randomAttribute attributeType =
    match attributeType with
    | AttributeType.String len -> randomString len
    | AttributeType.Integer -> System.Random().Next() |> string
    | AttributeType.Numeric -> System.Random().NextDouble() |> string
        
let missingColumnValue column =
    match column with
    | Column.SplitId -> 0 |> string
    | Column.Attribute (_, dt) -> randomAttribute dt
    | Column.Volume (_,_) -> System.Random().Next() |> string
    
type RowCellKey =
    | SplitId
    | Attribute of string
    | Volume

let newCellsToMap cells =
    cells
    |> List.map (fun x -> match x with
                          | NewRowCell.NewRowAttribute a -> (Attribute a.name, a.value |> string)
                          | NewRowCell.NewRowVolume v -> (Volume, v |> string))
    |> Map.ofList
   
let cellsToMap cells =
    cells
    |> List.map (fun x -> match x with
                          | RowCell.RowAttribute a -> (Attribute a.name, a.value |> string)
                          | RowCell.RowVolume v -> (Volume, v |> string)
                          | RowCell.RowSplitId s -> (SplitId, s |> string))
    |> Map.ofList

let columnToKey column =
    match column with
    | Column.SplitId -> SplitId
    | Column.Attribute (name,_) -> Attribute name
    | Column.Volume (_,_) -> Volume
    
let newRow data row =
    let map = newCellsToMap row
    data.meta.columns
    |> List.map (fun c -> match map.TryFind (columnToKey c) with
                          | Some s -> s
                          | None -> missingColumnValue c)
    |> String.concat "," 
    
let row data row =
    let map = cellsToMap row
    data.meta.columns
    |> List.map (fun c -> match map.TryFind (columnToKey c) with
                          | Some s -> s
                          | None -> missingColumnValue c)
    |> String.concat ","
    
let newRows data =
    data.newRows
    |> List.map (fun x -> [ for _ in 1..x.count -> (newRow data x.cells)])
    |> List.concat
    
let rows data =
    data.rows
    |> List.map (fun x -> row data x)

let createCSV data =
    [header data]
    @ newRows data
    @ rows data
    |> String.concat "\n"
