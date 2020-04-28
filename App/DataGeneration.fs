namespace App

open System
module HeaderGenerator =
    let annualRange range suffix =
        [for x in range.start .. range.finish -> sprintf "Y%d_%s" x suffix]

    let quarterlyOrMonthlyRange prefix range suffix =
        [for y in range.start .. range.finish do
         for q in 1 .. 4
         -> sprintf "%s%d_%d_%s" prefix q y suffix]
    let quarterlyRange = quarterlyOrMonthlyRange "Q"
    let monthlyRange = quarterlyOrMonthlyRange "M"

    let rangeNames dateRange suffix =
        match dateRange.frequency with
        | Annual -> annualRange dateRange.range suffix
        | Quarterly -> quarterlyRange dateRange.range suffix
        | Monthly -> monthlyRange dateRange.range suffix
        
    let columnNames column =
        match column with
        | SplitId -> ["SplitId"]
        | ParentId -> ["ParentId"]
        | Attribute a -> [a.name]
        | Volume r -> rangeNames r "volume"
        | Rate r -> rangeNames r "rate"
        
    let header data =
        data.columns
        |> List.map columnNames
        |> List.concat
        |> String.concat ","

module RowsGenerator =
    let volumesInYear freq =
        match freq with
        | Annual -> 1
        | Quarterly -> 4
        | Monthly -> 12
        
    let randomString n : string = 
        let r = System.Random()
        let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
        let sz = Array.length chars in
        String(Array.init n (fun _ -> chars.[r.Next sz]))

    let dateTimeString (dateTime : System.DateTime) = dateTime.ToString("MM/dd/yyyy")
    let randomAttribute attributeType =
        match attributeType with
        | AttributeType.String len -> randomString len
        | AttributeType.Integer -> System.Random().Next() |> string
        | AttributeType.DateTime -> System.DateTime.Now |> dateTimeString
        | AttributeType.Numeric -> System.Random().NextDouble()
                                   |> (*) 1000000.0
                                   |> fun x -> Math.Round(x, 6)
                                   |> string
            
    let rangeCount dateRange =
        let years = dateRange.range.finish - dateRange.range.start + 1
        years * (volumesInYear dateRange.frequency)
        
    let multiplyRange dateRange value =
        let count = rangeCount dateRange
        seq { for _ in 1..count -> value}
        |> String.concat ","
    let randomColumnValue column =
        match column with
        | Column.SplitId -> 0 |> string
        | ParentId -> 0 |> string
        | Column.Attribute a -> randomAttribute a.dataType
        | Column.Volume v -> System.Random().Next() |> string |> multiplyRange v 
        | Column.Rate r -> 0.0 |> string |> multiplyRange r 

    let valueToString column value =
        match value with
        | Integer i -> match column with
                       | Volume v -> i |> string |> multiplyRange v
                       | Rate r -> i |> string |> multiplyRange r
                       | _ -> i |> string
        | Numeric n -> match column with
                       | Volume v -> n |> int |> string |> multiplyRange v
                       | Rate r -> n |> string |> multiplyRange r
                       | _ -> n |> string
        | String s -> s
        | DateTime d -> d |> dateTimeString
        | CellValue.Random -> randomColumnValue column

    let randomRootColumnStr column = valueToString column CellValue.Random
    let randomNonRootColumnStr id column =
        match column with
        | ParentId -> id |> string
        | _ -> randomRootColumnStr column
    let columnStr (column, cellValue) = valueToString column cellValue  
    let existingRow columns row =
        let randomRoot = columns |> List.map randomRootColumnStr
        let randomNonRoot id = columns |> List.map (randomNonRootColumnStr id)
        let cells c = List.zip columns c |> List.map columnStr 
        match row with
        | RandomRoot -> randomRoot
        | RandomNonRoot id -> randomNonRoot id
        | CellValues v -> cells v
        |> String.concat ","
        
    
    let newRowSplitId = 0
    let rootParentId = 0
    let newRandomRoot columns =
        let newRandomColumn column =
            match column with
            | SplitId -> newRowSplitId |> string
            | ParentId -> rootParentId |> string
            | _ -> randomRootColumnStr column
            
        columns |> List.map newRandomColumn 
    
    let newRandomNonRoot parentId columns =
        let newRandomColumn column =
            match column with
            | SplitId -> newRowSplitId |> string
            | ParentId -> parentId |> string
            | _ -> randomRootColumnStr column
        
        columns |> List.map newRandomColumn
        
    let newRow columns row =
        let newRowCell (column, value) =
            match column with
            | SplitId -> newRowSplitId |> string
            | _ -> valueToString column value
            
        match row with
        | RandomRoot -> newRandomRoot columns
        | RandomNonRoot id -> newRandomNonRoot id columns
        | CellValues v -> List.zip columns v |> List.map newRowCell
        |> String.concat ","
    
    let rowStrings data row =
        match row with
        | NewRow n -> [for _ in 1..n.count -> (newRow data.columns n.cells)]
        | ExistingRow e -> [existingRow data.columns e]
        
    let rowString data =
        data.rows
        |> List.map (rowStrings data)
        |> List.concat

open HeaderGenerator
open RowsGenerator
module CSVGenerator = 
    let createCSV data = (header data) :: (rowString data) |> String.concat "\n"
