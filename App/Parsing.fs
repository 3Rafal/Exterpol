namespace App

open System
open Columns
open FParsec

module ColumnParser =
    let skipParens x = skipString"(" >>. x .>> skipString ")"
    let parseAttributeType : Parser<AttributeType,unit> =
        (stringCIReturn "int" Integer)
        <|> (stringCIReturn "numeric" Numeric)
        <|> (skipString "string(" >>. pint32 >>= (fun x -> preturn (String x)) .>> skipString")")
        <|> (stringCIReturn "datetime" DateTime)
        |> skipParens
        
    let parseAttributeName : Parser<string,unit> = (skipStringCI "a" >>. pint32 >>= (fun x -> preturn (string ("A" + string x))))
        
    let parseAttribute : Parser<Column,unit> =
        pipe2 parseAttributeName parseAttributeType (fun n t -> Column.Attribute {name = n; dataType = t})

    let parseVolumeFrequency : Parser<Frequency,unit> =
        (stringCIReturn "Y" Annual)
        <|> (stringCIReturn "Q" Quarterly)
        <|> (stringCIReturn "M" Monthly)
    let parseRange : Parser<Range,unit> =
        pipe2 (pint32 .>> skipString "-") pint32 (fun s f -> {start = s; finish = f})
        
    let parseVolume : Parser<Column,unit> =
        skipStringCI "volume("
        >>. pipe2 parseVolumeFrequency parseRange (fun f r -> Volume {frequency=f; range=r})
        .>> skipStringCI")"

    let parseRate : Parser<Column,unit> =
        skipStringCI "rate("
        >>. pipe2 parseVolumeFrequency parseRange (fun f r -> Rate {frequency=f; range=r})
        .>> skipStringCI")"
    let parseColumn : Parser<Column,unit> =
        (stringCIReturn "splitId" SplitId)
        <|> (stringCIReturn "parentId" ParentId)
        <|> parseAttribute 
        <|> parseVolume
        <|> parseRate

    let parseHeader header : Parser<unit,unit> =
        skipStringCI header .>> spaces
    
    let commaOrSpaces : Parser<unit,unit> =
        skipMany1 (anyOf " ,")

    let parseColumns : Parser<Column list,unit> =
        sepBy1 parseColumn commaOrSpaces 
    .>> spaces
    
open Rows
module RowParser =
    let parseNewRowCount : Parser<int,unit> =
        pint32
        .>> skipString ":"
        .>> spaces

    let parseRandomRoot : Parser<RowCells,unit> =
        skipString "**" >>= (fun _ -> preturn RandomRoot)
    
    let parseRandomNonRoot : Parser<RowCells,unit> =
        skipString "**(" >>. pint32 >>= (fun i -> preturn (RandomNonRoot i)) .>> skipChar ')'
        
    let parseDateTime : Parser<DateTime,unit> =
        ((many1Chars (noneOf ", \t")) >>= (fun s -> match DateTime.TryParse s with
                                                        | true,result -> preturn result
                                                        | false,_ -> fail "")) // ignore fail message
        
    let commaOrTab : Parser<unit,unit> = skipAnyOf ",\t"
    let parseCell : Parser<CellValue,unit> =
        (attempt (pint32 |>> Integer .>> followedBy (spaces1 <|> skipChar ',' )))
        <|> (attempt (parseDateTime |>> DateTime))
        <|> (attempt (pfloat |>> Numeric))
        <|> (attempt (pchar '*' >>= (fun _ -> preturn CellValue.Random)))
        <|> ((many1Chars (noneOf ",\t")) |>> String)

    let parseCells : Parser<RowCells,unit> =
        spaces >>. 
        ((parseRandomNonRoot)
         <|>(parseRandomRoot)
         <|> ((sepBy1 parseCell commaOrTab) |>> CellValues))
        .>> spaces
        
    let parseNewRow : Parser<Row,unit> =
        pipe2 parseNewRowCount parseCells (fun c v -> Row.NewRow {count = c; cells = v})
        
    let parseRow : Parser<Row,unit> = parseCells |>> ExistingRow .>> spaces

    let parseRows : Parser<Row list,unit> = many ((attempt parseNewRow) <|> parseRow)
        

open ColumnParser
open RowParser
module FileParser =
    let pData =
        pipe2 parseColumns parseRows (fun c r -> ({columns = c; rows = r}))

    let parseData str =
        match run pData str with
        | Success(data, _, _) -> data
        | Failure(errorMsg, _, _) -> failwith errorMsg
