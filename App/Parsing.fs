module App.Parser

open System
open FParsec

// COlumns
type AttributeType =
    | String of length : int
    | Integer
    | Numeric
    | DateTime

type Attribute = {
    name : string
    dataType : AttributeType
}

type Frequency =
    | Annual
    | Quarterly
    | Monthly
    
type Range = { start : int; finish : int}
type Volume = {
    frequency : Frequency
    range : Range
}
type Rate = {
    frequency : Frequency
    range : Range
}
type Column =
    | SplitId
    | ParentId
    | Attribute of Attribute
    | Volume of Volume
    | Rate of Rate
    
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
    
//////////
// Rows //
//////////

type CellValue =
    | String of string
    | Integer of int
    | DateTime of DateTime
    | Numeric of float
    | Random
 
 type RowCells =
     | CellValues of CellValue list
     | Random
     
type NewRow = {
    count : int
    cells : RowCells
}
type ExistingRow = RowCells

type Row =
    | NewRow of NewRow
    | ExistingRow of ExistingRow

let commaSpaceOrNewline : Parser<char,unit> = pchar ' ' <|> pchar ',' <|> pchar '\n'

let parseNewRowCount : Parser<int,unit> =
    pint32
    .>> skipString ":"
    .>> spaces

let parseRandomCells : Parser<RowCells,unit> =
    pstring "**" >>= (fun _ -> preturn Random)
    
let parseDateTime : Parser<DateTime,unit> =
    ((many1Chars (noneOf ", \t")) >>= (fun s -> match DateTime.TryParse s with
                                                    | true,result -> preturn result
                                                    | false,_ -> fail "")) // TODO: fail msg?
    
let commaOrTab : Parser<unit,unit> = skipAnyOf ",\t"
let backtrackingSepBy1 p sep = pipe2 p (many (sep >>? p)) (fun hd tl -> hd::tl)
let parseCell : Parser<CellValue,unit> =
    (attempt (pint32 |>> Integer .>> followedBy (spaces1 <|> skipChar ',' )))
    <|> (attempt (pfloat |>> Numeric))
    <|> (attempt (parseDateTime |>> DateTime))
    <|> (attempt (pchar '*' >>= (fun _ -> preturn CellValue.Random)))
    <|> ((many1Chars (noneOf ",\t")) |>> String)

let parseCells : Parser<RowCells,unit> =
    spaces >>. 
    ((parseRandomCells)
     <|> ((sepBy1 parseCell commaOrTab) |>> CellValues))
    .>> spaces
    
let parseNewRow : Parser<Row,unit> =
    pipe2 parseNewRowCount parseCells (fun c v -> Row.NewRow {count = c; cells = v})
    
let parseRow : Parser<Row,unit> = parseCells |>> ExistingRow .>> spaces

let parseRows : Parser<Row list,unit> = many ((attempt parseNewRow) <|> parseRow)
    
type Data = {
    columns : Column list
    rows : Row list
}

let pData =
    pipe2 parseColumns parseRows (fun c r -> ({columns = c; rows = r}))

let parseData str =
    match run pData str with
    | Success(data, _, _) -> data
    | Failure(errorMsg, _, _) -> failwith errorMsg
