module App.Parser

open System
open FParsec

///////
// Columns
//////

type Frequency =
    | Annual
    | Quarterly
    | Monthly
    
type Range = { start : int; finish : int}

type AttributeType =
    | String of length : int
    | Integer
    | Numeric
    | DateTime

type Attribute = {
    name : string
    dataType : AttributeType
} 
type Column =
    | SplitId 
    | Attribute of Attribute
    | Volume of frequency : Frequency * range : Range

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
let parseVolumeRange : Parser<Range,unit> =
    pipe2 (pint32 .>> skipString "-") pint32 (fun s f -> {start = s; finish = f})
    
let parseVolume : Parser<Column,unit> =
    skipStringCI "volume("
    >>. pipe2 parseVolumeFrequency parseVolumeRange (fun f r -> Volume (f, r))
    .>> skipStringCI")"
    
let parseColumn : Parser<Column,unit> =
    (stringCIReturn "splitId" Column.SplitId)
    <|> parseAttribute 
    <|> (parseVolume)

let parseHeader header : Parser<unit,unit> =
    spaces
    >>. skipStringCI header 
    .>> spaces
    
let commaWithSpaces : Parser<char,unit> =
    many (pchar ' ' ) >>. pchar ',' .>> many (pchar ' ')
    
let parseColumns : Parser<Column list,unit> =
    (parseHeader "#columns")
    >>. sepBy1 parseColumn commaWithSpaces 
    .>> spaces
    
//////////
// Rows //
//////////
type VolumeCell = 
    | Exact of int
    | Random of min : int * max : int
    
type AttributeCellValue =
    | Integer of int
    | String of string
    | Numeric of float
    | DateTime of System.DateTime

type AttributeCell = {
    name : string
    value : AttributeCellValue
}

type SplitId = int 
    
type NewRowCell =
    | NewRowAttribute of AttributeCell
    | NewRowVolume of VolumeCell

type RowCell =
    | RowSplitId of int
    | RowAttribute of AttributeCell
    | RowVolume of VolumeCell
type Row = RowCell list
type NewRowCount = int
type NewRow = {
    count : NewRowCount
    cells : NewRowCell list
}

let parseRandom : Parser<VolumeCell, unit> =
    pipe2
        (skipStringCI "(" >>. pint32 .>> skipStringCI "-")
        (pint32 .>> skipStringCI ")")
        (fun x y -> Random (x, y))
        
let parseExact : Parser<VolumeCell,unit> =
    pchar '(' >>. pint32 |>> Exact .>> pchar ')'

// TODO: must be a better way        
let parseRowVolume : Parser<VolumeCell,unit> =
    (attempt (skipStringCI "volume" >>. parseExact))
    <|> (skipStringCI "volume" >>. parseRandom)
    
let parseDateTime : Parser<DateTime,unit> =
    pchar '('
    >>. ((many1Chars (noneOf ")")) >>= (fun s -> match DateTime.TryParse s with
                                                    | true,result -> preturn result
                                                    | false,_ -> fail "")) // TODO: fail msg?
    .>> pchar ')'
let parseAttributeCellValue : Parser<AttributeCellValue,unit> =
    (attempt (pchar '(' >>. pint32 |>> Integer .>> pchar ')'))
    <|> (attempt (pchar '(' >>. pfloat |>> Numeric .>> pchar ')'))
    <|> (attempt (parseDateTime |>> DateTime))
    <|> (pstring "(\"" >>. (manyChars (noneOf"\"")) |>> String .>> pstring "\")")

let attributeName (char : char, id) =
    (string char) + id
let parseAttributeCell : Parser<AttributeCell,unit> =
    pipe2
        ((satisfy (fun x -> x = 'A')) .>>. (manyChars (noneOf "(")))
        (parseAttributeCellValue)
        (fun x y -> {name = (attributeName x); value = y} )

let parseNewRowCell : Parser<NewRowCell,unit> =
    (parseAttributeCell |>> NewRowAttribute)
    <|> (parseRowVolume |>> NewRowVolume)
    
let parseNewRowCells : Parser<NewRowCell list,unit> =
    many (skipString " ")
    >>. sepBy parseNewRowCell commaWithSpaces 

let parseNewRowCount : Parser<NewRowCount,unit> =
    pint32
    .>> skipString ":"
    .>> spaces

let parseNewRow : Parser<NewRow,unit> =
    pipe2 parseNewRowCount parseNewRowCells (fun c v -> {count = c; cells = v})

let parseNewRows : Parser<NewRow list,unit> =
    parseHeader "#new_rows"
    >>. many (parseNewRow .>> spaces)
    
let parseSplitId : Parser<SplitId,unit> =
    skipStringCI "splitId"
    >>. (pint32 |> skipParens)

let parseRowCell : Parser<RowCell,unit> =
    (parseAttributeCell |>> RowAttribute)
    <|> (parseRowVolume |>> RowVolume)
    <|> (parseSplitId |>> RowSplitId)
    
let parseRow : Parser<Row,unit> =
    (sepBy1 parseRowCell commaWithSpaces) .>> spaces

let parseRows : Parser<Row list,unit> =
    parseHeader "#rows"
    >>. many (parseRow)
    
type Data = {
    columns : Column list
    newRows : NewRow list Option
    rows : Row list Option
}

let pData =
    pipe3 parseColumns (opt parseNewRows) (opt parseRows) (fun c n r -> ({columns = c; newRows = n; rows = r}))

let parseData str =
    match run pData str with
    | Success(data, _, _) -> data
    | Failure(errorMsg, _, _) -> failwith errorMsg
