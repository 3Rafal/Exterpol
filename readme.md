# Exterpol
Polish DSL for data generation

## How to use
1. Open `data.txt` file
2. First line sets schema. Write schema columns, separated with commas.
    Possible columns are:
    - SplitId
        - It has a format `SplitId`
        - It has a type of int/long
        - Identifies a row
    - ParentId
        - It has a format `ParentId`
        - It has a type of int/long
        - Identifies a parent row
    - Attribute 
        - It has a format `A{6 digit id}({dataType})`
        - Possible data types are:
            - string: `string({max characters})`
            - int: `int`
            - numeric: `numeric`
            - date time: `datetime`
        - Example: `A761002(string(20))` is a string Attribute 
        with max length of 20 chars
    - Rate 
        - It has a format `Rate({FrequencyPrefix}{StartYear}-{EndYear})`
        - Possible frequency prefixes are:
            - `Yr` - Yearly (incorrectly called Annual)
            - `Q` - Quarterly
            - `M` - Monthly
        - It has a type of numeric/float
        - Example: `Rate(M2015-2020)` is a monthly Rate range beginning in 2015
        and ending in 2020
    - Volume
        - It has similar format as Rate: `Volume({FrequencyPrefix}{StartYear}-{EndYear})`
        - It has the same prefixes as Rate
        - It has a type of int/long
        - Example: `Volume(Yr2010-2011)` is a yearly Volume range beginning in
        2010 and ending in 2011
 
    Example header looks like:
    `SplitId,A127001(int),A127002(string(20)),Rate(Q2010-2029)`
 
 3. Next lines describes rows to generate. Possible rows are:
    - Generate random: `{numberOfRows}: **`. For example `10: **` will generate 10 random rows.
    - Generate random with ParentId set: `{numberOfRows}: **({parentId})`. 
    For example `110: **(345)` will generate 110 random rows with parentId set to 345
    - Generate concrete row: `123,abc,05/03/2020`
    - Generate concrete row with some random values: `123,*,abcdef,1.10,12/12/1999`
 
 4. Execute
 5. Open `data.csv` file and see generated data



