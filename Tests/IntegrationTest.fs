module Tests

open System
open Xunit

open Program

[<Fact>]
let ``My test`` () =
    let input = @"SplitId,ParentId,A701001(string(20)), A701002(int), A701003(datetime), Rate(Y2014-2015)
2: *,103,test,55,1/1/1990,1
105,22,root03,3,7/1/2013,1000"

    let output = @"SplitId,ParentId,A701001,A701002,A701003,Y2014_rate,Y2015_rate
0,103,test,55,01/01/1990,1,1
0,103,test,55,01/01/1990,1,1
105,22,root03,3,07/01/2013,1000,1000".Replace("\r\n", "\n")

    let result =  input |> generate
    Assert.Equal(output, result)