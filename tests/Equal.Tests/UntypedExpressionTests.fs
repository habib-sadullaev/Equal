module UntypedExpressionTests

open FSharp.Quotations
open Expecto
open TypeShape.Core.StagingExtensions
open FParsec
open Equal.Expression

#nowarn "49"

let parser = mkLambdaUntyped typeof<TestRecord>

let inline equal actual expected message =
    Expect.equal (string actual) (string expected) message

let inline should compare (expected: Expr<'T>) input =
    let name = sprintf "parses '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    let expected = expected
    test name { parsed (parser |>> Expr.cast<'T>) compare (Expr.cleanup expected) input } |> testLabel "with valid input"
    |> testLabel "with valid input"

let inline shouldFailWith expected input =
    let name = sprintf "fails parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input } |> testLabel "with invalid input"


[<Tests>]
let tests =
    let eof = "end of input"
    testList "untyped expression parser" [
        "TestArray"             |> should equal <@ fun Param_0 -> Param_0.TestArray @>
        "TestArray.IsFixedSize" |> should equal <@ fun Param_0 -> Param_0.TestArray.IsFixedSize @>
        "String.Length"         |> should equal <@ fun Param_0 -> Param_0.String.Length @>
        
        "TestArray Any (String Starts With 'zzz') AND Int > 8 OR Int < 3 and String contains 'abc'" |> should equal 
            <@ fun Param_0 -> 
                Array.exists (fun Param_1 -> Param_1.String.StartsWith "zzz") Param_0.TestArray && Param_0.Int > 8 || 
                Param_0.Int < 3 && Param_0.String.Contains "abc" @>

        ""                |> shouldFailWith { position = 1L;  errors = ["("; "NOT"; "property of Core+TestRecord"] }
        "TestArray IS "   |> shouldFailWith { position = 11L; errors = ["ALL"; "ANY"; "IS EMPTY"; eof] }
        "String >"        |> shouldFailWith { position =  8L; errors = ["CONTAINS"; "ENDS WITH"; "STARTS WITH"; eof] }
        "OptionalEnum &&" |> shouldFailWith { position = 14L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"; eof] }
        "HasValue &&"     |> shouldFailWith { position = 10L; errors = ["AND"; "OR"; eof] }
        "(Int > 0"        |> shouldFailWith { position =  9L; errors = [")"; "AND"; "OR"] }
    ]
