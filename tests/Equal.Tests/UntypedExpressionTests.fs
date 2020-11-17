module UntypedExpressionTests

open FSharp.Quotations
open Expecto
open TypeShape.Core.StagingExtensions
open Equal.Expression

#nowarn "49"

let parser = mkLambdaUntyped typeof<TestRecord>

let inline parsedInto (expected: Expr<TestRecord -> 'a>) input =
    let name = sprintf "parses '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name {
        let expected = expected |> Expr.cleanup
        let actual = validInput parser input
        Expect.equal (string actual) (string expected) ^ sprintf "expected:\n%A\n  actual:\n%A\n" expected actual
    } |> testLabel "with valid input"

let inline failedWith expected input =
    let name = sprintf "fails parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input } |> testLabel "with invalid input"

[<Tests>]
let tests =
    testList "untyped expression parser" [
        "TestArray"             |> parsedInto <@ fun Param_0 -> Param_0.TestArray @>
        "TestArray.IsFixedSize" |> parsedInto <@ fun Param_0 -> Param_0.TestArray.IsFixedSize @>
        "String.Length"         |> parsedInto <@ fun Param_0 -> Param_0.String.Length @>
        
        "TestArray Any (String Starts With 'zzz') AND Int > 8 OR Int < 3 and String contains 'abc'"
        |> parsedInto <@ fun Param_0 -> 
            Array.exists (fun Param_1 -> Param_1.String.StartsWith "zzz") Param_0.TestArray && Param_0.Int > 8 || 
            Param_0.Int < 3 && Param_0.String.Contains "abc" @>

        ""                |> failedWith { position = 1L;  errors = ["("; "NOT"; "property of Core+TestRecord"] }
        "TestArray IS "   |> failedWith { position = 11L; errors = [ "ALL"; "ANY"; "IS EMPTY" ] }
        "String >"        |> failedWith { position = 8L;  errors = [ "CONTAINS"; "ENDS WITH"; "STARTS WITH" ] }
        "OptionalEnum &&" |> failedWith { position = 14L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"] }
        "HasValue &&"     |> failedWith { position = 10L; errors = ["AND"; "OR"; "end of input"] }
    ]
