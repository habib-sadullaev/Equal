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

let [<Literal>] eof = "end of input"

[<Tests>]
let tests =
    testList "untyped expression parser" [
        "TestArray"             |> should equal <@ fun Param_0 -> Param_0.TestArray @>
        "TestArray.IsFixedSize" |> should equal <@ fun Param_0 -> Param_0.TestArray.IsFixedSize @>
        "String.Length"         |> should equal <@ fun Param_0 -> Param_0.String.Length @>
        
        "TestArray Any (String Starts With 'zzz') AND Int > 8 OR Int < 3 and String contains 'abc'" |> should equal 
            <@ fun Param_0 -> 
                Array.exists (fun Param_1 -> Param_1.String.StartsWith "zzz") Param_0.TestArray && Param_0.Int > 8 || 
                Param_0.Int < 3 && Param_0.String.Contains "abc" @>
        
        ""   |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        " "  |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "()" |> shouldFailWith { position = 2L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "z"  |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        
        "HasValue &&"       |> shouldFailWith { position = 10L; errors = ["AND"; "OR"; eof] }
        "OptionalEnum &&"   |> shouldFailWith { position = 14L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"; eof] }
        "String >"          |> shouldFailWith { position =  8L; errors = ["CONTAINS"; "ENDS WITH"; "STARTS WITH"; eof] }
        "TestArray IS "     |> shouldFailWith { position = 11L; errors = ["ALL"; "ANY"; "IS EMPTY"; eof] }

        "Int > 0 && String contains 'a'" |> shouldFailWith { position =  9L; errors = [ "AND"; "OR"; eof ] }
        "NOT Parent.Parent"              |> shouldFailWith { position =  5L; errors = [ "(" ] }
        "Parent.Parent.HasValue OR"      |> shouldFailWith { position = 26L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "OptionalEnum = one AND"         |> shouldFailWith { position = 23L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        
        "(HasValue AND Parent.Parent.Float = 1"           |> shouldFailWith { position = 38L; errors = [")"; "AND"; "OR"] }
        "(Int > 0"                                        |> shouldFailWith { position =  9L; errors = [")"; "AND"; "OR"] }
        "Not (HasValue"                                   |> shouldFailWith { position = 14L; errors = [")"; "AND"; "OR"] }
        "(String ends with ''"                            |> shouldFailWith { position = 21L; errors = [")"; "AND"; "OR"] }
        "((String ends with '')"                          |> shouldFailWith { position = 23L; errors = [")"; "AND"; "OR"] }
        "(Parent.Parent.String ends with ''"              |> shouldFailWith { position = 35L; errors = [")"; "AND"; "OR"] }
        "Int > 73 and (Parent.Parent.String ends with ''" |> shouldFailWith { position = 48L; errors = [")"; "AND"; "OR"] }

        
    ]
