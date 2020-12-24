module ExpressionTests

open Expecto
open FParsec
open TypeShape.Core.StagingExtensions
open Equal.Expression

#nowarn "49"

let parser = mkLambda<TestRecord>()

let equal actual expected message =
    Expect.equal (string actual) (string expected) message

let inline should compare expected input =
    let name = sprintf "parses '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    let expected = expected
    test name { parsed parser compare (Expr.cleanup expected) input } |> testLabel "with valid input"

let inline shouldFailWith expected input =
    let name = sprintf "fails parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input } |> testLabel "with invalid input"

let [<Literal>] eof = "end of input"

[<Tests>]
let tests =
    testList "expression parser" [
        "Int = 3"                |> should equal <@ fun Param_0 -> Param_0.Int = 3 @>
        "Parent.Int <> 4"        |> should equal <@ fun Param_0 -> Param_0.Parent.Int <> 4 @>
        "Parent.Parent.Int = 5"  |> should equal <@ fun Param_0 -> Param_0.Parent.Parent.Int = 5 @>
        
        "TestArray Any(Int = 6)" |> should equal <@ fun Param_0 -> Array.exists (fun Param_1 -> Param_1.Int = 6) Param_0.TestArray @>
        "TestList Any(Int = 7)"  |> should equal <@ fun Param_0 -> List.exists (fun Param_1 -> Param_1.Int = 7) Param_0.TestList @>
        
        "TestArray all(Int = 8)" |> should equal <@ fun Param_0 -> Array.forall (fun Param_1 -> Param_1.Int = 8) Param_0.TestArray @>
        "TestList all(Int = 9)"  |> should equal <@ fun Param_0 -> List.forall (fun Param_1 -> Param_1.Int = 9) Param_0.TestList @>
        
        "TestArray is empty"     |> should equal <@ fun Param_0 -> Array.isEmpty Param_0.TestArray @> 
        "TestList is empty"      |> should equal <@ fun Param_0 -> List.isEmpty Param_0.TestList @> 

        ""   |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        " "  |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "()" |> shouldFailWith { position = 2L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "z"  |> shouldFailWith { position = 1L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        
        "HasValue &&"         |> shouldFailWith { position = 10L; errors = ["AND"; "OR"; eof] }
        "Int > 0 andHasValue" |> shouldFailWith { position =  9L; errors = ["AND"; "OR"; "end of input"] }
        "Int > 0 orHasValue"  |> shouldFailWith { position =  9L; errors = ["AND"; "OR"; "end of input"] }
        "OptionalEnum &&"     |> shouldFailWith { position = 14L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"] }
        "String >"            |> shouldFailWith { position =  8L; errors = ["CONTAINS"; "ENDS WITH"; "STARTS WITH"] }
        "TestArray IS "       |> shouldFailWith { position = 11L; errors = ["ALL"; "ANY"; "IS EMPTY"] }
        
        "Int > 0 && String contains 'a'" |> shouldFailWith { position =  9L; errors = [ "AND"; "OR"; eof ] }
        "NOT Parent.Parent"              |> shouldFailWith { position =  5L; errors = [ "(" ] }
        "Parent.Parent.HasValue OR"      |> shouldFailWith { position = 26L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        "OptionalEnum = one AND"         |> shouldFailWith { position = 23L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
        
        "(HasValue AND Parent.Parent.Float = 1"           |> shouldFailWith { position = 38L; errors = [")"; "AND"; "OR"] }
        "Not (HasValue"                                   |> shouldFailWith { position = 14L; errors = [")"; "AND"; "OR"] }
        "(String ends with ''"                            |> shouldFailWith { position = 21L; errors = [")"; "AND"; "OR"] }
        "((String ends with '')"                          |> shouldFailWith { position = 23L; errors = [")"; "AND"; "OR"] }
        "(Parent.Parent.String ends with ''"              |> shouldFailWith { position = 35L; errors = [")"; "AND"; "OR"] }
        "Int > 73 and (Parent.Parent.String ends with ''" |> shouldFailWith { position = 48L; errors = [")"; "AND"; "OR"] }
    ]