module ExpressionTests

open FSharp.Quotations
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
    ]