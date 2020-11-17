module ExpressionTests

open FSharp.Quotations
open Expecto
open FParsec
open TypeShape.Core.StagingExtensions
open Equal.Expression

#nowarn "49"

let parser = mkLambda<TestRecord -> bool> ()

let gatherParams (e: Expr) =
    let rec aux e acc =
        match e with
        | ExprShape.ShapeVar x -> 
            x :: acc
        | ExprShape.ShapeLambda (x, _) -> x :: acc
        | ExprShape.ShapeCombination (_, es) -> [ for e in es do yield! aux e acc ]
    aux e []

let inline parsedInto (expected: Expr<TestRecord -> bool>) input =
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
    testList "expression parser" [
        "Parent.Parent.Int <> 4"  |> parsedInto <@ fun Param_0 -> Param_0.Parent.Parent.Int <> 4 @>
        
        "TestArray Any(Int = 5)" |> parsedInto <@ fun Param_0 -> Array.exists (fun Param_1 -> Param_1.Int = 5) Param_0.TestArray @>
        "TestList Any(Int = 5)"  |> parsedInto <@ fun Param_0 -> List.exists (fun Param_1 -> Param_1.Int = 5) Param_0.TestList @>
        
        "TestArray all(Int = 5)" |> parsedInto <@ fun Param_0 -> Array.forall (fun Param_1 -> Param_1.Int = 5) Param_0.TestArray @>
        "TestList all(Int = 5)"  |> parsedInto <@ fun Param_0 -> List.forall (fun Param_1 -> Param_1.Int = 5) Param_0.TestList @>
        
        "TestArray is empty"      |> parsedInto <@ fun Param_0 -> Array.isEmpty Param_0.TestArray @> 
        "TestList is empty"       |> parsedInto <@ fun Param_0 -> List.isEmpty Param_0.TestList @> 
    ]