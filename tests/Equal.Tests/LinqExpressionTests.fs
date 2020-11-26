module QueryParserTests

open System
open System.Linq
open FSharp.Quotations
open FSharp.Linq.RuntimeHelpers
open System.Linq.Expressions
open Equal.Linq
open TypeShape.Core.StagingExtensions
open Expecto

#nowarn "49"

let quote(e: Expr<'T -> 'R>) = 
    e |> Expr.cleanup |> Linq.toLambdaExpression :?> LambdaExpression

[<Tests>]
let tests =
    testList "linq expression parser" [
        test "String Starts With '' and Int > 0 order by Int desc, String" {
            let actual = Expression.CreateQuery<TestRecord> "String Starts With '' and Int > 0 order by Int desc, String"
            let expected = 
                { Predicate = quote <@ fun Param_0 -> Param_0.String.StartsWith("") && Param_0.Int > 0 @> :?> Predicate<TestRecord>

                  Order =   
                    [ struct(quote <@ fun Param_0 -> Param_0.Int @>, false)
                      struct(quote <@ fun Param_0 -> Param_0.String @>, true) ] }
            
            Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
        } |> testLabel "valid input"
    ]