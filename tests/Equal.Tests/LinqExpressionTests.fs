module QueryParserTests

open System
open System.Linq
open FSharp.Quotations
open Equal.Linq
open TypeShape.Core.StagingExtensions
open Expecto
open System.Linq.Expressions

#nowarn "49"

let quote(e: Expr<'T -> 'R>) = 
    Expr.cleanup e |> Linq.toLambdaExpression :?> Expression<Func<'T, 'R>>

let private orderBy sortedBy pred = { Predicate = pred; OrderBy = sortedBy } 

[<Tests>]
let tests =
    testList "linq expression parser" [
        testList "valid input" [
            test "String Starts With '' and Int > 0 order by Int desc, String" {
                let actual = 
                        "String Starts With '' and Int > 0 order by Int desc, String"
                        |> Linq.CreateQuery<TestRecord> 
                let expected = 
                    quote <@ fun Param_0 -> Param_0.String.StartsWith("") && Param_0.Int > 0 @>
                    |> orderBy [ 
                        struct(upcast quote <@ fun Param_0 -> Param_0.Int @>, false)
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, true) 
                    ]
                
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }
        
            test "TestArray Any (String Starts With '') and Int > 0" {
                let actual = 
                    "TestArray Any (String Starts With '') and Int > 0"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> Param_0.TestArray.Any(fun Param_1 -> Param_1.String.StartsWith("")) && Param_0.Int > 0 @>
                    |> orderBy []
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }

            test "Int > 0 order by Int desc, Int asc" {
                let actual = 
                    "Int > 0 order by Int ASC, Int DESC"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> Param_0.Int > 0 @>
                    |> orderBy [
                        struct(upcast quote <@ fun Param_0 -> Param_0.Int @>, true)
                        struct(upcast quote <@ fun Param_0 -> Param_0.Int @>, false)
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }

            test "Int > 0 order by String desc, String asc" {
                let actual = 
                    "Int > 0 order by String desc, String asc"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> Param_0.Int > 0 @>
                    |> orderBy [
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, false)
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, true)
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }

            test "Int > 0 order by String, String desc, String asc, String" {
                let actual = 
                    "Int > 0 order by String, String desc, String asc, String"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> Param_0.Int > 0 @>
                    |> orderBy [
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, true)
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, false)
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, true)
                        struct(upcast quote <@ fun Param_0 -> Param_0.String @>, true)
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }
        ]
    ]