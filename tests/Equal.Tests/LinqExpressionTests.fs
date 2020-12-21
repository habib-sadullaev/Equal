module QueryParserTests

open System
open System.Linq
open FSharp.Quotations
open Equal.Linq
open TypeShape.Core.StagingExtensions
open Expecto
open System.Linq.Expressions
open Expect

#nowarn "49"

let quote(e: Expr<'T -> 'R>) = 
    Expr.cleanup e |> Linq.toLambdaExpression :?> Expression<Func<'T, 'R>>

let inline should compare (expected: QueryResult<'T>) str  =
    test (str |> String.map ^ function '.' -> '_' | c -> c) {
        let actual = Linq.CreateQuery<'T> str
        compare (string actual) (string expected) (sprintf "%A\n%A" actual expected)
    }

[<Tests>]
let tests =
    testList "linq expression parser" [
        testList "valid input" [
            "String Starts With '' and Int > 0 order by Int > 0 desc, String contains 'aaa'" 
            |> should equal { 
                Predicate = quote <@ fun Param_0 -> Param_0.String.StartsWith("") && Param_0.Int > 0 @>
                OrderBy  = [
                    { Selector = quote <@ fun Param_1 -> Param_1.Int > 0               @>; Ascending = false }
                    { Selector = quote <@ fun Param_2 -> Param_2.String.Contains "aaa" @>; Ascending = true  }
                ]
            }

            "TestArray Any (String Starts With '') and Int > 0"
            |> should equal {
                Predicate = quote <@ fun Param_0 -> Param_0.TestArray.Any(fun Param_1 -> Param_1.String.StartsWith("")) && Param_0.Int > 0 @>
                OrderBy = []
            }

            "order by Int ASC, Int DESC"
            |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.Int @>; Ascending = true  }
                    { Selector = quote <@ fun Param_2 -> Param_2.Int @>; Ascending = false }
                ]
            }

            "order by String desc, String asc"
            |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = false }
                    { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = true  }
                ]
            }

            "order by String, String desc, String asc, String"
            |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = true  }
                    { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = false }
                    { Selector = quote <@ fun Param_3 -> Param_3.String @>; Ascending = true  }
                    { Selector = quote <@ fun Param_4 -> Param_4.String @>; Ascending = true  }
                ]
            }
        ]
    ]