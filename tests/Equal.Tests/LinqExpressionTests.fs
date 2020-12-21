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
            test "String Starts With '' and Int > 0 order by Int > 0 desc, String contains 'aaa'" {
                let actual = 
                        "String Starts With '' and Int > 0 order by Int > 0 desc, String contains 'aaa'"
                        |> Linq.CreateQuery<TestRecord> 
                let expected = 
                    quote <@ fun Param_0 -> Param_0.String.StartsWith("") && Param_0.Int > 0 @>
                    |> orderBy [ 
                        { Selector = quote <@ fun Param_1 -> Param_1.Int > 0               @>; Ascending = false }
                        { Selector = quote <@ fun Param_2 -> Param_2.String.Contains "aaa" @>; Ascending = true  } 
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

            test "order by Int desc, Int asc" {
                let actual = 
                    "order by Int ASC, Int DESC"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> true @>
                    |> orderBy [
                        { Selector = quote <@ fun Param_1 -> Param_1.Int @>; Ascending = true  }
                        { Selector = quote <@ fun Param_2 -> Param_2.Int @>; Ascending = false }
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }

            test "order by String desc, String asc" {
                let actual = 
                    "order by String desc, String asc"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> true @>
                    |> orderBy [
                        { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = false }
                        { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = true  }
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }

            test "order by String, String desc, String asc, String" {
                let actual = 
                    "order by String, String desc, String asc, String"
                    |> Linq.CreateQuery<TestRecord> 

                let expected = 
                    quote <@ fun Param_0 -> true @>
                    |> orderBy [
                        { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = true  }
                        { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = false }
                        { Selector = quote <@ fun Param_3 -> Param_3.String @>; Ascending = true  }
                        { Selector = quote <@ fun Param_4 -> Param_4.String @>; Ascending = true  }
                    ]
            
                Expect.equal (string actual) (string expected) (sprintf "%A\n%A" actual expected)
            }
        ]
    ]