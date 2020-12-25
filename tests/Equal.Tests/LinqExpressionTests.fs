module QueryParserTests

open System.Linq
open Equal.Linq
open Expecto
open Expect

#nowarn "49"

let inline should compare (expected: QueryResult<'T>) input  =
    let name = sprintf "should parse '%s'" input |> String.map ^ function '.' -> '_' | c -> c
    test name {
        let actual = EmbeddedQuery.CreateQuery<'T> input
        compare (string actual) (string expected) (sprintf "%A\n%A" actual expected)
    }

let inline shouldFailWith (expected: FailInfo) input =
    let parser = EmbeddedQuery.mkLinqExpression<TestRecord>
    let name = sprintf "should fail parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input }

let quote = EmbeddedQuery.quote

let [<Literal>] eof = "end of input"

[<Tests>]
let tests =
    testList "linq expression parser" [
        testList "with valid input" [
            "" |> should equal { 
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [] 
            }
            
            "String Starts With '' and Int > 0 order by Int > 0 desc, String contains 'aaa'" |> should equal { 
                Predicate = quote <@ fun Param_0 -> Param_0.String.StartsWith("") && Param_0.Int > 0 @>
                OrderBy  = [
                    { Selector = quote <@ fun Param_1 -> Param_1.Int > 0               @>; Ascending = false }
                    { Selector = quote <@ fun Param_2 -> Param_2.String.Contains "aaa" @>; Ascending = true  }
                ]
            }

            "TestArray Any (String Starts With '') and Int > 0" |> should equal {
                Predicate = quote <@ fun Param_0 -> Param_0.TestArray.Any(fun Param_1 -> Param_1.String.StartsWith("")) && Param_0.Int > 0 @>
                OrderBy = []
            }

            "order by Int ASC, Int DESC" |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.Int @>; Ascending = true  }
                    { Selector = quote <@ fun Param_2 -> Param_2.Int @>; Ascending = false }
                ]
            }

            "order by String desc, String asc" |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = false }
                    { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = true  }
                ]
            }

            "order by String, String desc, String asc, String" |> should equal {
                Predicate = quote <@ fun (Param_0: TestRecord) -> true @>
                OrderBy = [
                    { Selector = quote <@ fun Param_1 -> Param_1.String @>; Ascending = true  }
                    { Selector = quote <@ fun Param_2 -> Param_2.String @>; Ascending = false }
                    { Selector = quote <@ fun Param_3 -> Param_3.String @>; Ascending = true  }
                    { Selector = quote <@ fun Param_4 -> Param_4.String @>; Ascending = true  }
                ]
            }
        ]

        testList "with invalid input" [
            "TestArray IS " |> shouldFailWith { position = 11L; errors = ["ALL"; "ANY"; "IS EMPTY"] }
            "String >"      |> shouldFailWith { position =  8L; errors = ["CONTAINS"; "ENDS WITH"; "STARTS WITH"] }
            
            "OptionalEnum &&" |> shouldFailWith { position = 14L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"] }
            "Int order by"    |> shouldFailWith { position =  5L; errors = ["<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN"] }
            
            "HasValue order by" |> shouldFailWith { position = 18L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
            "order by"          |> shouldFailWith { position =  9L; errors = ["("; "NOT"; yield! propsof<TestRecord>] }
            "Int1 order by"     |> shouldFailWith { position =  1L; errors = ["("; "NOT"; "ORDER BY"; yield! propsof<TestRecord>] }
            
            "HasValue &&"                   |> shouldFailWith { position = 10L; errors = ["AND"; "OR"; "ORDER BY"; eof] }
            "HasValue ordr bi"              |> shouldFailWith { position = 10L; errors = ["AND"; "OR"; "ORDER BY"; eof] }
            "Int > 0 or HasValue andString" |> shouldFailWith { position = 21L; errors = ["AND"; "OR"; "ORDER BY"; eof] }
            "Int > 0 and HasValue orString" |> shouldFailWith { position = 22L; errors = ["AND"; "OR"; "ORDER BY"; eof] }

            "Not (HasValue"                                   |> shouldFailWith { position = 14L; errors = [")"; "AND"; "OR"] }
            "(String ends with ''"                            |> shouldFailWith { position = 21L; errors = [")"; "AND"; "OR"] }
            "((String ends with '')"                          |> shouldFailWith { position = 23L; errors = [")"; "AND"; "OR"] }
            "(Parent.Parent.String ends with ''"              |> shouldFailWith { position = 35L; errors = [")"; "AND"; "OR"] }
            "Int > 73 and (Parent.Parent.String ends with ''" |> shouldFailWith { position = 48L; errors = [")"; "AND"; "OR"] }
            
            "Parent.Parent.HasValue and String contains 'aaa' order by Int asx" |> shouldFailWith { 
                position = 63L
                errors = [","; "<"; "<="; "<>"; "="; ">"; ">="; "ASC"; "DESC"; "IN"; "NOT IN"; eof]
            }
        ]
    ]