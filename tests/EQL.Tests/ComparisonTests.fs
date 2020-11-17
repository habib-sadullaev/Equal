module ComparisonTests

open System
open FSharp.Quotations
open FSharp.Linq.NullableOperators
open Expecto
open FParsec
open EQL.Expression

let parser = mkComparison param

let inline parsedInto expected input =
    let name = sprintf "parses '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    let expected = expected
    test name { parsed (parser |>> Expr.Cast) expected input } |> testLabel "with valid input"

let inline failedWith expected input =
    let name = sprintf "fails parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input } |> testLabel "with invalid input"

[<Tests>]
let tests =
    testList "comparison parser" [
        "Parent.Parent.HasValue"   |> parsedInto <@ (%param).Parent.Parent.HasValue @>
        
        "String starts with 'aaa'" |> parsedInto <@ (%param).String.StartsWith "aaa" @>
        "String ContainS 'bbb'"    |> parsedInto <@ (%param).String.Contains   "bbb" @>
        "String ENDS WITH 'ccc'"   |> parsedInto <@ (%param).String.EndsWith   "ccc" @>

        "Parent.Parent.Int <  1"   |> parsedInto <@ (%param).Parent.Parent.Int <  1 @>
        "Parent.Parent.Int <= 2"   |> parsedInto <@ (%param).Parent.Parent.Int <= 2 @>
        "Parent.Parent.Int =  3"   |> parsedInto <@ (%param).Parent.Parent.Int =  3 @>
        "Parent.Parent.Int <> 4"   |> parsedInto <@ (%param).Parent.Parent.Int <> 4 @>
        "Parent.Parent.Int >  5"   |> parsedInto <@ (%param).Parent.Parent.Int >  5 @>
        "Parent.Parent.Int >= 6"   |> parsedInto <@ (%param).Parent.Parent.Int >= 6 @>
        "Parent.Parent.Float<=5"   |> parsedInto <@ (%param).Parent.Parent.Float <= 5. @>
        "Parent.Parent.Enum  <  1" |> parsedInto <@ (%param).Parent.Parent.Enum < TestEnum.One @>
        "Parent.OptionalEnum=two"  |> parsedInto <@ (%param).Parent.OptionalEnum = %(constExpr ^ Some ^ TestEnum.Two) @>
        "NullableFloat = 1"        |> parsedInto <@ (%param).NullableFloat ?=? %(constExpr ^ Nullable 1.) @>

        "Parent.Int in (4, 9)"       |> parsedInto <@ Array.contains (%param).Parent.Int %(constExpr [| 4; 9 |]) @>
        "Parent.Int NOT IN (4, 9)"   |> parsedInto <@ not (Array.contains (%param).Parent.Int %(constExpr [| 4; 9 |])) @>
        "NullableFloat in (1, 2, 3)" |> parsedInto <@ Array.contains (%param).NullableFloat %(constExpr [| for x in 1. .. 3. -> Nullable x |]) @>

        "HasValue = true"   |> failedWith { position = 10L; errors = [ "end of input" ] }
        "String = 'zzz'"    |> failedWith { position = 8L;  errors = [ "CONTAINS"; "ENDS WITH"; "STARTS WITH" ] }
        "Parent.Parent.Int" |> failedWith { position = 18L; errors = [ "<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN" ] }
        "NullableFloat"     |> failedWith { position = 14L; errors = [ "<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN" ] }
    ]