module ComparisonTests

open FSharp.Quotations
open Expecto
open FParsec
open Equal.Expression

let parser = mkComparison param

let inline should compare expected input =
    let name = sprintf "parses '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { parsed (parser |>> Expr.Cast) compare expected input } |> testLabel "with valid input"

let inline shouldFailWith expected input =
    let name = sprintf "fails parsing '%s'" input |> String.map ^ function '.' -> '_' | x -> x
    test name { failed parser expected input } |> testLabel "with invalid input"

let equal = Expect.equal

[<Tests>]
let tests =
    testList "comparison parser" [
        "Parent.Parent.HasValue"   |> should equal <@ (%param).Parent.Parent.HasValue @>
        
        "String starts with 'aaa'"    |> should equal <@ (%param).String.StartsWith "aaa" @>
        "String ContainS 'bbb'"       |> should equal <@ (%param).String.Contains   "bbb" @>
        "String ENDS WITH 'ccc'"      |> should equal <@ (%param).String.EndsWith   "ccc" @>
        "OptionalString CONTAINS 'A'" |> should equal <@ (%param).OptionalString.IsSome &&  (%param).OptionalString.Value.Contains "A" @>

        "Parent.Parent.Int <  1"   |> should equal <@ (%param).Parent.Parent.Int <  1 @>
        "Parent.Parent.Int <= 2"   |> should equal <@ (%param).Parent.Parent.Int <= 2 @>
        "Parent.Parent.Int =  3"   |> should equal <@ (%param).Parent.Parent.Int =  3 @>
        "Parent.Parent.Int <> 4"   |> should equal <@ (%param).Parent.Parent.Int <> 4 @>
        "Parent.Parent.Int >  5"   |> should equal <@ (%param).Parent.Parent.Int >  5 @>
        "Parent.Parent.Int >= 6"   |> should equal <@ (%param).Parent.Parent.Int >= 6 @>
        "Parent.Parent.Float<=5"   |> should equal <@ (%param).Parent.Parent.Float <= 5. @>
        "Parent.Parent.Enum  <  1" |> should equal <@ (%param).Parent.Parent.Enum < TestEnum.One @>
        "Parent.OptionalEnum=two"  |> should equal <@ (%param).Parent.OptionalEnum.IsSome && (%param).Parent.OptionalEnum.Value = TestEnum.Two @>
        "NullableFloat = 1"        |> should equal <@ (%param).NullableFloat.HasValue && (%param).NullableFloat.Value = 1. @>

        "Parent.Int in (4, 9)"       |> should equal <@ Array.contains (%param).Parent.Int %(constExpr [| 4; 9 |]) @>
        "Parent.Int NOT IN (4, 9)"   |> should equal <@ not (Array.contains (%param).Parent.Int %(constExpr [| 4; 9 |])) @>
        "NullableFloat in (1, 2, 3)" |> should equal <@ (%param).NullableFloat.HasValue && Array.contains (%param).NullableFloat.Value %(constExpr [| 1. .. 3. |]) @>
        "OptionalEnum in (1, 2, 3)"  |> should equal <@ (%param).OptionalEnum.IsSome && Array.contains (%param).OptionalEnum.Value %(constExpr [| TestEnum.One; TestEnum.Two; byteEnum 3uy |]) @>

        "Children IS EMPTY" |> should equal <@ (%param).Children.IsSome && List.isEmpty (%param).Children.Value @>

        "HasValue = true"   |> shouldFailWith { position = 10L; errors = [ "AND"; "OR"; "end of input" ] }
        "String = 'zzz'"    |> shouldFailWith { position =  8L; errors = [ "CONTAINS"; "ENDS WITH"; "STARTS WITH" ] }
        "Parent.Parent.Int" |> shouldFailWith { position = 18L; errors = [ "<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN" ] }
        "NullableFloat"     |> shouldFailWith { position = 14L; errors = [ "<"; "<="; "<>"; "="; ">"; ">="; "IN"; "NOT IN" ] }
        "OptionalString"    |> shouldFailWith { position = 15L; errors = ["CONTAINS"; "ENDS WITH"; "STARTS WITH"] }
        "(HasValue"         |> shouldFailWith { position = 10L; errors = [")"; "AND"; "OR"] }
    ]
