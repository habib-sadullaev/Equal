module ComparisonTests

open System
open FSharp.Quotations
open Expecto
open FParsec
open EQL.Comparison

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
        "Parent.Parent.HasValue" |> parsedInto <@ (%param).Parent.Parent.HasValue @>

        "Parent.HasValue = true" |> failedWith { position = 17L; message = "end of input" }
    ]