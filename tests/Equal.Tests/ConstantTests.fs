module ConstantTests

open System
open FSharp.Quotations
open Expecto
open Equal.Constant
open TypeShape.Core

let parser<'T> = mkConst<'T>()

let parsed' parser compare expected input =
    let actual = validInput parser input
    compare actual expected ^ sprintf "expected:\n%A\n  actual:\n%A\n" expected actual

let equal = Expect.equal

let inline should compare (expected: 'T) input =
    let name = sprintf "parses '%s' into '%s'" input typeName<'T>
    test name { parsed' parser<'T> compare (constExpr expected) input }

let inline shouldFailWith<'T> expected input = 
    let testName = sprintf "fails parsing '%s' into '%s'" input typeName<'T>
    test testName { failed parser<'T> expected input }

let testListOf input data =
    testList "with invalid input" [ for tester in data -> tester input ]

[<Tests>]
let tests =
    testList "constant parser" [
        testList "with valid input" [
            "1"               |> should equal 1
            "2"               |> should equal ^ TestEnum.Two
            "3"               |> should equal ^ byteEnum<TestEnum> 3uy
            "4."              |> should equal 4M
            "5.0"             |> should equal 5.0

            "'4 o''clock'"    |> should equal "4 o'clock" 
            
            "('a', 'b', 'c')" |> should equal ^ [| for c in 'a' .. 'c' -> string c |]
            "(6, 12, 18)"     |> should equal ^ [| 6. .. 6. .. 18. |]
            "(5, 5, 5)"       |> should equal ^ Array.replicate 3 5
        ]

        testListOf "()" [
            shouldFailWith<int array>    { position = 2L; errors = ["integer number (32-bit, signed)"] }
            shouldFailWith<string array> { position = 2L; errors = ["'"] }
        ]

        testListOf "" [
            shouldFailWith<int>          { position = 1L; errors = ["integer number (32-bit, signed)"] }
            shouldFailWith<float>        { position = 1L; errors = ["floating-point number"] }
            shouldFailWith<decimal>      { position = 1L; errors = ["decimal number"] }
            shouldFailWith<TestEnum>     { position = 1L; errors = ["TestEnum"] }
            
            shouldFailWith<int array>    { position = 1L; errors = ["("] }
            shouldFailWith<string array> { position = 1L; errors = ["("] }
        ]
    ]