module ConstantTests

open System
open FSharp.Quotations
open Expecto
open EQL.Constant

let parser<'T> = mkConst<'T>()

let parsedInto expected input =
    let name = sprintf "parses '%s' into '%s'" input typeName<'T>
    test name { parsed parser<'T> expected input }

let failedWith<'T> expected = typeName<'T>, failed parser<'T> expected

let invalidInputList input data =
    testList "invalid input" [
        yield! testFixture (fun f () -> f input) 
            [ for (typeName, tester) in data -> 
              let testName = sprintf "fails parsing '%s' into '%s'" input typeName
              testName, tester ]
    ]

[<Tests>]
let tests =
    testList "constant tests" [
        testList "valid input" [
            "1" |> parsedInto ^ constexpr 1
            "1" |> parsedInto ^ constexpr ^ Some 1
            "1" |> parsedInto ^ constexpr ^ Nullable 1

            "2" |> parsedInto ^ constexpr 2M
            "2" |> parsedInto ^ constexpr ^ Some 2M
            "2" |> parsedInto ^ constexpr ^ Nullable 2M

            "3" |> parsedInto ^ constexpr 3.0
            "3" |> parsedInto ^ constexpr ^ Some 3.0
            "3" |> parsedInto ^ constexpr ^ Nullable 3.0

            "'4 o''clock'" |> parsedInto ^ constexpr "4 o'clock" 
            "'4 o''clock'" |> parsedInto ^ constexpr ^ Some "4 o'clock" 

            "3"   |> parsedInto ^ constexpr ^ byteEnum<TestEnum> 3uy
            "one" |> parsedInto ^ constexpr ^ Some ^ TestEnum.One
            "TWO" |> parsedInto ^ constexpr ^ Nullable TestEnum.Two
            "2"   |> parsedInto ^ constexpr ^ TestEnum.Two

            "('a', 'b', 'c')" |> parsedInto ^ constexpr ^ List.map string [ 'a' .. 'c' ]
            "(6, 12, 18)"     |> parsedInto ^ constexpr [| 6.0 .. 6.0 .. 18.0 |]
            
            test "parses '(5, 5, 5)' into 'int seq'" {
                match validInput parser<int seq> "(5, 5, 5)" with
                | Patterns.Value(:? (int seq) as actual, _) -> 
                    Expect.sequenceEqual actual (Seq.replicate 3 5) "They should be equal"
                
                | _ -> ()
            }
        ]

        invalidInputList "()" [
            failedWith<string seq>   { position = 1L; message = "'\\''" }
            failedWith<string list>  { position = 1L; message = "'\\''" }
            failedWith<string array> { position = 1L; message = "'\\''" }

            failedWith<int seq>   { position = 1L; message = "integer number (32-bit, signed)" }
            failedWith<int list>  { position = 1L; message = "integer number (32-bit, signed)" }
            failedWith<int array> { position = 1L; message = "integer number (32-bit, signed)" }
            
            failedWith<ResizeArray<string>> { position = 1L; message = "'\\''" }
            failedWith<ResizeArray<int>>    { position = 1L; message = "integer number (32-bit, signed)" }
        ]

        invalidInputList "" [
            failedWith<int>           { position = 1L; message = "integer number (32-bit, signed)" }
            failedWith<int option>    { position = 1L; message = "integer number (32-bit, signed)" }
            failedWith<Nullable<int>> { position = 1L; message = "integer number (32-bit, signed)" }
            failedWith<float>         { position = 1L; message = "floating-point number" }
            failedWith<decimal>       { position = 1L; message = "decimal number" }
            failedWith<TestEnum>      { position = 1L; message = "TestEnum" }
               
            failedWith<string seq>           { position = 1L; message = "'('" }
            failedWith<string list>          { position = 1L; message = "'('" }
            failedWith<string array>         { position = 1L; message = "'('" }
            failedWith<ResizeArray<string>>  { position = 1L; message = "'('" }
        ]
    ]