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

let equal (actual: Expr<'T>) (expected: Expr<'T>) message =
    match shapeof<'T> with
    | Shape.Enumerable s ->
        match s.Element with
        | Shape.Equality s ->
            let res = s.Accept { new IEqualityVisitor<_> with
                member _.Visit<'t when 't : equality>() =
                    match actual, expected with
                    | Patterns.Value(:? seq<'t> as actual, _), Patterns.Value(:? seq<'t> as expected, _) ->
                        Expect.sequenceEqual actual expected
                    | _ -> ignore }
            res message
        | _ -> ()
    | _ -> Expect.equal actual expected message


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
            "1" |> should equal 1
            "1" |> should equal ^ Some 1
            "1" |> should equal ^ Nullable 1

            "2" |> should equal 2M
            "2" |> should equal ^ Some 2M
            "2" |> should equal ^ Nullable 2M

            "3" |> should equal 3.0
            "3" |> should equal ^ Some 3.0
            "3" |> should equal ^ Nullable 3.0

            "'4 o''clock'" |> should equal "4 o'clock" 
            "'4 o''clock'" |> should equal ^ Some "4 o'clock" 

            "3"   |> should equal ^ byteEnum<TestEnum> 3uy
            "one" |> should equal ^ Some ^ TestEnum.One
            "TWO" |> should equal ^ Nullable TestEnum.Two
            "2"   |> should equal ^ TestEnum.Two

            "('a', 'b', 'c')" |> should equal ^ List.map string [ 'a' .. 'c' ]
            "('a', 'b', 'c')" |> should equal ^ seq { for c in 'a' .. 'c' -> string c }
            "(6, 12, 18)"     |> should equal ^ Array.init 3 ^ fun idx -> float (idx + 1) * 6.0
            "(6, 12, 18)"     |> should equal ^ Seq.init 3 ^ fun idx -> float (idx + 1) * 6.0
            "(5, 5, 5)"       |> should equal ^ Seq.replicate 3 5
        ]

        testListOf "()" [
            shouldFailWith<string seq>   { position = 2L; errors = [ "'" ] }
            shouldFailWith<string list>  { position = 2L; errors = [ "'" ] }
            shouldFailWith<string array> { position = 2L; errors = [ "'" ] }

            shouldFailWith<int seq>   { position = 2L; errors = [ "integer number (32-bit, signed)" ] }
            shouldFailWith<int list>  { position = 2L; errors = [ "integer number (32-bit, signed)" ] }
            shouldFailWith<int array> { position = 2L; errors = [ "integer number (32-bit, signed)" ] }
            
            shouldFailWith<ResizeArray<string>> { position = 2L; errors = [ "'" ] }
            shouldFailWith<ResizeArray<int>>    { position = 2L; errors = [ "integer number (32-bit, signed)" ] }
        ]

        testListOf "" [
            shouldFailWith<int>           { position = 1L; errors = [ "integer number (32-bit, signed)" ] }
            shouldFailWith<int option>    { position = 1L; errors = [ "integer number (32-bit, signed)" ] }
            shouldFailWith<Nullable<int>> { position = 1L; errors = [ "integer number (32-bit, signed)" ] }
            shouldFailWith<float>         { position = 1L; errors = [ "floating-point number"] }
            shouldFailWith<decimal>       { position = 1L; errors = [ "decimal number" ] }
            shouldFailWith<TestEnum>      { position = 1L; errors = [ "TestEnum" ] }
               
            shouldFailWith<string seq>           { position = 1L; errors = [ "(" ] }
            shouldFailWith<string list>          { position = 1L; errors = [ "(" ] }
            shouldFailWith<string array>         { position = 1L; errors = [ "(" ] }
            shouldFailWith<ResizeArray<string>>  { position = 1L; errors = [ "(" ] }
        ]
    ]