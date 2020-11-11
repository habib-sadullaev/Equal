module ConstantTests

open System
open FSharp.Quotations
open Expecto
open FParsec
open EQL.Primitives
open EQL.Constant

type FailInfo = { position: int64; message: string }

let parseConstant<'T> input =
    match runParserOnString (mkConst<'T>() .>> eof) state "test" input with
    | Success(v, _, _) -> Result.Ok v
    | Failure(s, e, _) -> Result.Error { position = e.Position.Column; message = s.Trim() }

let typeName<'T> =
    let ty = typeof<'T>
    if not ty.IsGenericType then ty.Name else
    
    let tyName = ty.Name |> Seq.takeWhile ^ (<>) '`' |> String.Concat
    let args = ty.GenericTypeArguments |> Seq.map (fun t -> t.Name) |> String.concat ", "
    sprintf "%s<%s>" tyName args

let shouldPass input f (expected : 'T) =
    let name = sprintf "parses '%s' into '%s'" input typeName<'T>
    test name {
        match parseConstant<'T> input with
        | Result.Ok(Patterns.Value(:? 'T as actual, _)) ->
            f actual expected "They should be equal"
        | _ -> failtest "The input should be valid"
    }

let invalidInput<'T> expected =
    let tester input =
        match parseConstant<'T> input with
        | Result.Error { position = pos; message = msg } ->
            Expect.stringEnds msg expected.message  "The test should fail with the proper message" 
            Expect.equal      pos expected.position "The test should fail with the proper position" 
        | _ -> failtest "The input should be invalid"
    typeName<'T>, tester

let shouldFail input data =
    testList "invalid input" [
        yield! testFixture (fun f () -> f input) 
            [ for (typeName, tester) in data -> 
              let testName = sprintf "fails parsing '%s' into '%s'" input typeName
              testName, tester ]
    ]

type TestEnum = One = 1uy | Two = 2uy

let inline byteEnum<'T when 'T : enum<byte> > (value: byte) : 'T = LanguagePrimitives.EnumOfValue value

[<Tests>]
let tests =
    testList "constant tests" [
        testList "valid input" [
            shouldPass "1" Expect.equal ^ 1
            shouldPass "1" Expect.equal ^ Some 1
            shouldPass "1" Expect.equal ^ Nullable 1

            shouldPass "2" Expect.equal ^ 2M
            shouldPass "2" Expect.equal ^ Some 2M
            shouldPass "2" Expect.equal ^ Nullable 2M

            shouldPass "3" Expect.equal ^ 3.0
            shouldPass "3" Expect.equal ^ Some 3.0
            shouldPass "3" Expect.equal ^ Nullable 3.0

            shouldPass "'4 o''clock'" Expect.equal ^ "4 o'clock" 
            shouldPass "'4 o''clock'" Expect.equal ^ Some "4 o'clock" 

            shouldPass "3"   Expect.equal ^ byteEnum<TestEnum> 3uy
            shouldPass "one" Expect.equal ^ Some ^ TestEnum.One
            shouldPass "TWO" Expect.equal ^ Nullable TestEnum.Two
            shouldPass "2"   Expect.equal ^ TestEnum.Two

            shouldPass "('a', 'b', 'c')" Expect.equal         ^ List.map string [ 'a' .. 'c' ]
            shouldPass "(6, 12, 18)"     Expect.equal         ^ [| 6.0 .. 6.0 .. 18.0 |]
            shouldPass "(5, 5, 5)"       Expect.sequenceEqual ^ Seq.replicate 3 5
        ]

        shouldFail "()" [
            invalidInput<string seq>   { position = 1L; message = "'\\''" }
            invalidInput<string list>  { position = 1L; message = "'\\''" }
            invalidInput<string array> { position = 1L; message = "'\\''" }

            invalidInput<int seq>   { position = 1L; message = "integer number (32-bit, signed)" }
            invalidInput<int list>  { position = 1L; message = "integer number (32-bit, signed)" }
            invalidInput<int array> { position = 1L; message = "integer number (32-bit, signed)" }
            
            invalidInput<ResizeArray<string>> { position = 1L; message = "'\\''" }
            invalidInput<ResizeArray<int>>    { position = 1L; message = "integer number (32-bit, signed)" }
        ]

        shouldFail "" [
            invalidInput<int>           { position = 1L; message = "integer number (32-bit, signed)" }
            invalidInput<int option>    { position = 1L; message = "integer number (32-bit, signed)" }
            invalidInput<Nullable<int>> { position = 1L; message = "integer number (32-bit, signed)" }
               
            invalidInput<float>         { position = 1L; message = "floating-point number" }
            invalidInput<decimal>       { position = 1L; message = "decimal number" }
            invalidInput<TestEnum>      { position = 1L; message = "TestEnum" }
               
            invalidInput<string seq>           { position = 1L; message = "'('" }
            invalidInput<string list>          { position = 1L; message = "'('" }
            invalidInput<string array>         { position = 1L; message = "'('" }
            invalidInput<ResizeArray<string>>  { position = 1L; message = "'('" }
        ]
    ]