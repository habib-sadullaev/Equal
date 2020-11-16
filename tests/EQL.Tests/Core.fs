[<AutoOpen>]
module Core

open FSharp.Quotations
open Expecto
open FParsec

type FailInfo = { position: int64; message: string }

type TestEnum = One = 1uy | Two = 2uy

type TestRecord =
    { mutable Parent: TestRecord
      Int: int
      String: string
      TestArray: TestRecord [] }

let testType = typeof<TestRecord>
let x = Var("x", testType)
let param = x |> Expr.Var |> Expr.Cast<TestRecord>

let inline byteEnum<'T when 'T : enum<byte> > (value: byte) : 'T = LanguagePrimitives.EnumOfValue value

let typeName<'T> =
    let ty = typeof<'T>
    if not ty.IsGenericType then ty.Name else

    let tyName = 
        ty.Name 
        |> Seq.takeWhile ^ (<>) '`' 
        |> System.String.Concat
    
    let args =
        ty.GenericTypeArguments
        |> Seq.map ^ fun t -> t.Name
        |> String.concat ", "

    sprintf "%s<%s>" tyName args

let private parse parser input =
    match runParserOnString (parser .>> eof) state "test" input with
    | Success(v, _, _) -> Result.Ok v
    | Failure(s, e, _) -> Result.Error { position = e.Position.Column; message = s.Trim() }

let validInput parser input =
    match parse parser input with
    | Result.Ok actual -> actual
    | Result.Ok _ | Result.Error _ -> failtest "The input should be valid"

let invalidInput parser input =
    match parse parser input with
    | Result.Error actual -> actual
    | Result.Ok _ -> failtest "The input should be invalid"

let failed parser expected input =
    let { position = pos; message = msg } = invalidInput parser input
    Expect.equal      pos expected.position
        ^ sprintf "incorrect error position\ncurrently: '%d'\nexpecting: '%d'" expected.position pos
    Expect.stringEnds msg expected.message  "Parsing should fail with the proper message"

let parsed parser expected input =
    let actual = validInput parser input
    Expect.equal actual expected ^ sprintf "currently:\n%A\nshould be:\n%A\n" actual expected