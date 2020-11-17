[<AutoOpen>]
module Core

open FSharp.Quotations
open Expecto
open FParsec

type FailInfo = { position: int64; errors: string list }

type TestEnum = One = 1uy | Two = 2uy

type TestRecord =
    { mutable Parent: TestRecord
      HasValue: bool
      Int: int
      String: string
      Float: float
      Enum: TestEnum
      OptionalEnum: TestEnum option
      TestArray: TestRecord []
      TestList: TestRecord list }

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
    | Failure(_, e, _) -> 
        let rec gatherErrs errs = 
            [ for err in errs |> ErrorMessageList.ToSortedArray do
              match err with
              | Expected msg 
              | ExpectedString msg 
              | ExpectedStringCI msg -> msg
              | NestedError (_, _, errs) -> yield! gatherErrs errs
              | _ -> ()
            ]
        Result.Error { position = e.Position.Column; errors = gatherErrs e.Messages }

let validInput parser input =
    match parse parser input with
    | Result.Ok actual -> actual
    | Result.Error info -> failtest ^ sprintf "The input should be valid\n%A" info

let invalidInput parser input =
    match parse parser input with
    | Result.Error actual -> actual
    | Result.Ok _ -> failtest "The input should be invalid"

let failed parser expected input =
    let { position = pos; errors = errs } = invalidInput parser input
    Expect.equal pos expected.position
        ^ sprintf "incorrect error position\nexpected: '%d'\n  actual: '%d'" expected.position pos
    Expect.equal errs expected.errors
        ^ sprintf "Incorrect error message\nexpected:\n%A\n  actual:\n%A" errs expected.errors

let parsed parser expected input =
    let actual = validInput parser input
    Expect.equal actual expected ^ sprintf "expected:\n%A\n  actual:\n%A\n" expected actual