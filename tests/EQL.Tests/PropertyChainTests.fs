module PropertyChainTests

open FSharp.Quotations
open Expecto
open EQL.PropertyChain
open FParsec

type Test =
    { mutable Parent: Test
      Int: int
      String: string
      TestArray: Test [] }

// replaces IL's prop virtual calls to calls
let devirtualize (e: Expr<'T>) =
    let rec aux e =
        match e with
        | Patterns.PropertyGet(Some e, pi, _) -> Expr.PropertyGet(e, e.Type.GetProperty(pi.Name))
        | ExprShape.ShapeVar _ -> e
        | ExprShape.ShapeLambda(x, e) -> Expr.Lambda(x, aux e)
        | ExprShape.ShapeCombination(obj, es) -> ExprShape.RebuildShapeCombination(obj, List.map aux es)

    aux e |> Expr.Cast<'T>

let x = Var("x", typeof<Test>)
let param = x |> Expr.Var |> Expr.Cast<Test>

let parser = mkPropChain param

let parsedInto expected input =
    let name = input |> String.map(function '.' -> '_' | x -> x)
    test name { parsed (parser |>> Expr.Cast) expected input }

let failedWith expected input =
    let name = sprintf "fails parsing '%s'" (input |> String.map ^ function '.' -> '_' | x -> x) 
    let expected = { expected with message = sprintf "'property of %s'" expected.message } 
    test name { failed parser expected input }

[<Tests>]
let tests =
    testList "property chain tests" [
        testList "valid input" [
            "Int"                   |> parsedInto  <@ (%param).Int @>
            "Parent.Int"            |> parsedInto  <@ (%param).Parent.Int @>
            "Parent.Parent.Int"     |> parsedInto  <@ (%param).Parent.Parent.Int @>
            "Parent.String"         |> parsedInto  <@ (%param).Parent.String @>
            "Parent.String.Length"  |> parsedInto  <@ (%param).Parent.String.Length @>
            "Parent.TestArray"      |> parsedInto  <@ (%param).Parent.TestArray @>

            "TestArray.IsFixedSize" |> parsedInto ^ devirtualize <@ (%param).TestArray.IsFixedSize @>
        ]

        test "IL.virtual call -> call" {
            let actual = 
                let expr1 = Expr.PropertyGet(param, typeof<Test>.GetProperty("TestArray"))
                let expr2 = Expr.PropertyGet(param, typeof<Test>.GetProperty("TestArray"))

                let expr1' = Expr.Cast<bool> ^ Expr.PropertyGet(expr1, typeof<Test array>.GetProperty "IsFixedSize")
                let expr2' = <@ %(Expr.Cast<int> ^ Expr.PropertyGet(expr2, typeof<Test array>.GetProperty "Length")) > 10 @>
                    
                Expr.Lambda(x, <@ %expr1' && %expr2' @>)
                
            let expected = 
                Expr.Lambda(x, <@ (%param).TestArray.IsFixedSize && (%param).TestArray.Length > 10 @>) 
                |> Expr.Cast<Test -> bool> 
                |> devirtualize 
                
            Expect.equal actual expected.Raw "all prop 'virtual call's should be replaced with 'call's"
        }

        testList "invalid input" [
            ""               |> failedWith { position = 1L;  message = typeof<Test>.FullName }
            "Int."           |> failedWith { position = 5L;  message = typeof<int>.FullName }
            "Int2"           |> failedWith { position = 1L;  message = typeof<Test>.FullName }
            "Int2."          |> failedWith { position = 1L;  message = typeof<Test>.FullName }
            "Parent1."       |> failedWith { position = 1L;  message = typeof<Test>.FullName }
            "Parent."        |> failedWith { position = 8L;  message = typeof<Test>.FullName }
            "Parent.String." |> failedWith { position = 15L; message = typeof<string>.FullName }
            
            "Parent.Parent.String1.Length" |> failedWith { position = 15L; message = typeof<Test>.FullName }
            "Parent.Parent.String1.Length."|> failedWith { position = 15L; message = typeof<Test>.FullName }
        ]
    ]