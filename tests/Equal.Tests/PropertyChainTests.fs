module PropertyChainTests

open FSharp.Quotations
open Expecto
open Equal.Expression
open FParsec

// replaces IL's prop 'virtual call's with 'call's
let devirtualizeUntyped (e: Expr) =
    let rec aux e =
        match e with
        | Patterns.PropertyGet(Some e, pi, _) -> Expr.PropertyGet(e, e.Type.GetProperty(pi.Name))
        | ExprShape.ShapeVar _ -> e
        | ExprShape.ShapeLambda(x, e) -> Expr.Lambda(x, aux e)
        | ExprShape.ShapeCombination(obj, es) -> ExprShape.RebuildShapeCombination(obj, List.map aux es)

    aux e

let inline devirtualize (e: Expr<'T>) = devirtualizeUntyped e |> Expr.Cast<'T>

let parser = mkPropChain param

let inline parsedInto expected input =
    let name = input |> String.map(function '.' -> '_' | x -> x)
    test name { parsed (parser |>> Expr.Cast) expected input }

let inline failedWith expected input =
    let name = sprintf "fails parsing '%s'" (input |> String.map ^ function '.' -> '_' | x -> x) 
    let expected = { expected with errors = expected.errors |> List.map ^ sprintf "property of %s" } 
    test name { failed parser expected input }

[<Tests>]
let tests =
    testList "property chain parser" [
        testList "with valid input" [
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
                let expr1 = Expr.PropertyGet(param, testType.GetProperty("TestArray"))
                let expr2 = Expr.PropertyGet(param, testType.GetProperty("TestArray"))

                let expr1' = Expr.Cast ^ Expr.PropertyGet(expr1, typeof<TestRecord array>.GetProperty "IsFixedSize")
                let expr2' = Expr.Cast ^ Expr.PropertyGet(expr2, typeof<TestRecord array>.GetProperty "Length")
                    
                Expr.Lambda(x, <@ %expr1' && %expr2' > 10 @>)
                
            let expected = 
                Expr.Lambda(x, <@ (%param).TestArray.IsFixedSize && (%param).TestArray.Length > 10 @>) 
                |> devirtualizeUntyped
                
            Expect.equal actual expected "all prop 'virtual call's should be replaced with 'call's"
        }

        testList "with invalid input" [
            ""               |> failedWith { position = 1L;  errors = [ testType.FullName ] }
            "Int."           |> failedWith { position = 5L;  errors = [ typeof<int>.FullName ] }
            "Int2"           |> failedWith { position = 1L;  errors = [ sprintf "%A" testType ] }
            "Int2."          |> failedWith { position = 1L;  errors = [ sprintf "%O" testType ] }
            "Parent1."       |> failedWith { position = 1L;  errors = [ testType.FullName ] }
            "Parent."        |> failedWith { position = 8L;  errors = [ testType.FullName ] }
            "Parent.String." |> failedWith { position = 15L; errors = [ typeof<string>.FullName ] }
            
            "Parent.Parent.String1.Length" |> failedWith { position = 15L; errors = [ testType.FullName ] }
            "Parent.Parent.String1.Length."|> failedWith { position = 15L; errors = [ testType.FullName ] }
        ]
    ]