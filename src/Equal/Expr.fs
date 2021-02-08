//replaces the default unlambda and unlet implementations with the fixed ones
module internal TypeShape.Core

module StagingExtensions =

    [<RequireQualifiedAccess>]
    module Expr =

        open FSharp.Quotations
        open FSharp.Quotations.Patterns
        open FSharp.Quotations.ExprShape
        open FSharp.Quotations.DerivedPatterns
        open TypeShape.Core.StagingExtensions

        let untyped (x: Expr<'a>) = x.Raw

        /// traverses an expression tree applying the transformation
        /// `(fun x y z .. -> M[x,y,z,..]) a b c` => `M[a,b,c,..]`
        let unlambda (expr : Expr<'T>) =
            let (|AppLambdas|_|) (e : Expr) =
                match e with
                | Applications(Lambdas(args, body), vals) ->
                    let flatten x = List.collect id x
                    let replacements = 
                        flatten vals
                        |> List.zip (flatten args)
                        |> Map.ofList

                    body.Substitute(replacements.TryFind) |> Some
                | _ -> None
    
            // traverse the full expression tree
            let rec aux e =
                match e with
                | AppLambdas reducedExpr -> aux reducedExpr
                | ShapeVar _ -> e
                | ShapeLambda(v,b) -> Expr.Lambda(v, aux b)
                | ShapeCombination(comb, args) -> RebuildShapeCombination(comb, List.map aux args)
    
            Expr.cast<'T> (aux expr)
    
        /// Performs the transformation
        /// `let x = y in M[x]` => `M[y]`
        let unlet (expr : Expr<'T>) : Expr<'T> =
            let rec aux e =
                match e with
                | Let(x, (Var _ | Value _ as e), body) when not x.IsMutable || x.Type.IsValueType && x.Name = "copyOfStruct" ->
                    body.Substitute(function v when v = x -> Some e | _ -> None)
                    |> aux
                | ShapeVar _ -> e
                | ShapeLambda(v, body) -> Expr.Lambda(v, aux body)
                | ShapeCombination(comb, args) -> RebuildShapeCombination(comb, List.map aux args)
    
            aux expr |> Expr.cast<'T>
    
        /// Optimizes away staging artifacts from expression tree
        let cleanup (expr : Expr<'T>) : Expr<'T> =
            expr |> unlambda |> unlet
