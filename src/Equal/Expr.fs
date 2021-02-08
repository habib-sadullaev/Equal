//replaces the default unlambda and unlet implementations with the fixed ones
module internal TypeShape.Core

module StagingExtensions =

    [<RequireQualifiedAccess>]
    module Expr =

        open FSharp.Quotations
        open FSharp.Quotations.Patterns
        open FSharp.Quotations.ExprShape
        open TypeShape.Core.StagingExtensions

        let untyped (x: Expr<'a>) = x.Raw
    
        let unlambda (expr : Expr<'T>) =
            let (|AppLambdas|_|) (e : Expr) =
                // traverses the "App(App(App ... " part of the expression
                let rec gatherApps args e =
                    match e with
                    | Application(lhs, rhs) -> gatherApps (rhs :: args) lhs
                    | _ -> args, e
    
                // traverses the "Lambda(Lambda(Lambda ... " part of the expression
                let rec gatherLambdas args acc e =
                    match e, args with
                    | _, [] -> Some (acc, e)
                    | Lambda(v, body), hd :: tl -> gatherLambdas tl ((v, hd) :: acc) body
                    | _ -> None
    
                // performs substitution of each recovered var with corresponding value
                let rec substitute vars (body : Expr) =
                    match vars with
                    | [] -> body
                    | (var, value) :: rest ->
                        let body2 = body.Substitute(function v when v = var -> Some value | _ -> None)
                        substitute rest body2
    
                match gatherApps [] e with
                | [], _ -> None
                | args, body ->
                    match gatherLambdas args [] body with
                    | None -> None
                    | Some(vars, body) -> Some(substitute vars body)
    
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
