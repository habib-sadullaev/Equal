namespace Equal.Linq

open System
open System.Runtime.CompilerServices
open System.Linq
open System.Linq.Expressions
open FSharp.Linq.RuntimeHelpers
open FSharp.Quotations
open FParsec
open TypeShape.Core
open Equal.Expression

type QueryResult<'T> = 
    { Predicate: Expression<Func<'T, bool>>
      Order: struct(LambdaExpression * bool) list }

[<AutoOpen>]
module private Linq =
    type Predicate<'T> = Expression<Func<'T, bool>>
    type Selector<'T, 'R> = Expression<Func<'T, 'R>>
    
    let rec normalize (e: Expr) : Expr =
        match e with
        | DerivedPatterns.SpecificCall <@ Array.contains @> (None, _, [e1; e2]) ->
            TypeShape.Create(e1.Type).Accept { new ITypeVisitor<_> with
                member _.Visit<'t>() =
                    <@@ (%(Expr.Cast<'t []> e2)).Contains %(Expr.Cast<'t> e1) @@> }

        | DerivedPatterns.SpecificCall <@ Seq.isEmpty   @> (None, _, [e])
        | DerivedPatterns.SpecificCall <@ List.isEmpty  @> (None, _, [e])
        | DerivedPatterns.SpecificCall <@ Array.isEmpty @> (None, _, [e]) ->
            match TypeShape.Create(e.Type) with
            | Shape.Enumerable s ->
                s.Accept { new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> e)).Any() @@> }
            | _ -> failwith ""
        
        | DerivedPatterns.SpecificCall <@ Seq.exists   @> (None, _, [e1; e2])
        | DerivedPatterns.SpecificCall <@ List.exists  @> (None, _, [e1; e2])
        | DerivedPatterns.SpecificCall <@ Array.exists @> (None, _, [e1; e2]) ->
            match TypeShape.Create(e2.Type) with
            | Shape.Enumerable s ->
                s.Accept { new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> e2)).Any %(e1 |> normalize |> Expr.Cast) @@> }
            | _ -> failwith ""

        | DerivedPatterns.SpecificCall <@ Seq.forall   @> (None, _, [e1; e2])
        | DerivedPatterns.SpecificCall <@ List.forall  @> (None, _, [e1; e2])
        | DerivedPatterns.SpecificCall <@ Array.forall @> (None, _, [e1; e2]) ->
            match TypeShape.Create(e2.Type) with
            | Shape.Enumerable s ->
                s.Accept { new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> e2)).All %(e1 |> normalize |> Expr.Cast) @@> }
            | _ -> failwith ""

        | ExprShape.ShapeVar _ -> e

        | ExprShape.ShapeLambda(_, Patterns.Lambda _) ->
            failwithf "unsupported type %A" e.Type
        
        | ExprShape.ShapeLambda(x, e) ->
            let obj = TypeShape.Create x.Type
            let res = TypeShape.Create e.Type

            obj.Accept { 
                new ITypeVisitor<_> with
                    member _.Visit<'dom>() =
                        res.Accept {
                            new ITypeVisitor<_> with
                                member _.Visit<'cod>() =
                                    Expr.NewDelegate(typeof<Func<'dom, 'cod>>, [x], normalize e)
                        }
            }
        
        | ExprShape.ShapeCombination(obj, es) ->
            ExprShape.RebuildShapeCombination(obj, List.map normalize es)

    let toLambdaExpression (e: Expr) : Expression = 
        normalize e |> LeafExpressionConverter.QuotationToExpression 

    let predicate<'T> = 
        mkLambda<'T>() 
        |>> fun x -> x |> toLambdaExpression :?> Predicate<'T>

    let order<'T> : Parser<struct(LambdaExpression * bool) list, State> =
        let asc = stringCIReturn "ASC" true 
        let desc = stringCIReturn "DESC" false
        let direction = desc <|> asc <|>% true .>> spaces
        let expr = mkLambdaUntyped typeof<'T> .>> spaces

        sepBy (expr .>>. direction) (pchar ',' .>> spaces)
        |>> List.map (fun (x, d) -> struct(downcast toLambdaExpression x, d))

    let mkLinqExpression<'T> =
        parse { let! pred  = predicate<'T> .>> spaces
                and! _     = skipStringCI "ORDER BY" .>> spaces
                and! order = order<'T> .>> spaces
                return { Predicate = pred; Order = order } }

    let translate<'T> query = 
        runParserOnString (mkLinqExpression<'T> .>> eof) state "embeddable query" query
    
    let orderBy struct(selector: LambdaExpression, isAsc: bool) (source: IQueryable<'T>) =
        let shape = TypeShape.Create selector.ReturnType
        shape.Accept { new ITypeVisitor<_> with
            member _.Visit<'a>() =
                selector :?> Selector<'T, 'a>
                |> if isAsc then source.OrderBy else source.OrderByDescending }
    
    let thenBy struct(selector: LambdaExpression, isAsc: bool) (source: IOrderedQueryable<'T>) =
        let shape = TypeShape.Create selector.ReturnType
        shape.Accept { new ITypeVisitor<_> with
            member _.Visit<'a>() =
                selector :?> Selector<'T, 'a>
                |> if isAsc then source.ThenBy else source.ThenByDescending }

type [<Extension>] Expression =
    static member CreateQuery<'T>(query: string) =
        match translate<'T> query with
        | Success(v, _, _) -> v
        | Failure(e, _, _) -> failwith e

open type Expression
 
type Expression with
    [<Extension>]
    static member Apply(source: IQueryable<'TSource>, query: string) : IQueryable<'TSource> =
        let { Predicate = predicate; Order = order } = CreateQuery<'TSource> query
        source.Where(predicate).OrderBy(order)
    
    [<Extension>]
    static member private OrderBy(source: IQueryable<'TSource>, expressions: struct(LambdaExpression * bool) list) =
        match expressions with
        | []      ->  source
        | e :: es -> 
            let mutable res = source |> orderBy e
            for e in es do res <- res |> thenBy e
            upcast res