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
    
    let private (|TypeShape|) (e: Expr) = TypeShape.Create e.Type

    let rec private normalize (e: Expr) : Expr =
        match e with
        | DerivedPatterns.SpecificCall <@ Array.contains @> (None, _, [elem & TypeShape s; source]) ->
            s.Accept { new ITypeVisitor<_> with
                member _.Visit<'t>() =
                    <@@ (%(Expr.Cast<'t []> source)).Contains %(Expr.Cast<'t> elem) @@> }
    
        | DerivedPatterns.SpecificCall <@ Seq.isEmpty   @> (None, _, [source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ List.isEmpty  @> (None, _, [source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ Array.isEmpty @> (None, _, [source & TypeShape (Shape.Enumerable s)]) ->
            s.Accept { new IEnumerableVisitor<_> with
                member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                    <@@ (%(Expr.Cast<'c> source)).Any() @@> }
    
        | DerivedPatterns.SpecificCall <@ Seq.exists   @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ List.exists  @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ Array.exists @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)]) ->
            s.Accept { new IEnumerableVisitor<_> with
                member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                    <@@ (%(Expr.Cast<'c> source)).Any %(pred |> normalize |> Expr.Cast) @@> }
    
        | DerivedPatterns.SpecificCall <@ Seq.forall   @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ List.forall  @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)])
        | DerivedPatterns.SpecificCall <@ Array.forall @> (None, _, [pred; source & TypeShape (Shape.Enumerable s)]) ->
            s.Accept { new IEnumerableVisitor<_> with
                member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                    <@@ (%(Expr.Cast<'c> source)).All %(pred |> normalize |> Expr.Cast) @@> }
    
        | ExprShape.ShapeVar _ -> e
    
        | ExprShape.ShapeLambda(_, Patterns.Lambda _) -> failwithf "unsupported type %A" e.Type
    
        | ExprShape.ShapeLambda(x, es) ->
            let dom = TypeShape.Create x.Type
            let cod = TypeShape.Create es.Type
    
            dom.Accept {
                new ITypeVisitor<_> with
                    member _.Visit<'dom>() =
                        cod.Accept {
                            new ITypeVisitor<_> with
                                member _.Visit<'cod>() =
                                    Expr.NewDelegate(typeof<Func<'dom, 'cod>>, [x], normalize es)
                        }
            }
    
        | ExprShape.ShapeCombination(obj, es) ->
            ExprShape.RebuildShapeCombination(obj, List.map normalize es)

    let private toLambdaExpression (e: Expr) : Expression = 
        normalize e |> LeafExpressionConverter.QuotationToExpression 

    let private predicate<'T> = 
        mkLambda<'T>() 
        |>> fun x -> x |> toLambdaExpression :?> Predicate<'T>

    let private order<'T> : Parser<struct(LambdaExpression * bool) list, State> =
        let asc = stringCIReturn "ASC" true 
        let desc = stringCIReturn "DESC" false
        let direction = desc <|> asc <|>% true .>> spaces
        let expr = mkLambdaUntyped typeof<'T> .>> spaces

        sepBy (expr .>>. direction) (pchar ',' .>> spaces)
        |>> List.map (fun (x, d) -> struct(downcast toLambdaExpression x, d))

    let private mkLinqExpression<'T> =
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