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

type private Predicate<'T>    = Expression<Func<'T, bool>>
type private Selector<'T, 'R> = Expression<Func<'T, 'R>>

type [<Struct>] OrderBy         = { Selector: LambdaExpression; Ascending: bool }
type [<Struct>] QueryResult<'T> = { Predicate: Predicate<'T>; OrderBy: OrderBy list }

[<RequireQualifiedAccess>]
module Linq =
    let private (|TypeShape|) (e: Expr) = TypeShape.Create e.Type

    let private (|MethodCall|_|) m =
        function DerivedPatterns.SpecificCall m (None, _, args) -> Some args | _ -> None

    let rec private normalize (e: Expr) : Expr =
        match e with
        | MethodCall <@ Array.contains @> [elem & TypeShape s; source] ->
            s.Accept { 
                new ITypeVisitor<_> with
                    member _.Visit<'t>() =
                        <@@ (%(Expr.Cast<'t []> source)).Contains %(Expr.Cast<'t> elem) @@>
            }
    
        | MethodCall <@ Seq.isEmpty   @> [source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ List.isEmpty  @> [source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ Array.isEmpty @> [source & TypeShape (Shape.Enumerable s)] ->
            s.Accept { 
                new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> source)).Any() @@> 
            }
    
        | MethodCall <@ Seq.exists   @> [pred; source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ List.exists  @> [pred; source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ Array.exists @> [pred; source & TypeShape (Shape.Enumerable s)] ->
            s.Accept { 
                new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> source)).Any %(pred |> normalize |> Expr.Cast) @@> 
            }
    
        | MethodCall <@ Seq.forall   @> [pred; source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ List.forall  @> [pred; source & TypeShape (Shape.Enumerable s)]
        | MethodCall <@ Array.forall @> [pred; source & TypeShape (Shape.Enumerable s)] ->
            s.Accept {
                new IEnumerableVisitor<_> with
                    member _.Visit<'c, 'e when 'c :> seq<'e>>() =
                        <@@ (%(Expr.Cast<'c> source)).All %(pred |> normalize |> Expr.Cast) @@> 
            }

        | Patterns.NewDelegate(ty, parameters, body) -> Expr.NewDelegate(ty, parameters, normalize body)
    
        | ExprShape.ShapeVar _ -> e
    
        | ExprShape.ShapeLambda(_, Patterns.Lambda _) -> failwithf "unsupported type %A" e.Type
    
        | ExprShape.ShapeLambda(x, e) ->
            let dom = TypeShape.Create x.Type
            let cod = TypeShape.Create e.Type
    
            dom.Accept {
                new ITypeVisitor<_> with
                    member _.Visit<'dom>() =
                        cod.Accept {
                            new ITypeVisitor<_> with
                                member _.Visit<'cod>() =
                                    Expr.NewDelegate(typeof<Func<'dom, 'cod>>, [x], normalize e)
                        }
            }
    
        | ExprShape.ShapeCombination(obj, es) ->
            ExprShape.RebuildShapeCombination(obj, List.map normalize es)

    let toLambdaExpression (e: Expr) : LambdaExpression = 
        normalize e |> LeafExpressionConverter.QuotationToExpression :?> LambdaExpression

    let private mkPredicate<'T> = mkLambda<'T>() |>> fun x -> x |> toLambdaExpression :?> Predicate<'T>
    
    let private mkOrderBy<'T> : Parser<OrderBy list, State> =
        let asc = stringCIReturn "ASC" true 
        let desc = stringCIReturn "DESC" false
        let expr = parse {
            let! selector = mkLambdaUntyped typeof<'T> .>> spaces |>> toLambdaExpression
            and! direction = desc <|> asc <|>% true .>> spaces
            return { Selector = selector; Ascending = direction }
        }

        skipStringCI "ORDER BY" >>. spaces >>. sepBy (expr .>> spaces) (pchar ',' .>> spaces)

    let private mkLinqExpression<'T> = parse {
        let! pred = 
            let defaultPredicate = parse {
                let! var = newParam typeof<'T>
                return Expr.Lambda(var, <@@ true  @@>) |> toLambdaExpression :?> Predicate<'T>
            }
            attempt mkPredicate<'T> <|> defaultPredicate
        
        and! ord  = mkOrderBy<'T> <|>% []
        
        return { Predicate = pred; OrderBy = ord }
    }

    let translate<'T> query = 
        runParserOnString (mkLinqExpression<'T> .>> eof) state "embeddable query" query

    let orderBy { Selector = selector; Ascending = asc } (source: IQueryable<'T>) =
        let shape = TypeShape.Create selector.ReturnType
        shape.Accept { new ITypeVisitor<_> with
            member _.Visit<'a>() =
                selector :?> Selector<'T, 'a>
                |> if asc then source.OrderBy else source.OrderByDescending }

    let thenBy { Selector = selector; Ascending = asc } (source: IOrderedQueryable<'T>) =
        let shape = TypeShape.Create selector.ReturnType
        shape.Accept { new ITypeVisitor<_> with
            member _.Visit<'a>() =
                selector :?> Selector<'T, 'a>
                |> if asc then source.ThenBy else source.ThenByDescending }

type [<Extension>] Linq =
    static member CreateQuery<'T>(query: string) =
        match Linq.translate<'T> query with
        | Success(v, _, _) -> v
        | Failure(e, _, _) -> failwith e

open type Linq

type Linq with
    [<Extension>]
    static member Apply(source: IQueryable<'TSource>, query: string) : IQueryable<'TSource> =
        let { Predicate = predicate; OrderBy = orderBy } = CreateQuery<'TSource> query
        source.Where(predicate).OrderBy(orderBy)
    
    [<Extension>]
    static member private OrderBy(source: IQueryable<'TSource>, expressions: OrderBy list) =
        match expressions with
        | []      ->  source
        | e :: es -> 
            let mutable res = source |> Linq.orderBy e
            for e in es do res <- res |> Linq.thenBy e
            upcast res