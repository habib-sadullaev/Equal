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

type private Selector<'T, 'R> = Expression<Func<'T, 'R>>
type private Predicate<'T>    = Selector<'T, bool>

type [<Struct>] OrderBy         = { Selector: LambdaExpression; Ascending: bool }
type [<Struct>] QueryResult<'T> = { Predicate: Predicate<'T>; OrderBy: OrderBy list }

[<RequireQualifiedAccess>]
module private Linq =
    let (|TypeShape|) (e: Expr) = TypeShape.Create e.Type

    let (|MethodCall|_|) m =
        function DerivedPatterns.SpecificCall m (None, _, args) -> Some args | _ -> None

    let rec normalize (e: Expr) : Expr =
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
    
        | ExprShape.ShapeLambda(_, Patterns.Lambda _) -> unsupported e.Type
    
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

    let mkLinqExpression<'T> : Parser<QueryResult<'T>, State> =
        let orderBy = skipStringCI "ORDER BY" >>. spaces
        
        let predicate =
            fun stream ->
                // check that there is no predicate expression
                // query of the form 'order by ...'
                let orderBy = lookAhead orderBy stream
                
                // if so return '_ => true'
                if orderBy.Status = Ok || stream.IsEndOfStream then
                    let var = (newParam typeof<'T> stream).Result
                    Reply(Expr.Lambda(var, <@@ true @@>) |> Expr.Cast)
                else
                    let mutable predicate = mkLambda<'T>() stream
                    
                    // StateTag = 2u means that only the first parser 
                    // (property expression parser) was applied and need to merge errors
                    if predicate.Status <> Ok && stream.StateTag = 2u then 
                        predicate.Error <- mergeErrors orderBy.Error predicate.Error

                    predicate
        
        let orderBy =
            let asc  = stringCIReturn "ASC" true 
            let desc = stringCIReturn "DESC" false

            let expr = parse {
                let! selector = mkLambdaUntyped typeof<'T> .>> spaces
                and! direction = desc <|> asc <|>% true .>> spaces
                return { Selector = toLambdaExpression selector; Ascending = direction }
            }

            orderBy >>. sepBy (expr .>> spaces) (pchar ',' .>> spaces) <|>% []

        parse { let! pred = predicate
                and! ord  = orderBy
                return { Predicate = downcast toLambdaExpression pred
                         OrderBy = ord } }

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

#if TESTS_FRIENDLY
[<RequireQualifiedAccess>]
module internal EmbeddedQuery =
    open TypeShape.Core.StagingExtensions
    let quote(e: Expr<'T -> 'R>) = Expr.cleanup e |> Linq.toLambdaExpression :?> Expression<Func<'T, 'R>>

    let mkLinqExpression<'T> = Linq.mkLinqExpression<'T>
#endif

type [<Extension>] EmbeddedQuery =
    static member CreateQuery<'T>(query: string) =
        match Linq.translate<'T> query with
        | Success(v, _, _) -> v
        | Failure(e, _, _) -> failwith e

    [<Extension>]
    static member Apply(source: IQueryable<'TSource>, query: string) : IQueryable<'TSource> =
        let { Predicate = predicate; OrderBy = orderBy } = EmbeddedQuery.CreateQuery<'TSource> query
        source.Where(predicate).OrderBy(orderBy)
    
    [<Extension>]
    static member private OrderBy(source: IQueryable<'TSource>, expressions: OrderBy list) =
        match expressions with
        | []      ->  source
        | e :: es -> 
            let mutable res = source |> Linq.orderBy e
            for e in es do res <- res |> Linq.thenBy e
            upcast res