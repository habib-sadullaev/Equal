namespace Equal.Linq

open System
open System.Linq
open System.Linq.Expressions
open FSharp.Quotations
open FParsec
open TypeShape.Core
open Equal.Expression
open System.Runtime.CompilerServices

type QueryResult<'T> = 
    { Predicate: Expression<Func<'T, bool>>
      Order: struct(LambdaExpression * bool) list }

[<AutoOpen>]
module private Linq =
    type Selector<'T, 'R> = Expression<Func<'T, 'R>>
    type Predicate<'T> = Expression<Func<'T, bool>>
    
    let private toLambdaExpression (e: Expr) : LambdaExpression = failwith ""

    let predicate<'T> = 
        mkLambda<'T>() 
        |>> fun x -> x |> toLambdaExpression :?> Predicate<'T>

    let order<'T> : Parser<struct(LambdaExpression * bool) list, State> =
        let asc = stringCIReturn "ASC" true 
        let desc = stringCIReturn "DESC" false
        let direction = desc <|> asc <|>% true .>> spaces
        let expr = mkLambdaUntyped typeof<'T> .>> spaces

        sepBy (expr .>>. direction) (pchar ',' .>> spaces)
        |>> List.map (fun (x, d) -> struct(toLambdaExpression x, d))

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
    
    let thenBy (source: IOrderedQueryable<'T>) struct(selector: LambdaExpression, isAsc: bool) =
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
        | e :: es -> (source |> orderBy e, es) ||> Seq.fold thenBy :> IQueryable<_>
