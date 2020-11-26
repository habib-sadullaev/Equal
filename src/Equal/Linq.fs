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
module Linq =
    type Predicate<'T> = Expression<Func<'T, bool>>
    type Selector<'T, 'R> = Expression<Func<'T, 'R>>
    
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

    let toLambdaExpression (e: Expr) : Expression = 
        normalize e |> LeafExpressionConverter.QuotationToExpression 

    let private predicate<'T> = 
        mkLambda<'T>() 
        |>> fun x -> x |> toLambdaExpression :?> Predicate<'T>
    
    let [<Literal>] maxChars = Int32.MaxValue
    let private skipUntilFound str (stream: CharStream<State>) =
        let init = stream.State
        let mutable found = false
        stream.SkipCharsOrNewlinesUntilCaseFoldedString(str, maxChars, &found) |> ignore
        if not found then stream.BacktrackTo init
        found

    let private order<'T> : Parser<struct(LambdaExpression * bool) list, State> =
        let asc = stringCIReturn "ASC" true 
        let desc = stringCIReturn "DESC" false
        let direction = desc <|> asc <|>% true .>> spaces
        let expr = mkLambdaUntyped typeof<'T> .>> spaces

        let (|Splitted|EOF|) =
            let ascending = Text.FoldCase("ASC")
            let descending = Text.FoldCase("DESC")
            fun (stream: CharStream<State>) ->
                if   stream |> skipUntilFound ascending  then Splitted
                elif stream |> skipUntilFound descending then Splitted
                elif stream |> skipUntilFound "," then Splitted
                else EOF

        let expr =
            fun (stream: CharStream<State>) ->
                let init = stream.State
                match stream with
                | Splitted ->
                    use substream = stream.CreateSubstream(init)
                    let reply1 = expr substream
                    if reply1.Status = Ok then
                        let reply2 = direction stream
                        if reply2.Status = Ok then
                            Reply(struct(reply1.Result, reply2.Result))
                        else
                            Reply(reply2.Status, reply2.Error)
                    else
                        Reply(reply1.Status, reply1.Error)
                | EOF ->
                    let reply1 = expr stream
                    if reply1.Status = Ok then
                        Reply(struct(reply1.Result, true))
                    else
                        Reply(reply1.Status, reply1.Error)

        sepBy (expr .>> spaces) (pchar ',' .>> spaces)
        |>> List.map (fun struct (x, d) -> struct(downcast toLambdaExpression x, d))

    let private mkLinqExpression<'T> =
        let p1 = predicate<'T> |>> fun p -> { Predicate = p; Order = [] }
        let p2 = order<'T>
        let orderBy = Text.FoldCase("ORDER BY ")
        
        fun (stream: CharStream<State>) ->
            let init = stream.State
            if stream |> skipUntilFound orderBy then
                use substream = stream.CreateSubstream(init)
                let reply1 = p1 substream
                if reply1.Status = ReplyStatus.Ok then 
                    stream.SkipCaseFolded(orderBy) |> ignore
                    let reply2 = p2 stream
                    if reply2.Status = Ok then
                        Reply { reply1.Result with Order = reply2.Result }
                    else
                        Reply(reply2.Status, reply2.Error)
                else
                    Reply(reply1.Status, reply1.Error)
            else
                p1 stream

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