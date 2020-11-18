module Equal.Expression

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.StagingExtensions
open Equal.Constant
open Equal.PropertyChain

let rec mkLambda<'T> () : Parser<'T -> bool> =
    match cache.TryFind() with
    | Some v -> v
    | None ->
        use ctx = cache.CreateGenerationContext()
        let delay (c: Cell<Parser<'T -> bool>>) = parse { return! c.Value }
        match ctx.InitOrGetCachedValue delay with
        | Cached(value = v) -> v
        | NotCached t ->
            let v = mkLambdaAux<'T> ctx
            ctx.Commit t v

and private mkLambdaAux<'T> (ctx: TypeGenerationContext) : Parser<'T -> bool> =
    let wrap (e: Expr<'a -> bool>) = unbox<Expr<'T -> bool>> e
    match shapeof<'T> with
    | Shape.Bool -> preturn ^ wrap ^ <@ fun x -> x @>

    | Shape.String ->
        parse { 
            let! cmp = stringComparison()
            let! rhs = mkConst()
            return wrap <@ fun lhs -> (%cmp) lhs %rhs @> }
    
    | Shape.Enumerable s ->
        s.Accept { new IEnumerableVisitor<_> with
            member _.Visit<'c, 'e when 'c :> 'e seq>() =
                let emptiness = parse { 
                    let! cmp = emptiness()
                    return wrap <@ fun (source : 'c) -> (%cmp) source @>
                }

                let existence = parse { 
                    let! cmp = existence()
                    let! pred = parenthesize ^ mkLambda()
                    return wrap <@ fun (source: 'c) -> (%cmp) %pred source @> 
                }

                emptiness <|> existence
        }
    
    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() = parse {
                let! cmp = mkLambda<'t> ()
                return wrap <@ fun (lhs: 't option) -> lhs.IsSome && (%cmp) lhs.Value @>
            }
        }

    | Shape.Poco _ ->
            parse { 
                let! param = newParam typeof<'T>
                let! body = mkComparison ^ Expr.Var param

                return Expr.Lambda(param, body) |> Expr.cast<'T -> bool>
            }

    | Shape.Comparison s ->
        s.Accept { new IComparisonVisitor<_> with
            member _.Visit<'t when 't: comparison>() =
                let comparison = parse {
                    let! cmp = numberComparison()
                    let! rhs = mkConst()
                    return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @>
                }

                let inclusion = parse {
                    let! cmp = inclusion()
                    let! rhs = mkConst()
                    return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @>
                }

                comparison <|> inclusion
        }

    | Shape.NullableComparison s ->
        s.Accept { new INullableComparisonVisitor<_> with
            member _.Visit<'t when 't : (new : unit -> 't)
                               and 't :> ValueType
                               and 't : struct
                               and 't : comparison>() =
                let comparison = parse { 
                   let! cmp = nullableComparison()
                   let! rhs = mkConst()
                   return wrap <@ fun (lhs: Nullable<'t>) -> (%cmp) lhs %rhs @> 
                }

                let inclusion = parse {
                    let! cmp = inclusion()
                    let! rhs = mkConst()
                    return wrap <@ fun (lhs: Nullable<'t>) -> (%cmp) lhs %rhs @>
                }

                comparison <|> inclusion
        }
    
    | _ -> unsupported typeof<'T>

and private cache : TypeCache = TypeCache()

and mkComparison param =
    mkLogicalChain ^ parse { 
        let! prop = mkPropChain param
        let shape = TypeShape.Create(prop.Type)
        return! shape.Accept { new ITypeVisitor<_> with
            override _.Visit<'t>() = 
                mkLambda<'t>() |>> fun f -> <@ (%f) %(Expr.Cast prop) @>
        }
    }

and mkLogicalChain parser =
    let mkOperation operator operand =
        operator .>> spaces |> chainl1 (operand .>> spaces)
    
    let operand, operandRef = createParserForwardedToRef()

    let operation = mkOperation OR (mkOperation AND operand)
    let nestedOperation = parenthesize operation
    let negation = NOT nestedOperation
    
    operandRef := choice [ negation; nestedOperation; parser ]
    
    operation |>> Expr.cleanup

let mkLambdaUntyped ty = parse { 
    let! param = newParam ty
    let var = Expr.Var param
    let prop = mkPropChain var .>> followedBy eof
    let cmp = mkComparison var |>> Expr.untyped
    let! body = attempt prop <|> cmp
    return Expr.Lambda(param, body)
}
