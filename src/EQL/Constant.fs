module EQL.Constant

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils

let private mkConstExpr (x: 'a) = x |> Expr.Value |> Expr.Cast<'a>

let rec mkConst<'T> () : Parser<'T> =
    let res =
        match cache.TryFind() with
        | Some x -> x
        | None ->
            use ctx = cache.CreateGenerationContext()
            mkConstCached<'T> ctx
    
    res |>> mkConstExpr

and private mkConstCached<'T> (ctx: TypeGenerationContext) : Parser<'T, State> =
    let delay (c: Cell<Parser<'T, State>>) = parse { return! c.Value }
    match ctx.InitOrGetCachedValue(delay) with
    | Cached(value = v) -> v
    | NotCached t ->
        let v = mkConstAux<'T> ctx
        ctx.Commit t v

and private mkConstAux<'T> (ctx: TypeGenerationContext) : Parser<'T, State> =
    let wrap (v: 'a) = unbox<'T> v
    let wrapWith (ctor: 'a -> 'b) = ctor >> wrap

    match shapeof<'T> with
    | Shape.Int32 -> pint32 |>> wrap
    | Shape.Double -> pfloat |>> wrap
    | Shape.String -> literal |>> wrap
    | Shape.Decimal -> pdecimal |>> wrap
    | Shape.DateTime -> pdatetime |>> wrap
    | Shape.DateTimeOffset -> pdatetimeoffset |>> wrap

    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<_> with
            member _.Visit<'t, 'u when 't : enum<'u>
                                   and 't : (new : unit -> 't)
                                   and 't : struct
                                   and 't :> ValueType>() =
                penum |>> (wrap : 't -> 'T)
        }

    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<_> with
            member _.Visit<'t when 't : (new : unit -> 't)
                               and 't :> ValueType
                               and 't : struct>() =
                mkConstCached<'t> ctx |>> wrapWith Nullable
        }

    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                mkConstCached<'t> ctx |>> wrapWith Some
        }

    | Shape.FSharpList s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                sepBy1 (mkConstCached<'t> ctx) (skipChar ',' .>> spaces)
                |> parenthesize
                |>> wrap
        }

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                mkConstCached<'t seq> ctx |>> wrapWith Array.ofSeq
        }

    | Shape.ResizeArray s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                mkConstCached<'t seq> ctx |>> wrapWith ResizeArray
        }

    | Shape.Enumerable s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                mkConstCached<'t list> ctx |>> wrapWith ^ fun vs -> seq { for v in vs -> v }
        }

    | x -> fail ^ sprintf "unsupported type %A" x.Type

and private cache : TypeCache = TypeCache()

