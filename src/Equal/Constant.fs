module internal Equal.Constant

open System
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils

// const-array = '(' const { ',' const } ')'

// const       = ( number | enum | datetime | literal )

// number      = ( int | float | decimal )
// enum        = ( number | string )
// datetime    = literal
// literal     = '''' string { '''' string } '''' (* e.g. '6 o''clock' *)
let rec mkConst<'T> () : Parser<'T> =
    match cache.TryFind() with
    | Some x -> x
    | None ->
        use ctx = cache.CreateGenerationContext()
        mkConstCached<'T> ctx |>> constExpr

and private mkConstCached<'T> (ctx: TypeGenerationContext) : Parser<'T, State> =
    let delay (c: Cell<Parser<'T, State>>) s = c.Value s
    match ctx.InitOrGetCachedValue(delay) with
    | Cached(value = v) -> v
    | NotCached t ->
        let v = mkConstAux<'T> ctx
        ctx.Commit t v

and private mkConstAux<'T> (ctx: TypeGenerationContext) : Parser<'T, State> =
    let wrap (v: 'a) = unbox<'T> v
    let wrapWith (ctor: 'a -> 'b) = ctor >> wrap

    match shapeof<'T> with
    // number
    | Shape.Int32 -> pint32 |>> wrap
    | Shape.Double -> pfloat |>> wrap
    | Shape.Decimal -> pdecimal |>> wrap
    
    // literal
    | Shape.String -> literal |>> wrap
    
    // dateTime 
    | Shape.DateTime -> pdatetime |>> wrap
    | Shape.DateTimeOffset -> pdatetimeoffset |>> wrap

    // enum
    | Shape.Enum s ->
        s.Accept { new IEnumVisitor<_> with
            member _.Visit<'t, 'u when 't : enum<'u>
                                   and 't : (new : unit -> 't)
                                   and 't : struct
                                   and 't :> ValueType>() =
                penum |>> (wrap : 't -> 'T)
        }
    
    // const-array
    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() =
                sepBy1 (mkConstCached<'t> ctx) (skipChar ',' .>> spaces)
                |> parenthesize
                |>> wrapWith Array.ofSeq
        }
    
    | _ -> unsupported typeof<'T>

and private cache : TypeCache = TypeCache()
