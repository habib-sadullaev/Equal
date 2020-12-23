module Equal.Expression

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.StagingExtensions
open Equal.Constant

let private cache = TypeCache()

let rec mkLambda<'T> () : Parser<'T -> bool> =
    match cache.TryFind() with
    | Some v -> v
    | None ->
        use ctx = cache.CreateGenerationContext()
        mkLambdaCached ctx

and mkLambdaCached<'T> (ctx: TypeGenerationContext) : Parser<'T -> bool> =
        let delay (c: Cell<Parser<'T -> bool>>) s = c.Value s
        match ctx.InitOrGetCachedValue delay with
        | Cached(value = v) -> v
        | NotCached t ->
            let v = mkLambdaAux ctx
            ctx.Commit t v

and private mkLambdaAux<'T> (ctx: TypeGenerationContext) : Parser<'T -> bool> =
    let wrap (e: Expr<'a -> bool>) = unbox<Expr<'T -> bool>> e
    match shapeof<'T> with
    | Shape.Bool -> preturn ^ wrap <@ fun x -> x @>

    | Shape.String ->
        parse { let! cmp = stringComparison()
                and! rhs = mkConst()
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
                    and! pred = parenthesize ^ mkLambdaCached ctx
                    return wrap <@ fun (source: 'c) -> (%cmp) %pred source @> 
                }

                emptiness <|> existence
        }
    
    | Shape.FSharpOption s ->
        s.Element.Accept { new ITypeVisitor<_> with
            member _.Visit<'t>() = parse {
                let! cmp = mkLambdaCached ctx
                return wrap <@ fun (lhs: 't option) -> lhs.IsSome && (%cmp) lhs.Value @>
            }
        }
    
    | Shape.Nullable s ->
        s.Accept { new INullableVisitor<_> with
            member _.Visit<'t when 't : (new : unit -> 't) and 't :> ValueType and 't : struct>() = parse {
                let! cmp = mkLambdaCached ctx
                return wrap <@ fun (lhs: 't Nullable) -> lhs.HasValue && (%cmp) lhs.Value @>
            }
        }

    | Shape.Poco _ ->
        parse { 
            let! param = newParam typeof<'T>
            let! body = mkComparison (Expr.Var param)
            return Expr.Lambda(param, body) |> Expr.cast<'T -> bool>
        }
    
    | Shape.Comparison s ->
        s.Accept { new IComparisonVisitor<_> with
            member _.Visit<'t when 't: comparison>() =
                let comparison = parse {
                    let! cmp = numberComparison()
                    and! rhs = mkConst()
                    return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @>
                }

                let inclusion = parse {
                    let! cmp = inclusion()
                    and! rhs = mkConst()
                    return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @>
                }

                comparison <|> inclusion
        }

    | _ -> unsupported typeof<'T>

and mkComparison param =
    mkPropChain param >>= fun prop -> 
        TypeShape.Create(prop.Type).Accept { 
            new ITypeVisitor<_> with
                override _.Visit<'t>() = parse { 
                    let! cmp = mkLambda<'t>() 
                    return <@ (%cmp) %(Expr.Cast prop) @>
                }
        }
    |> mkLogicalChain

and mkPropChain (instance: Expr) : Parser<Expr, State> =
    let error = Reply(Error, expectedString ^ sprintf "property of %A" instance.Type)
    fun stream ->
        let initState = stream.State
        let label = ident stream
        match label.Status with
        | Ok ->
            match instance.Type.GetProperty label.Result with
            | null ->
                stream.BacktrackTo initState
                error

            | prop ->
                let next = Expr.PropertyGet(instance, prop)
                if stream.Skip '.' then mkPropChain next stream else Reply next

        | _ -> error

and mkLogicalChain parser =
    let mkOperation operator operand =
        operator .>> spaces |> chainl1 (operand .>> spaces)
    
    let operand, operandRef = createParserForwardedToRef()

    let operation = mkOperation OR (mkOperation AND operand)
    let nestedOperation = parenthesize operation
    let negation = NOT nestedOperation
    
    operandRef := choice [ negation; nestedOperation; parser ]
    
    operation |>> Expr.cleanup

let mkLambdaUntyped ty = 
    fun stream ->
        let param = (newParam ty stream).Result
        let var  = Expr.Var param
        let prop = mkPropChain var  |>> fun prop -> Expr.Lambda(param, prop)
        let cmp  = mkComparison var |>> fun cmp  -> Expr.Lambda(param,  cmp)
        
        // first try to parse the input using the comparison parser
        let init = stream.State
        let mutable reply = cmp stream

        // and if it doesn't work, try to parse it using the property parser
        if reply.Status <> Ok then
            use substream = stream.CreateSubstream init
            let propReply = prop substream
            reply.Result <- propReply.Result
            reply.Status <- propReply.Status

            // 2u difference between the property and comparison parser
            // means they parse the same part of the input
            // and need to combine errors from both of them
            if stream.StateTag - substream.StateTag = 2u then 
                reply.Error <- mergeErrors propReply.Error reply.Error
        
        reply