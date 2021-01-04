module internal Equal.Expression

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.Utils
open TypeShape.Core.StagingExtensions
open Equal.Constant

// expr
let rec mkLambda<'T> () : Parser<'T -> bool> =
    match cache.TryFind() with
    | Some v -> v
    | None ->
        use ctx = cache.CreateGenerationContext()
        mkLambdaCached ctx

// expr
and mkLambdaCached<'T> (ctx: TypeGenerationContext) : Parser<'T -> bool> =
        let delay (c: Cell<Parser<'T -> bool>>) s = c.Value s
        match ctx.InitOrGetCachedValue delay with
        | Cached(value = v) -> v
        | NotCached t ->
            let v = mkLambdaAux ctx
            ctx.Commit t v

// expr
and private mkLambdaAux<'T> (ctx: TypeGenerationContext) : Parser<'T -> bool> =
    let wrap (e: Expr<'a -> bool>) = unbox<Expr<'T -> bool>> e
    match shapeof<'T> with
    // bool = prop-chain
    | Shape.Bool -> 
        preturn ^ wrap <@ fun x -> x @>
    
    // string-comparison = prop-chain operator literal
    // operator          = ( 'STARTS WITH' | 'CONTAINS' | 'ENDS WITH' )
    | Shape.String ->
        parse { let! cmp = stringComparison()
                and! rhs = mkConst()
                return wrap <@ fun lhs -> (%cmp) lhs %rhs @> }

    // collection-comparison = ( emptiness | existence ) 
    | Shape.Enumerable shape ->
        shape.Accept { 
            new IEnumerableVisitor<_> with
                member _.Visit<'c, 'e when 'c :> 'e seq>() =
                    // emptiness = prop-chain 'IS EMPTY'
                    let emptiness = parse { 
                        let! cmp = emptiness()
                        return wrap <@ fun (source : 'c) -> (%cmp) source @>
                    }

                    // existence = prop-chain operator predicate
                    // operator  = ( 'ANY' | 'ALL' )
                    // predicate = '(' expr ')'
                    let existence = parse { 
                        let! cmp = existence()
                        and! pred = parenthesize ^ mkLambdaCached ctx
                        return wrap <@ fun (source: 'c) -> (%cmp) %pred source @> 
                    }

                    emptiness <|> existence
        }
    
    // option-comparison = expr
    | Shape.FSharpOption shape -> 
        shape.Element.Accept { 
            new ITypeVisitor<_> with
                member _.Visit<'t>() = parse {
                    let! cmp = mkLambdaCached ctx
                    return wrap <@ fun (lhs: 't option) -> lhs.IsSome && (%cmp) lhs.Value @>
                }
        }
    
    // nullable-comparison = common-comparison
    | Shape.Nullable shape ->
        shape.Accept { 
            new INullableVisitor<_> with
                member _.Visit<'t when 't : (new : unit -> 't)
                                   and 't :> ValueType 
                                   and 't : struct>() = parse {
                    let! cmp = mkLambdaCached ctx
                    return wrap <@ fun (lhs: 't Nullable) -> lhs.HasValue && (%cmp) lhs.Value @> 
                } 
        }

    // common-comparison = ( arithmetic-comparison | inclusion )
    | Shape.Struct _ & Shape.Comparison shape ->
        shape.Accept { 
            new IComparisonVisitor<_> with
                member _.Visit<'t when 't: comparison>() =
                    // arithmetic-comparison = prop-chain operator value
                    // operator              = ( '>' | '>=' | '=' | '<>' | '<=' | '<' )
                    // value                 = ( number | enum | datetime )
                    let arithmeticComparison = parse {
                        let! cmp = arithmeticComparison()
                        and! rhs = mkConst()
                        return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @> 
                    }

                    // inclusion = prop-chain operator const-array
                    // operator  = ( 'IN' | 'NOT IN' )
                    let inclusion = parse {
                        let! cmp = inclusion()
                        and! rhs = mkConst()
                        return wrap <@ fun (lhs: 't) -> (%cmp) lhs %rhs @>
                    }

                    arithmeticComparison <|> inclusion
        }

    // entry point
    // expr        = node { operator node }
    // node        = ( negation | nested-expr | comparison )
    // negation    = 'NOT' nested-expr
    // nested-expr = '(' expr ')'
    // comparison  = ( string-comparison | collection-comparison | nullable-comparison | option-comparison | common-comparison | bool )
    // operator    = ( 'OR' | 'AND' ) 
    | Shape.Poco _ ->
        parse { let! param = newParam typeof<'T>
                let! body = mkLogicalChain ^ mkComparison ^ Expr.Var param
                return Expr.Lambda(param, body) |> Expr.cast<'T -> bool> }

    | _ -> unsupported typeof<'T>

and private cache : TypeCache = TypeCache()

// comparison  = ( string-comparison | collection-comparison | nullable-comparison | option-comparison | common-comparison | bool )
and mkComparison param = 
    parse {
        let! prop = mkPropChain param
        return! TypeShape.Create(prop.Type).Accept { 
            new ITypeVisitor<_> with
                override _.Visit<'t>() = parse {
                    let! cmp = mkLambda<'t>() 
                    return Expr.cleanup <@ (%cmp) %(Expr.Cast prop) @>
                }
        }
    }

// prop-chain = prop { '.' prop }
// prop       = string
and mkPropChain (instance: Expr) : Parser<Expr, State> =
    let ty = instance.Type
    
    let expectedProps =
        match ty.GetProperties() with
        | [||]      -> 
            Reply(Error, unexpected ^ sprintf "%A has no props" ty)

        | propInfos ->
            let mutable errors = NoErrorMessages
            for p in propInfos do 
                if p.GetIndexParameters().Length = 0 then 
                    errors <- mergeErrors errors ^ expectedString p.Name 
            Reply(Error, errors)

    fun stream ->
        let initState = stream.State
        let label = ident stream
        match label.Status with
        | Ok ->
            match ty.GetProperty label.Result with
            | null ->
                stream.BacktrackTo initState
                expectedProps

            | prop ->
                let next = Expr.PropertyGet(instance, prop)
                if stream.Skip '.' then mkPropChain next stream else Reply next

        | _ -> expectedProps

// operation         = operand { operator operand }
// operand           = ( negation | nested-operation | bool-expr )
// negation          = 'NOT' nested-operation
// nested-operation  = '(' operation ')'
// operator          = ( 'OR' | 'AND' )
and mkLogicalChain parser =
    // operation = operand { operator operand }
    let mkOperation operator operand =
        operator .>> spaces |> chainl1 (operand .>> spaces)
    
    let operand, operandRef = createParserForwardedToRef()

    let operation = mkOperation OR (mkOperation AND operand)
    let nestedOperation = parenthesize operation
    let negation = NOT nestedOperation
    
    // operand = ( negation | nested-operation | bool-expr )
    operandRef := choice [ negation; nestedOperation; parser ]
    
    operation |>> Expr.cleanup

/// creates a strongly typed expression but its type is resolved at runtime
// untyped-expr = ( expr | prop-chain )
let mkLambdaUntyped ty = 
    fun stream ->
        let param = (newParam ty stream).Result
        let var  = Expr.Var param
        let prop = mkPropChain var .>> eof |>> fun prop -> Expr.Lambda(param, prop)
        let cmp  = mkLogicalChain ^ mkComparison var |>> fun cmp  -> Expr.Lambda(param,  cmp)
        
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