module EQL.Expression

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.StagingExtensions
open EQL.Constant
open EQL.PropertyChain

let rec mkLambda<'T> () : Parser<Expr<'T>, State> =
    let shape = shapeof<'T>
    match shape with
    | Shape.FSharpFunc s ->
        parse { let! param = newParam s.Domain.Type
                let! body = mkComparison ^ Expr.Var param

                return Expr.Lambda(param, body) |> Expr.cast<'T> |> Expr.cleanup }

    | _ -> unsupported typeof<'T>

and mkComparison param =
    mkPropChain param >>= mkComparisonAux 
    |> mkLogicalChain

and mkLogicalChain parser =
    let mkOperation operator operand =
        operator .>> spaces |> chainl1 (operand .>> spaces)
    
    let operand, operandRef = createParserForwardedToRef()

    let operation = mkOperation OR (mkOperation AND operand)
    let nestedOperation = parenthesize operation
    let negate = NOT nestedOperation
    
    operandRef := choice [ negate; nestedOperation; parser ]
    
    operation |>> Expr.cleanup

and mkComparisonAux prop =
    match TypeShape.Create prop.Type with
    | Shape.Bool -> preturn ^ Expr.cast prop

    | Shape.String ->
        parse { let lhs = Expr.cast prop
                let! cmp = stringComparison()
                let! rhs = mkConst()
                return <@ (%cmp) %lhs %rhs @> }
    
    | Shape.Enumerable s ->
        s.Accept { new IEnumerableVisitor<_> with
            member _.Visit<'c, 'e when 'c :> 'e seq>() =
                let source = Expr.cast<'c> prop

                let emptiness = parse { 
                    let! cmp = emptiness()
                    return <@ (%cmp) %source @>
                }

                let existence = parse { 
                    let! cmp = existence()
                    let! pred = parenthesize ^ mkLambda()
                    return <@ (%cmp) %pred %source @> 
                }

                emptiness <|> existence
        }

    | Shape.Comparison s ->
        s.Accept { new IComparisonVisitor<_> with
            member _.Visit<'t when 't: comparison>() =
                let lhs = Expr.cast<'t> prop

                let comparison = parse {
                    let! cmp = numberComparison()
                    let! rhs = mkConst()
                    return <@ (%cmp) %lhs %rhs @>
                }

                let inclusion = parse {
                    let! cmp = inclusion()
                    let! rhs = mkConst()
                    return <@ (%cmp) %lhs %rhs @>
                }

                comparison <|> inclusion
        }

    | Shape.NullableComparison s ->
        s.Accept { new INullableComparisonVisitor<_> with
            member _.Visit<'t when 't : (new : unit -> 't)
                               and 't :> ValueType
                               and 't : struct
                               and 't : comparison>() =
                let lhs = Expr.cast<Nullable<'t>> prop
                
                let comparison = parse { 
                   let! cmp = nullableComparison()
                   let! rhs = mkConst()
                   return <@ (%cmp) %lhs %rhs @> 
                }

                let inclusion = parse {
                    let! cmp = inclusion()
                    let! rhs = mkConst()
                    return <@ (%cmp) %lhs %rhs @>
                }

                comparison <|> inclusion
        }
    
    | _ -> unsupported prop.Type
