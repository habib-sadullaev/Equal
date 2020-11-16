module EQL.Comparison

open FSharp.Quotations
open FParsec
open TypeShape.Core
open TypeShape.Core.StagingExtensions
open EQL.Constant
open EQL.PropertyChain

let stringComparison () : Parser<Expr<string -> string -> bool>, 'u> =
    choice [
        stringCIReturn "STARTS WITH" <@ fun x y -> x.StartsWith y @>
        stringCIReturn "CONTAINS"    <@ fun x y -> x.Contains   y @>
        stringCIReturn "ENDS WITH"   <@ fun x y -> x.EndsWith   y @>
    ] .>> spaces

let numberComparison ()  =
    choice [
        stringReturn "="  <@ (=)  @>
        stringReturn "<>" <@ (<>) @>
        stringReturn "<=" <@ (<=) @>
        stringReturn "<"  <@ (<)  @>
        stringReturn ">=" <@ (>=) @>
        stringReturn ">"  <@ (>)  @>
    ] .>> spaces

let rec mkComparison param =
    mkPropChain param >>= mkComparisonAux |>> Expr.cleanup

and mkComparisonAux prop =
    match TypeShape.Create prop.Type with
    | Shape.Bool -> preturn ^ Expr.cast prop

    | Shape.String ->
        parse { let lhs = Expr.cast prop
                let! cmp = stringComparison()
                let! rhs = mkConst()
                return <@ (%cmp) %lhs %rhs @> }

    | Shape.Comparison s ->
        s.Accept { new IComparisonVisitor<_> with
            member _.Visit<'t when 't: comparison>() =
                let lhs = Expr.cast<'t> prop

                let comparison = parse {
                    let! cmp = numberComparison()
                    let! rhs = mkConst()
                    return <@ (%cmp) %lhs %rhs @>
                }

                comparison
        }

    | _ -> unsupported prop.Type
