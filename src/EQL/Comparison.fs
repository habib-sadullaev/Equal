module EQL.Comparison

open System
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

let inclusion () =
    choice [
        stringCIReturn "IN"     <@ Array.contains @>
        stringCIReturn "NOT IN" <@ fun x y -> not (Array.contains x y) @>
    ] .>> spaces

let (|Array|List|Seq|) (ty: Type) =
    if ty.IsArray then Array
    else if ty = typedefof<_ list> then List
    else Seq

let emptiness : unit -> Parser<Expr<'c -> bool>, 'u> when 'c :> 'e seq =
    let isEmpty src  =
        match typedefof<'c> with
        | Seq -> <@ Seq.isEmpty %src @>
        | List -> <@ List.isEmpty %(unbox<Expr<'e list>> src) @>
        | Array -> <@ Array.isEmpty %(unbox<Expr<'e array>> src) @>

    fun () -> stringCIReturn "IS EMPTY" ^ Expr.lam isEmpty

let existence =
    let exists (pred: Expr<'e -> bool>) (src: Expr<'c>) =
        match typedefof<'c> with
        | Seq -> <@ Seq.exists %pred %src @>
        | List -> <@ List.exists %pred %(unbox src) @>
        | Array -> <@ Array.exists %pred %(unbox src) @>

    let forall (pred: Expr<'e -> bool>) (src: Expr<'c>) =
        match typedefof<'c> with
        | Seq -> <@ Seq.forall %pred %src @>
        | List -> <@ List.forall %pred %(unbox src) @>
        | Array -> <@ Array.forall %pred %(unbox src) @>

    fun () ->
        choice [
            stringCIReturn "ANY" ^ Expr.lam2 exists
            stringCIReturn "ALL" ^ Expr.lam2 forall
        ] .>> spaces

let rec mkLambda<'T> () : Parser<Expr<'T>, State> =
    let shape = shapeof<'T>
    match shape with
    | Shape.FSharpFunc s ->
        parse { let! param = newParam s.Domain.Type
                let! body = mkComparison ^ Expr.Var param

                return Expr.Lambda(param, body) |> Expr.cast<'T> |> Expr.cleanup }

    | _ -> unsupported typeof<'T>

and mkComparison param =
    mkPropChain param >>= mkComparisonAux |>> Expr.cleanup

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
    
    | _ -> unsupported prop.Type
