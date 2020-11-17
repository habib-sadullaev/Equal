[<AutoOpen>]
module Operators

open System
open FSharp.Quotations
open FSharp.Linq.NullableOperators
open FParsec
open TypeShape.Core.StagingExtensions

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

let nullableComparison () =
    choice [
        stringReturn "="  <@ (?=?)  @>
        stringReturn "<>" <@ (?<>?) @>
        stringReturn "<=" <@ (?<=?) @>
        stringReturn "<"  <@ (?<?)  @>
        stringReturn ">=" <@ (?>=?) @>
        stringReturn ">"  <@ (?>?)  @>
    ] .>> spaces

let mkOperator name operator = stringCIReturn name operator .>> spaces

let AND : Parser<_, State> = mkOperator "AND" ^ fun lhs rhs -> <@ %lhs && %rhs @>
let OR  : Parser<_, State> = mkOperator "OR"  ^ fun lhs rhs -> <@ %lhs || %rhs @>