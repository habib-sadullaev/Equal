[<AutoOpen>]
module Core

open System
open FSharp.Quotations
open FParsec

#nowarn "86"
#nowarn "42"

let (^) f x = f x

let inline (=) (x: 'T) (y: 'T) =
    if Type.(=)(typeof<'T>, typeof<Type>) then
        Type.(=)((# "" x : Type #), (# "" y : Type #))
    else 
        x = y

let inline (<>) (x: 'T) (y: 'T) =
    if Type.(=)(typeof<'T>, typeof<Type>) then
        Type.(<>)((# "" x : Type #), (# "" y : Type #))
    else 
        x <> y

type State = State
type Parser<'T> = FParsec.Primitives.Parser<Expr<'T>, State>

let state = State

let unsupported (ty: Type) = fail ^ sprintf "unsupported type %A" ty

let inline constExpr (x: 'a) = x |> Expr.Value |> Expr.Cast<'a>
