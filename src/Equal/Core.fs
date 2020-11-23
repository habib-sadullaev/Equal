[<AutoOpen>]
module Core

open System
open FSharp.Quotations
open FParsec

#nowarn "86"
#nowarn "42"

let (^) f x = f x

let inline (=) (x: 'T) (y: 'T) =
    if Type.(<>)(typeof<'T>, typeof<Type>) then x = y else
    Type.(=)((# "" x : Type #), (# "" y : Type #))

let inline (<>) (x: 'T) (y: 'T) =
    if Type.(<>)(typeof<'T>, typeof<Type>) then x <> y else
    Type.(<>)((# "" x : Type #), (# "" y : Type #))

let unsupported (ty: Type) = fail ^ sprintf "unsupported type %A" ty

let inline constExpr (x: 'a) = x |> Expr.Value |> Expr.Cast<'a>

type State = int

type Parser<'T> = FParsec.Primitives.Parser<Expr<'T>, State>

let state : State = 0

let newParam ty : Parser<_, State> =
    parse { let! x = getUserState
            let param = Var(sprintf "Param_%i" x, ty)
            do! updateUserState ^ (+) 1
            return param }
