[<AutoOpen>]
module Core

open System
open FSharp.Quotations
open FParsec

let inline (^) f x = f x

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
