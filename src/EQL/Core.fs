[<AutoOpen>]
module Core

open FSharp.Quotations

let (^) f x = f x

type State = State
type Parser<'T> = FParsec.Primitives.Parser<Expr<'T>, State>

let state = State

