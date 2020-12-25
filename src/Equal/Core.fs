[<AutoOpen>]
#if TESTS_FRIENDLY 
// change the 'Core' module accessibility to public 
// to prevent compilation errors because it has inline functions
module Core

//... and make the internals visible to tests
module AssemblyInfo =
    [<assembly: System.Runtime.CompilerServices.InternalsVisibleTo "Equal.Tests">] do()
#else
module internal Core
#endif

open System
open FSharp.Quotations
open FParsec

let inline (^) f x = f x

let inline unsupported (ty: Type) = failwithf "unsupported type %A" ty

let inline constExpr (x: 'a) = x |> Expr.Value |> Expr.Cast<'a>

type State = int

type Parser<'T> = Primitives.Parser<Expr<'T>, State>

let state : State = 0

let newParam ty : Parser<_, State> =
    parse { let! x = getUserState
            let param = Var(sprintf "Param_%i" x, ty)
            do! updateUserState ^ (+) 1
            return param }