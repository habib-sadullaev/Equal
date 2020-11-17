module Equal.PropertyChain

open FParsec
open FSharp.Quotations

let rec mkPropChain (instance: Expr) : Parser<Expr, State> =
    let error = Reply(Error, expectedString ^ sprintf "property of %A" instance.Type)
    fun s ->
        let initState = s.State
        let label = ident s
        match label.Status with
        | Ok ->
            match instance.Type.GetProperty label.Result with
            | null ->
                s.BacktrackTo initState
                error

            | prop ->
                let next = Expr.PropertyGet(instance, prop)
                if s.Skip '.' then mkPropChain next s else Reply next

        | _ -> error