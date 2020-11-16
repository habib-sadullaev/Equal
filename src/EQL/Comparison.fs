module EQL.Comparison

open FParsec
open TypeShape.Core.StagingExtensions
open EQL.PropertyChain

let mkComparison param =
    parse { let! res = mkPropChain param
            if res.Type = typeof<bool> 
            then return  Expr.cast<bool> res 
            else return! unsupported res.Type }