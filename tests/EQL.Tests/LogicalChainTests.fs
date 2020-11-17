module LogicalChainTests

open FSharp.Quotations
open Expecto
open FParsec
open EQL.Expression


let wrap str = Var(str, typeof<bool>) |> Expr.Var |> Expr.Cast<bool>

let A = wrap "A"
let B = wrap "B"
let C = wrap "C"
let D = wrap "D"
let E = wrap "E"

let logicalParser = 
    mkLogicalChain ^ choice [
        stringReturn "A" A
        stringReturn "B" B
        stringReturn "C" C
        stringReturn "D" D
    ] .>> spaces
    

let inline parsedInto expected input =
    let name = sprintf "parses '%s'" input
    let expected = expected
    test name { parsed logicalParser expected input } |> testLabel "with valid input"

let inline failedWith expected input =
    let name = sprintf "fails parsing '%s'" input
    test name { failed logicalParser expected input } |> testLabel "with invalid input"

[<Tests>]
let tests =
    testList "logical chain parser" [
        "A"                     |> parsedInto A
        "A AND B OR C AND D"    |> parsedInto <@  %A && %B  ||  %C && %D  @>
        "A OR B AND C OR D"     |> parsedInto <@  %A || %B  &&  %C || %D  @> 
        "A OR B AND (C OR D)"   |> parsedInto <@  %A || %B  && (%C || %D) @>
        "(A OR B) AND C OR D"   |> parsedInto <@ (%A || %B) &&  %C || %D  @>
        "(A OR B) AND (C OR D)" |> parsedInto <@ (%A || %B) && (%C || %D) @>

        ""       |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] }
        "()"     |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] }
        "z"      |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] }
        "A && B" |> failedWith { position = 3L; errors = [ "AND"; "OR"; "end of input" ] }
    ]