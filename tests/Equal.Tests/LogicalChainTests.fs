module LogicalChainTests

open FSharp.Quotations
open Expecto
open FParsec
open Equal.Expression

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
        
        "NOT(A)"                        |> parsedInto <@ not %A @>
        "NOT(A) AND B"                  |> parsedInto <@ not %A && %B @>
        "NOT(A) AND NOT(B)"             |> parsedInto <@ not %A && not %B @>
        "not (A) OR B"                  |> parsedInto <@ not %A || %B @>
        "not ( A )  OR   not ( B )"     |> parsedInto <@ not %A || not %B @>
        "NOT(A OR B) AND (C OR D)"      |> parsedInto <@ not(%A || %B) && (%C || %D) @>
        "NOT(NOT(A OR B) AND (C OR D))" |> parsedInto <@ not(not(%A || %B) && (%C || %D)) @>

        ""       |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
        " "      |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
        "()"     |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
        "z"      |> failedWith { position = 1L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
        "A && B" |> failedWith { position = 3L; errors = [ "AND"; "OR"; "end of input" ] }
        "NOT A"  |> failedWith { position = 5L; errors = [ "(" ] }
        "A OR"   |> failedWith { position = 5L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
        "A AND"  |> failedWith { position = 6L; errors = "(" :: List.map string [ 'A' .. 'D' ] @ [ "NOT" ] }
    ]