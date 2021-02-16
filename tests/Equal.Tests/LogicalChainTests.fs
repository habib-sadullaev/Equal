module LogicalChainTests

open FSharp.Quotations
open Expecto
open FParsec
open Equal.LogicalChain

let wrap str = Var(str, typeof<bool>) |> Expr.Var |> Expr.Cast<bool>

let rec A = wrap ^ nameof A
let rec B = wrap ^ nameof B
let rec C = wrap ^ nameof C
let rec D = wrap ^ nameof D
let rec E = wrap ^ nameof E

let parser = 
    mkLogicalChain ^ choice [
        A |> stringReturn ^ nameof A
        B |> stringReturn ^ nameof B
        C |> stringReturn ^ nameof C
        D |> stringReturn ^ nameof D
    ] .>> spaces

let inline should compare expected input =
    let name = sprintf "parses '%s'" input
    test name { parsed parser compare expected input } |> testLabel "with valid input"

let equal = Expect.equal

let inline shouldFailWith expected input =
    let name = sprintf "fails parsing '%s'" input
    test name { failed parser expected input } |> testLabel "with invalid input"

[<Tests>]
let tests =
    testList "logical chain parser" [
        "A"                     |> should equal A
        "A AND B OR C AND D"    |> should equal <@  %A && %B  ||  %C && %D  @>
        "A OR B AND C OR D"     |> should equal <@  %A || %B  &&  %C || %D  @> 
        "A OR B AND (C OR D)"   |> should equal <@  %A || %B  && (%C || %D) @>
        "(A OR B) AND C OR D"   |> should equal <@ (%A || %B) &&  %C || %D  @>
        "(A OR B) AND (C OR D)" |> should equal <@ (%A || %B) && (%C || %D) @>
        
        "NOT(A)"                        |> should equal <@ not %A @>
        "NOT(A) AND B"                  |> should equal <@ not %A && %B @>
        "NOT(A) AND NOT(B)"             |> should equal <@ not %A && not %B @>
        "not (A) OR B"                  |> should equal <@ not %A || %B @>
        "not ( A )  OR   not ( B )"     |> should equal <@ not %A || not %B @>
        "NOT(A OR B) AND (C OR D)"      |> should equal <@ not(%A || %B) && (%C || %D) @>
        "NOT(NOT(A OR B) AND (C OR D))" |> should equal <@ not(not(%A || %B) && (%C || %D)) @>

        ""          |> shouldFailWith { position = 1L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        " "         |> shouldFailWith { position = 1L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        "()"        |> shouldFailWith { position = 2L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        "z"         |> shouldFailWith { position = 1L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        "A && B"    |> shouldFailWith { position = 3L; errors = ["AND"; "OR"; "end of input"] }
        "NOT A"     |> shouldFailWith { position = 5L; errors = ["("] }
        "A OR"      |> shouldFailWith { position = 5L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        "A AND"     |> shouldFailWith { position = 6L; errors = ["("; "A"; "B"; "C"; "D"; "NOT"] }
        "(A AND B"  |> shouldFailWith { position = 9L; errors = [")"; "AND"; "OR"] }
        "A andB"    |> shouldFailWith { position = 3L; errors = ["AND"; "OR"; "end of input"] }
        "A orB"     |> shouldFailWith { position = 3L; errors = ["AND"; "OR"; "end of input"] }
    ]