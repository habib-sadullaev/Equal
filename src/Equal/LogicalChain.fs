module internal Equal.LogicalChain

open System
open FSharp.Quotations
open FParsec
open TypeShape.Core.StagingExtensions

// operation = operand { operator operand }
let private mkOperation operator operand =
    operator .>> spaces |> chainl1 (operand .>> spaces)

let private mkOperator name operator : Parser<Expr<'a> -> Expr<'a> -> Expr<'a>, State> =
    let operator = stringCIReturn name operator
    fun stream ->
        let init = stream.State
        let reply = operator stream
        if reply.Status = Ok && stream.Peek() |> Char.IsLetterOrDigit then
            stream.BacktrackTo init
            Reply(Error, expectedStringCI name)
        else
            reply

let private AND = mkOperator "AND" ^ fun lhs rhs -> <@ %lhs && %rhs @>
let private OR  = mkOperator "OR"  ^ fun lhs rhs -> <@ %lhs || %rhs @>
let private NOT parser = skipStringCI "NOT" >>. spaces >>. parser |>> fun x -> <@ not %x @>

// operation         = operand { operator operand }
// operand           = ( negation | nested-operation | bool-expr )
// negation          = 'NOT' nested-operation
// nested-operation  = '(' operation ')'
// operator          = ( 'OR' | 'AND' )
let mkLogicalChain parser =
    let operand, operandRef = createParserForwardedToRef()

    let operation = mkOperation OR (mkOperation AND operand)
    let nestedOperation = parenthesize operation
    let negation = NOT nestedOperation
    
    // operand = ( negation | nested-operation | bool-expr )
    operandRef := choice [ negation; nestedOperation; parser ]
    
    operation |>> Expr.cleanup
