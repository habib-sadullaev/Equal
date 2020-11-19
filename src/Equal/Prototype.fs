module Equal.Prototype
open FParsec

type Var = Param | Prop  of Var * string

type Operand = Var of Var | Const of string

type Comparison =
    | NotEqual | LessThan    | LessThanOrEqual
    | Equal    | GreaterThan | GreaterThanOrEqual

type StringComparison = StartsWith | Contains | EndsWith

type Operation =
    | Comparison of Comparison * Var * Operand

    | StringComparison of StringComparison * Var * string

    | In  of Var * string list

    | Any of Var * Operation option
    | All of Var * Operation option

    | Not of Operation

    | And of Operation * Operation
    | Or  of Operation * Operation

let literal: Parser<_, unit> =
    let apostrophe  = stringReturn "''" '\''
    let otherSymbol = satisfy ((<>) '\'')

    manyChars (apostrophe <|> otherSymbol)
    |> between (skipChar '\'') (skipChar '\'')
    .>> spaces

let ident: Parser<_, unit> =
    identifier(IdentifierOptions()) .>> spaces <?> "property"

let parenthesize parser =
    parser
    |> between spaces spaces
    |> between (skipChar '(') (skipChar ')')
    |> attempt

let mkOperator ctor left right = ctor(left, right)

let var =
    let propAccessor = skipChar '.'

    sepBy1 ident propAccessor .>> spaces
    |>> fun props -> props |> List.fold (mkOperator Prop) Param

let list =
    sepBy literal (skipChar ',' .>> spaces)
    |> parenthesize

let operand = choice [ literal |>> Const; var |>> Var ]

let predicate, predicateRef = createParserForwardedToRef()

let comparison =
    let mkOp operator operand =
        pipe2 (operator |>> mkOperator .>> spaces)
              operand
              (fun op right left -> op left right)
        .>> spaces

    let compareWith str op operand =
        mkOp (stringReturn str op) operand

    let compareCIWith str op operand =
        mkOp (stringCIReturn str op) operand

    let comparison str op =
        compareWith str (fun (x, y) -> Comparison(op, x, y)) operand

    let reversedComparison str op =
        compareWith str (fun (l, r) -> Comparison(op, r, Const l)) var

    let stringComparison str op =
        compareCIWith str (fun (x, y) -> StringComparison(op, x, y)) literal

    let inComparison str op = compareCIWith str op list

    let anyComparison str op =
        compareCIWith str op (parenthesize (opt predicate))

    let boolComparison =
        preturn (fun left -> Comparison(Equal, left, Const "true"))

    let direct =
        [ comparison ">=" GreaterThanOrEqual
          comparison "<=" LessThanOrEqual
          comparison "="  Equal
          comparison "!=" NotEqual
          comparison "<>" NotEqual
          comparison "<"  LessThan
          comparison ">"  GreaterThan

          stringComparison "STARTS WITH" StartsWith
          stringComparison "ENDS WITH"   EndsWith
          stringComparison "CONTAINS"    Contains

          inComparison "IN"     In
          inComparison "NOT IN" (Not << In)

          anyComparison "ANY" Any
          anyComparison "ALL" All

          boolComparison ]
        |> choice
        |> fun f -> pipe2 (var .>> spaces) f (|>)

    let reverse =
        [ reversedComparison ">=" GreaterThanOrEqual
          reversedComparison "<=" LessThanOrEqual
          reversedComparison "="  Equal
          reversedComparison "!=" NotEqual
          reversedComparison "<>" NotEqual
          reversedComparison "<"  LessThan
          reversedComparison ">"  GreaterThan ]
        |> choice
        |> fun f -> pipe2 (literal .>> spaces) f (|>)

    direct <|> reverse


do predicateRef :=
    let op name operator operand =
        stringCIReturn name (mkOperator operator) .>> spaces
        |> chainl1 (operand .>> spaces)

    let operand, operandRef = createParserForwardedToRef()

    let operation = op "OR" Or (op "AND" And operand)
    let nestedOperation = parenthesize operation
    let negation = skipStringCI "NOT" >>. nestedOperation |>> Not

    operandRef := choice [ negation; comparison; nestedOperation ]

    operation