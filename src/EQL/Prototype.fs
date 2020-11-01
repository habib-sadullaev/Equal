module EQL.Prototype
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
    let negate = skipStringCI "NOT" >>. parenthesize operation |>> Not

    operandRef := choice [ negate; comparison; parenthesize operation ]

    operation

let test parser input =
    match run (parser .>> eof) input with
    | Success (v, _, _) -> printfn "%A" v
    | Failure (e, _, _) -> printfn "%s" e

test operand "A.B.C"
test operand "A.B "
test operand "'a'"

test literal "'5 o''clock'"

test list "('a' , 'b', 'c' ,'d','e' )"

test comparison "A.B.C < '5'"
test comparison "A.B.C <= '5'"
test comparison "A.B.C = '5'"
test comparison "A.B.C Contains 'aaa'"
test comparison "A.B.C IN ('aaa','bbb')"
test comparison "A.B.C NOT IN ('aaa','bbb')"
test comparison "A"
test comparison "A ANY (    )"
test comparison "A   ANY  (  B = '7/29/2020 11:44:00'   OR   C  )"

test predicate "(A.B = '19' AND B = '6') or C Starts with '6' or C"
test predicate "A.B = '19' AND B = '6' or C Starts with '6' or C"
test predicate "A AND B OR C AND D"
test predicate "A OR B AND C OR D"
test predicate "(A OR B) AND C OR D"
test predicate "NOT(A OR B) AND C OR D"
test predicate "A ANY ()"
test predicate "A ANY (B = '7/29/2020 11:44:00' OR C)"
test predicate "(A OR B) AND C OR D AND E ANY()"
test predicate "(A OR B) AND C OR D AND E ANY(F OR G AND H)"
test predicate "X ANY(A AND B or C and D) or Y IN('1', '2', '3')"
test predicate "(X AND (Y AND (Z AND ((A AND B) AND C))))"
test predicate "(X OR (Y OR (Z OR ((A OR B) OR C))))"
test predicate "(NOT(A ANY(B ANY(C ANY()))))"

//------------------------------------------------------------------
//------------------------------errors------------------------------
//------------------------------------------------------------------

test operand "(A).B"
test comparison "'a' <> "
test operand "A.B || C"
test operand ""
test operand "A.B. "
test operand "A.B."

test comparison "A || 'B'"
test comparison ""
test comparison "'a'"
test comparison "A.B.C <= "
test comparison "A.B.C Starts wItH"

test predicate "A || B"
test predicate ""
test predicate "NOT A OR B AND C OR D"
test predicate "A.B.C IN 'aaa'"
test predicate "A.B.C contains aaa"
test predicate "'a'"
