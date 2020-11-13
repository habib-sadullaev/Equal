module PrototypeTests

open Expecto
open FParsec
open EQL.Prototype

let parse parser input =
    match run (parser .>> eof) input with
    | Success (v, _, _) -> v
    | Failure (e, _, _) -> failwith e

let pass name parser =
    let mutable x = 0
    let callback (input, expected) () =
        let actual = parse parser input
        Expect.equal actual expected "They should be equal"
    
    fun data -> 
        testList name [ yield! testFixture callback [ for item in data -> x <- x + 1; sprintf "case %i" x, item ] ]

type ExpectedFailInfo = { position: int64; message: string }

let fail name parser =
    let mutable x = 0
    let parse parser input =
        match run (parser .>> eof) input with
        | Success (_, _, _) -> failwith "The test should fail"
        | Failure (s, e, _) -> { position = e.Position.Column; message = s.Trim() }
    let callback (input, { position = pos; message = msg}) () =
        let actual = parse parser input
        Expect.stringEnds actual.message  msg "Parsing should fail with the proper message" 
        Expect.equal      actual.position pos "Parsing should fail with the proper position" 
    
    fun data ->
        testList (sprintf "incorrect input.%s" name)
            [ yield! testFixture callback [ for item in data -> x <- x + 1; sprintf "case %i" x, item ] ]

let bool input = parse comparison input
let (.&&.) a b = And(a, b)
let (.||.) a b = Or(a, b)

[<RequireQualifiedAccess>]
module ExpectedOperators = 
    let [<Literal>] comparisonLevel =
        """Expecting: end of input, '!=', '.', '<', '<=', '<>', '=', '>', '>=', 'ALL'
(case-insensitive), 'ANY' (case-insensitive), 'CONTAINS' (case-insensitive),
'ENDS WITH' (case-insensitive), 'IN' (case-insensitive), 'NOT IN'
(case-insensitive) or 'STARTS WITH' (case-insensitive)"""

    let [<Literal>] predicateLevel =
        """Expecting: end of input, '!=', '.', '<', '<=', '<>', '=', '>', '>=', 'ALL'
(case-insensitive), 'AND' (case-insensitive), 'ANY' (case-insensitive),
'CONTAINS' (case-insensitive), 'ENDS WITH' (case-insensitive), 'IN'
(case-insensitive), 'NOT IN' (case-insensitive), 'OR' (case-insensitive) or
'STARTS WITH' (case-insensitive)"""
        

[<Tests>]
let operandtests =
    testList "prototype tests" [
        pass "operand" operand 
            [ 
                "A",            Var(Prop(Param, "A"))
                "A.B",          Var(Prop(Prop(Param,  "A"), "B"))
                "A.B.C ",       Var(Prop(Prop(Prop(Param, "A"), "B"), "C"))
                "'5 o''clock'", Const "5 o'clock"
            ] 

        pass "list" list
            [
                "('a' , 'b', 'c' ,'d','e' )", [ for i = 0 to 4 do string ('a' + char i) ] 
            ]

        pass "comparison" comparison
            [
                "A.B.C < '5'",  Comparison(LessThan, parse var "A.B.C", parse operand "'5'")
                "A.B.C <= '5'", Comparison(LessThanOrEqual, parse var "A.B.C", parse operand "'5'")
                "A.B.C = '5'",  Comparison(Equal, parse var "A.B.C", parse operand "'5'")
                "A",            Comparison(Equal, parse var "A", Const "true")
              
                "A.B.C Contains 'aaa'", StringComparison(Contains, parse var "A.B.C", "aaa")
              
                "A.B.C IN ('aaa','bbb')",     In(parse var "A.B.C", parse list "('aaa', 'bbb')")
                "A.B.C NOT IN ('aaa','bbb')", Not(In(parse var "A.B.C", parse list "('aaa', 'bbb')"))
              
                "A ANY (    )",
                    Any(parse var "A", None)

                "A   ANY  (  B = '7/29/2020 11:44:00'   OR   C  )", 
                    Any(parse var "A", Some(parse predicate "B = '7/29/2020 11:44:00' or C"))
            ]

        pass "predicate" predicate
            [
                "A AND B OR C AND D",     bool "A" .&&. bool "B" .||. (bool "C" .&&. bool "D")
                "A OR B AND C OR D",      bool "A" .||. (bool "B" .&&. bool "C") .||. bool "D" 
                "(A OR B) AND C OR D",    bool "A" .||. bool "B" .&&. bool "C" .||. bool "D"
                "A.B = '19' AND B = '6'", bool "A.B = '19'" .&&. bool "B = '6'"
                "NOT(A OR B) AND C OR D", Not(bool "A" .||. bool "B") .&&. bool "C" .||. bool "D"
            
                "(A.B = '19' AND B = '6') or C Starts with '6'", 
                    bool "A.B = '19'" .&&. bool "B = '6'" .||. bool "C Starts with '6'"
            
                "(A.B = '19' AND B = '6') or C Starts with '6' or C", 
                    bool "A.B = '19'" .&&. bool "B = '6'" .||. bool "C Starts with '6'" .||. bool "C"
            
                "A.B = '19' AND B = '6' or C Starts with '6' or C",
                    bool "A.B = '19'" .&&. bool "B = '6'" .||. bool "C Starts with '6'" .||. bool "C"
            
                "(A OR B) AND C OR D AND E ANY()",
                    bool "A" .||. bool "B" .&&. bool "C" .||. (bool "D" .&&. Any(parse var "E", None))
                
                "(X AND (Y AND (Z AND ((A AND B) AND C))))",
                    bool "X" .&&. (bool "Y" .&&. (bool "Z" .&&. (bool "A" .&&. bool "B" .&&. bool "C")))
            
                "(X OR (Y OR (Z OR ((A OR B) OR C))))",
                    bool "X" .||. (bool "Y" .||. (bool "Z" .||. (bool "A" .||. bool "B" .||. bool "C")))

                "(A OR B) AND C OR D AND E ANY(F OR G AND H)",
                    bool "A" .||. bool "B" .&&. bool "C" .||. 
                        (bool "D" .&&. 
                            Any(parse var "E", 
                                 Some(bool "F" .||. (bool "G" .&&. bool "H"))))
            
                "A ANY ()", Any(parse var "A", None)

                "A ANY (B = '7/29/2020 11:44:00' OR C)", 
                    Any(parse var "A",
                        Some(parse predicate "B = '7/29/2020 11:44:00' OR C"))

                "X ANY(A AND B or C and D) or Y IN('1', '2', '3')",
                    Or
                        (Any(parse var "X", Some(bool "A" .&&. bool "B" .||. (bool "C" .&&. bool "D"))),
                         In(parse var "Y", List.map string [1..3]))

                "(NOT(A ANY(B ANY(C ANY()))))",
                    Not(Any(parse var "A", 
                            Some(Any(parse var "B", 
                                     Some(Any(parse var "C", None))))))
            ]

        fail "operand" operand
            [
                "",         { position = 1L; message = "Expecting: property or '\\''" }
                "(A).B",    { position = 1L; message = "Expecting: property or '\\''" }
                "A.B. ",    { position = 5L; message = "Expecting: property" }
                "A.B.",     { position = 5L; message = "Expecting: property" }

                "A.B || C", { position = 5L; message = "Expecting: end of input or '.'"}
            ]

        fail "comparison" comparison
            [
                "A.B.C Starts wItH", { position = 18L; message = "Expecting: '\\''" }
                "'a' <> ",           { position = 8L;  message = "Expecting: property" }
                "A.B.C <= ",         { position = 10L; message = "Expecting: property or '\\''" }
                
                "",                  { position = 1L;  message = "Expecting: property or '\\''" }
                "'a'",               { position = 4L;  message = "Expecting: '!=', '<', '<=', '<>', '=', '>' or '>='" }
                
                "A || 'B'",          { position = 3L; message  = ExpectedOperators.comparisonLevel }
            ]

        fail "predicate" predicate
            [
                "NOT A OR B",         { position = 4L;  message = "Expecting: '('" }
                "A.B.C IN 'aaa'",     { position = 10L; message = "Expecting: '('" }
                "A.B.C contains aaa", { position = 16L; message = "Expecting: '\\''" }
                
                "'a'",                { position = 4L;  message =  "Expecting: '!=', '<', '<=', '<>', '=', '>' or '>='" }
                "",                   { position = 1L; message =  "Expecting: property, '\\'', '(' or 'NOT' (case-insensitive)" }

                "A || B",             { position = 3L; message  = ExpectedOperators.predicateLevel }
            ]
  ]