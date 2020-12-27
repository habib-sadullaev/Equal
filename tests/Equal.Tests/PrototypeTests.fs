module PrototypeTests

open Expecto
open FParsec
open Equal.Prototype

#nowarn "86"

let ok parser input =
    match run (parser .>> eof) input with
    | Success(v, _, _) -> v
    | Failure(e, _, _) -> failtest e

let error parser input =
    match run (parser .>> eof) input with
    | Success(v, _, _) -> failtestf "%A" v
    | Failure(_, e, _) -> 
        let rec gatherErrs errs = 
            [ for err in errs |> ErrorMessageList.ToSortedArray do
              match err with
              | Expected msg 
              | ExpectedString msg 
              | ExpectedStringCI msg -> msg
              | NestedError (_, _, errs) -> yield! gatherErrs errs
              | _ -> ()
            ] |> List.distinct |> List.sort

        { position = e.Position.Column; errors = gatherErrs e.Messages }

type TestInfo<'a when 'a : equality> = 
    { name: string
      input: string
      parser: Parser<'a, unit> }
    
    member inline this.Expect(expected) =
        let { name = name; input = input; parser = parser } = this
        let testName = 
            sprintf "'%s' parsed with '%s parser'" input name
            |> String.map ^ function '.' -> '_' | sym -> sym
        test testName {
            let { position = pos; errors = errs } = error parser input
            Expect.equal pos expected.position
                ^ sprintf "incorrect error position\nexpected: '%d'\n  actual: '%d'" expected.position pos
            Expect.equal errs expected.errors
                ^ sprintf "Incorrect error message\nexpected:\n%A\nactual:\n%A" expected.errors errs
        } |> testLabel "invalid input"

    member inline this.Expect(expected) =
        let { name = name; input = input; parser = parser } = this
        let testName =
            sprintf "'%s' parsed with '%s parser'" input name
            |> String.map ^ function '.' -> '_' | sym -> sym
        test testName {
            let actual = ok parser input
            Expect.equal actual expected ^ sprintf "expected:\n%A\n  actual:\n%A\n" expected actual
        } |> testLabel "valid input"

let inline expects expected testInfo =
    let inline resolve (stub: ^t) (res: ^a) = (^t : (member Expect : ^a -> Test) (stub, res))
    resolve testInfo expected

let operandParser    = operand,    nameof(operand)
let listParser       = list,       nameof(list)
let comparisonParser = comparison, nameof(comparison)
let predicateParser  = predicate,  nameof(predicate)

let whenParsing input (parser, name) = { name = name; input = input; parser = parser }
let bool input = ok comparison input

let private (&&) a b = And(a, b)
let private (||) a b = Or(a, b)

[<Tests>]
let tests =
    testList "prototype parser" [
        operandParser |> whenParsing "A"               |> expects ^ Var(Prop(Param, "A"))
        operandParser |> whenParsing "A.B"             |> expects ^ Var(Prop(Prop(Param,  "A"), "B"))
        operandParser |> whenParsing "A.B.C "          |> expects ^ Var(Prop(Prop(Prop(Param, "A"), "B"), "C"))
        operandParser |> whenParsing "'5 o''clock'"    |> expects ^ Const "5 o'clock"
        
        listParser |> whenParsing "( 'a' , 'b', 'c' )" |> expects [for i = 0 to 2 do string ('a' + char i)]
        
        comparisonParser |> whenParsing "A.B.C < '5'"  |> expects ^ Comparison(LessThan, ok var "A.B.C", ok operand "'5'")
        comparisonParser |> whenParsing "A.B.C <= '5'" |> expects ^ Comparison(LessThanOrEqual, ok var "A.B.C", ok operand "'5'")
        comparisonParser |> whenParsing "A.B.C = '5'"  |> expects ^ Comparison(Equal, ok var "A.B.C", ok operand "'5'")
        comparisonParser |> whenParsing "A"            |> expects ^ Comparison(Equal, ok var "A", Const "true")
              
        comparisonParser |> whenParsing "B contains 'a'"         |> expects ^ StringComparison(Contains, ok var "B", "a")
        comparisonParser |> whenParsing "C IN ('aaa','bbb')"     |> expects ^ In(ok var "C", ok list "('aaa', 'bbb')")
        comparisonParser |> whenParsing "C NOT IN ('aaa','bbb')" |> expects ^ Not(In(ok var "C", ok list "('aaa', 'bbb')"))
        comparisonParser |> whenParsing "A ANY (    )"           |> expects ^ Any(ok var "A", None)
        comparisonParser |> whenParsing "A ANY (B='10' OR C)"    |> expects ^ Any(ok var "A", Some(ok predicate "B = '10' or C"))
        
        predicateParser |> whenParsing "A AND B OR C AND D"     |> expects (bool "A" && bool "B" || bool "C" && bool "D")
        predicateParser |> whenParsing "A OR B AND C OR D"      |> expects (bool "A" || bool "B" && bool "C" || bool "D")
        predicateParser |> whenParsing "(A OR B) AND C OR D"    |> expects ((bool "A" || bool "B") && bool "C" || bool "D")
        predicateParser |> whenParsing "A.B = '19' AND B = '6'" |> expects (bool "A.B = '19'" && bool "B = '6'")
        predicateParser |> whenParsing "NOT(A OR B) AND C OR D" |> expects (Not(bool "A" || bool "B") && bool "C" || bool "D")
            
        predicateParser |> whenParsing "B = '6' or C CONTAINS '6'" |> expects (bool "B = '6'" || bool "C contains '6'")
        predicateParser |> whenParsing "C STARTS WITH '6' or D"    |> expects (bool "C starts with '6'" || bool "D")
        predicateParser |> whenParsing "(A OR B) AND C OR D ANY()" |> expects ((bool "A" || bool "B") && bool "C" || Any(ok var "D", None))
        predicateParser |> whenParsing "X AND (Y AND Z) AND A"     |> expects (bool "X" && (bool "Y" && bool "Z") && bool "A")
        predicateParser |> whenParsing "X ANY(A AND B or C)"       |> expects (Any(ok var "X", Some(bool "A" && bool "B" || bool "C")))
        predicateParser |> whenParsing "X OR (Y OR Z) OR A"        |> expects (bool "X" || (bool "Y" || bool "Z") || bool "A")
        predicateParser |> whenParsing "X ANY() or Y IN('1', '2')" |> expects (Any(ok var "X", None) || In(ok var "Y", ["1"; "2"]))
        predicateParser |> whenParsing "(A ANY(B ANY(C ANY())))"   |> expects (Any(ok var "A", Some(Any(ok var "B", Some(Any(ok var "C", None))))))
        
        operandParser |> whenParsing ""         |> expects { position = 1L; errors = ["'"; "property"] }
        operandParser |> whenParsing "(A).B"    |> expects { position = 1L; errors = ["'"; "property"] }
        operandParser |> whenParsing "A.B. "    |> expects { position = 5L; errors = ["property"] }
        operandParser |> whenParsing "A.B."     |> expects { position = 5L; errors = ["property"] }
        operandParser |> whenParsing "A.B || C" |> expects { position = 5L; errors = ["."; "end of input"] }

        comparisonParser |> whenParsing "A starts with"  |> expects { position = 14L; errors = ["'"] }
        comparisonParser |> whenParsing "'a' <> "        |> expects { position =  8L; errors = ["property"] }
        comparisonParser |> whenParsing "A.B.C <= "      |> expects { position = 10L; errors = ["'"; "property"] }
        comparisonParser |> whenParsing ""               |> expects { position =  1L; errors = ["'"; "property"] }
        comparisonParser |> whenParsing "'a'"            |> expects { position =  4L; errors = ["!=";  "<"; "<="; "<>"; "="; ">"; ">="] }
        comparisonParser |> whenParsing "A || 'B'"       |> expects { position =  3L; errors = 
            [ "!="; "."; "<"; "<="; "<>"; "="; ">"; ">="; "ALL"; "ANY"; "CONTAINS"; 
              "ENDS WITH"; "IN"; "NOT IN"; "STARTS WITH"; "end of input" ] }

        predicateParser |> whenParsing "NOT A OR B"     |> expects { position =  4L; errors = ["("] }
        predicateParser |> whenParsing "A.B.C IN 'aaa'" |> expects { position = 10L; errors = ["("] }
        predicateParser |> whenParsing "A contains aaa" |> expects { position = 12L; errors = ["'"] }
        predicateParser |> whenParsing "'a'"            |> expects { position =  4L; errors = ["!=";  "<"; "<="; "<>"; "="; ">"; ">="] }
        predicateParser |> whenParsing ""               |> expects { position =  1L; errors = ["'"; "("; "NOT"; "property"] }
        predicateParser |> whenParsing  "A || B"        |> expects { position =  3L; errors = 
            [ "!="; "."; "<"; "<="; "<>"; "="; ">"; ">="; "ALL"; "AND"; "ANY"; "CONTAINS";
              "ENDS WITH"; "IN"; "NOT IN"; "OR"; "STARTS WITH"; "end of input" ] }
    ]