//replaces the default parse CE
[<AutoOpen>]
module internal FParsec.Primitives

[<Sealed>]
type ParserCombinator() =
    member _.Return(x) = preturn x
    member _.Bind(p, f) = p >>= f
    member _.ReturnFrom(p: Parser<'a,'u>) = p
    member _.MergeSources(p1, p2) = p1 .>>. p2
    member _.BindReturn(p: Parser<'a, 'u>, f) = p |>> f

let parse = ParserCombinator()