module Peg

type Expression =
| Terminal of string
| NonTerminal of string
| Epsilon
| Sequence of Expression list
| Choice of Expression list
| ZeroOrMore of Expression
| OneOrMore of Expression
| Optional of Expression
| And of Expression
| Not of Expression

let rec pegToString expr =
    let pegToStringParen = function
    | Terminal _ | NonTerminal _ | Epsilon as x -> pegToString x
    | x -> "(" + pegToString x + ")"

    match expr with
    | Terminal x -> "\"" + x + "\""
    | NonTerminal x -> x
    | Epsilon -> "<epsilon>"
    | Sequence x -> List.map pegToStringParen >> List.reduce (fun a b -> a + " " + b) <| x
    | Choice x -> List.map pegToStringParen >> List.reduce (fun a b -> a + " / " + b) <| x
    | ZeroOrMore x -> pegToStringParen x + "*"
    | OneOrMore x -> pegToStringParen x + "+"
    | Optional x -> pegToStringParen x + "?"
    | And x -> "&" + pegToStringParen x
    | Not x -> "!" + pegToStringParen x


let printPeg expr =
    printfn "%s" <| pegToString expr

type ParseResult<'a> =
| Parsed of 'a
| TerminalSymbol of string
| Production of ParseResult<'a> list
| EmptyMatch
| Unmatched

type GrammarRule<'a>(prod:Expression, func:(string -> ParseResult<'a> -> ParseResult<'a>)) =
    member this.Prod = prod
    member this.Func = func

let parseExpression (grammar:Map<string,GrammarRule<'a>>) start (input:string) =
    let rec parse offset = function
    | Terminal x -> 
        let e = offset + x.Length - 1
        if e < input.Length && input.[offset..e] = x then (TerminalSymbol x, e + 1) else (Unmatched, offset)
    | NonTerminal x -> 
        let rule = grammar.[x]
        match parse offset rule.Prod with 
        | (Unmatched, _) as y -> y
        | (parsed, endOffset) -> (rule.Func input.[offset..endOffset - 1] parsed, endOffset)
    | Epsilon -> if offset = input.Length then (EmptyMatch, offset) else (Unmatched, offset)
    | Sequence x ->
        let rec parseList lst off =
            seq {
                if List.isEmpty lst then ()
                else
                    match parse off <| List.head lst with
                    | (Unmatched, _) as y -> yield y
                    | (y, z) -> yield (y, z)
                                yield! parseList (List.tail lst) z
            }
        
        let s = List.ofSeq <| parseList x offset 
        if List.exists (function | (Unmatched, _) -> true | _ -> false) s then (Unmatched, offset)
        else (Production <| List.map fst s, snd <| List.maxBy snd s)
    | Choice x ->
        let rec parseList lst =
            if List.isEmpty lst then (Unmatched, offset)
            else
                match parse offset <| List.head lst with
                | (Unmatched, _) -> parseList <| List.tail lst
                | y -> y
        
        parseList x
    | ZeroOrMore x ->
        let rec parseList off =
            seq {
                match parse off x with
                | (Unmatched, _) -> ()
                | (y, z) -> yield (y, z)
                            yield! parseList z
            }
        let s = List.ofSeq <| parseList offset
        if List.isEmpty s then (EmptyMatch, offset) 
        else (Production <| List.map fst s, snd <| List.maxBy snd s)
    | OneOrMore x ->
        let rec parseList off =
            seq {
                match parse off x with
                | (Unmatched, _) -> ()
                | (y, z) -> yield (y, z)
                            yield! parseList z
            }
        let s = List.ofSeq <| parseList offset
        if List.isEmpty s then (Unmatched, offset)
        else (Production <| List.map fst s, snd <| List.maxBy snd s)
    | Optional x ->
        match parse offset x with
        | (Unmatched, _) -> (EmptyMatch, offset)
        | y -> y
    | And x ->
        match parse offset x with
        | (Unmatched, _) -> (Unmatched, offset)
        | _ -> (EmptyMatch, offset)
    | Not x ->
        match parse offset x with
        | (Unmatched, _) -> (EmptyMatch, offset)
        | _ -> (Unmatched, offset)

    parse 0 <| NonTerminal start
