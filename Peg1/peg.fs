
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

type ParseResult =
| TerminalSymbol of string
| Production of ParseResult list
| EmptyMatch
| Unmatched

let parseExpression (grammar:Map<string,Expression>) start (input:string) =
    let rec parse offset = function
    | Terminal x -> 
        let e = offset + x.Length - 1
        if e < input.Length && input.[offset..e] = x then (TerminalSymbol x, e + 1) else (Unmatched, offset)
    | NonTerminal x -> parse offset grammar.[x]
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

    parse 0 grammar.[start]

let g = Map.ofList [("bool", Choice [Terminal "true"; Terminal "false"]);
                    ("paren", Sequence [Terminal "("; NonTerminal "expr"; Terminal ")"]);
                    ("not", Sequence [Terminal "!"; NonTerminal "expr"]);
                    ("atom", Choice [NonTerminal "bool"; NonTerminal "paren"; NonTerminal "not"]);
                    ("and", Sequence [NonTerminal "atom"; Optional (Sequence [Terminal "&"; NonTerminal "and"])]);
                    ("or", Sequence [NonTerminal "and"; Optional (Sequence [Terminal "|"; NonTerminal "or"])]);
                    ("expr", NonTerminal "or");
                    ("start", Sequence [NonTerminal "expr"; Epsilon])]