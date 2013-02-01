module Peg

open System
open System.Globalization
open System.Text

type Expression =
| Terminal of string
| TerminalOneOf of string
| TerminalUnicode of UnicodeCategory
| TerminalWildcard
| NonTerminal of string
| Epsilon
| Sequence of Expression list
| Choice of Expression list
| ZeroOrMore of Expression
| OneOrMore of Expression
| Optional of Expression
| And of Expression
| Not of Expression
| Rule of string * Expression * string

let escapeChar = function
| '\a' -> "\\a" | '\b' -> "\\b" | '\f' -> "\\f" | '\n' -> "\\n" | '\r' -> "\\r" | '\t' -> "\\t" | '\v' -> "\\v" | '\u0000' -> "\\u0000" | '\\' -> "\\\\" | '\'' -> "\\'"
| '"' -> "\\\"" 
| x -> let u = int(x) 
       if (u < 0x7F) then string(x)
       else sprintf "\\u%04X" u

let escape (x:string) = 
    String.init x.Length (fun i -> escapeChar x.[i])

let rec pegToString expr =
    let pegToStringParen = function
    | Sequence _ | Choice _ as x -> "(" + pegToString x + ")"
    | x -> pegToString x

    match expr with
    | Terminal x -> if x.Length = 1 then "'" + escape x + "'" else "\"" + escape x + "\""
    | TerminalOneOf x -> "[" + escape x + "]"
    | TerminalUnicode x -> "{" + string(x) + "}"
    | TerminalWildcard -> "<anychar>"
    | NonTerminal x -> x
    | Epsilon -> "<epsilon>"
    | Sequence x -> List.map pegToStringParen >> List.reduce (fun a b -> a + " " + b) <| x
    | Choice x -> List.map pegToStringParen >> List.reduce (fun a b -> a + " / " + b) <| x
    | ZeroOrMore x -> pegToStringParen x + "*"
    | OneOrMore x -> pegToStringParen x + "+"
    | Optional x -> pegToStringParen x + "?"
    | And x -> "&" + pegToStringParen x
    | Not x -> "!" + pegToStringParen x
    | Rule (x, y, "") -> x + " <- " + pegToString y
    | Rule (x, y, z) -> x + " <- " + pegToString y + "\t: " + z


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

let currentCharacter (s:string) i =
    if i >= s.Length then None
    else if Char.IsSurrogatePair(s, i) then Some s.[i..i+1] else Some s.[i..i]

let parseExpression (grammar:Map<string,GrammarRule<'a>>) start (input:string) =
    let rec parse offset = function
    | Terminal x -> 
        let e = offset + x.Length - 1
        if e < input.Length && input.[offset..e] = x then (TerminalSymbol x, e + 1) else (Unmatched, offset)
    | TerminalOneOf x -> match currentCharacter input offset with
                         | Some c -> if x.Contains(c) then (TerminalSymbol c, offset + c.Length) else (Unmatched, offset)
                         | None -> (Unmatched, offset)
    | TerminalWildcard -> match currentCharacter input offset with 
                          | Some c -> (TerminalSymbol c, offset + c.Length)
                          | None -> (Unmatched, offset)
    | TerminalUnicode x ->
        if offset < input.Length && System.Char.GetUnicodeCategory(input, offset) = x then (TerminalSymbol input.[offset..offset], offset + 1) else (Unmatched, offset)
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
    | Rule _ -> failwith "Internal error"

    parse 0 <| NonTerminal start

let checkForLeftRecursion (rules:Map<string,Expression>) =
    let rec leftRules = function
    | Terminal _ | TerminalOneOf _ | TerminalUnicode _ | TerminalWildcard | Epsilon -> []
    | NonTerminal x -> [x]
    | ZeroOrMore x | OneOrMore x | Optional x | Not x | And x -> leftRules x
    | Choice x ->
        List.concat <| List.map leftRules x
    | Sequence x ->
        let rec processList = function
        | [] -> []
        | (head :: tail) ->
            match head with
            | Optional _ | Not _ | And _ | ZeroOrMore _ | OneOrMore _ -> List.append (leftRules head) (processList tail)
            | x -> leftRules head
        processList x
    | Rule _ -> failwith "Internal error"

    let rec checkRule name prior = 
        if List.exists ((=) name) prior then
            failwithf "Left recursion calling %s" <| List.reduce (fun a b -> (b + " -> " + a)) (name :: prior)
        match Map.tryFind name rules with
        | None -> failwithf "Unknown rule %s" name
        | Some x ->
            let leftchoices = leftRules x
            let sp = (name :: prior)
            for rule in leftchoices do checkRule rule sp done

    try
        Map.iter (fun k _ -> checkRule k []) rules
    with
    | ex -> eprintfn "Grammar error: %s" ex.Message
            failwith "Invalid Grammar"

let capitalIdentifier (x:string) =
    let y = String.init x.Length (fun i -> if System.Char.IsLetter(x, i) then x.[i..i] else "")
    string(System.Char.ToUpperInvariant(y.[0])) + y.[1..]

let ibprintf (b:StringBuilder) i fmt = Printf.kprintf (fun s -> b.Append(' ', i).Append(s) |> ignore) fmt

let ibprintfn (b:StringBuilder) i fmt = Printf.kprintf (fun s -> b.Append(' ',i).AppendLine(s) |> ignore) fmt
    
let rec codeGen (b:StringBuilder) i = function
| Terminal x -> ibprintf b i """matchTerminal "%s" input offset""" <| escape x
| TerminalOneOf x -> ibprintf b i """matchTerminalOneOf "%s" input offset""" <| escape x
| TerminalWildcard -> ibprintf b i """matchTerminalWildcard input offset"""
| TerminalUnicode x -> ibprintf b i """matchTerminalUnicode System.Globalization.UnicodeCategory.%s input offset""" <| string(x)
| Epsilon -> ibprintf b i """if offset = input.Length then (EmptyMatch, offset) else (Unmatched, offset)"""
| NonTerminal x -> ibprintf b i """matchRule%s input offset""" <| capitalIdentifier x
| Sequence x | Choice x as y ->
    let fn = match y with | Sequence _ -> "Sequence" | Choice _ -> "Choice" | _ -> failwith "Internal Error"
    let processItem index item =
        ibprintfn b 0 ""
        ibprintfn b (i + 4) "(fun offset -> "
        codeGen b (i + 8) item
        ibprintf b 0 ");"
    ibprintf b i "let l = ["
    List.iteri processItem x
    ibprintf b 0 "] in match%s input offset l" fn
| ZeroOrMore x | OneOrMore x | Optional x | And x | Not x as y ->
    let fn = match y with | ZeroOrMore _ -> "ZeroOrMore" | OneOrMore _ -> "OneOrMore" | Optional _ -> "Optional" | And _ -> "And" | Not _ -> "Not" | _ -> failwith "Internal Error"
    ibprintfn b i "match%s input offset (fun offset -> " fn
    codeGen b (i + 8) x
    ibprintf b 0 ");"
| Rule (x, y, z) ->
    ibprintfn b 0 "matchRule%s (input:string) (offset:int) = " <| capitalIdentifier x
    let j = i + 4 
    if z = "" then codeGen b j y else 
        ibprintfn b j "let res = "
        codeGen b (j + 4) y
        ibprintfn b 0 " in"
        ibprintfn b (j + 4) "match res with"
        ibprintfn b (j + 4) "| (Unmatched, _) -> (Unmatched, offset)"
        ibprintf b (j + 4) "| (parsed, endOffset) -> (%s input.[offset..endOffset - 1] parsed, endOffset)" z
    ibprintfn b 0 ""

let codeGenRule b i first = function
| Rule _ as x ->
    ibprintf b i "%s " <| if first then "let rec" else "and"
    codeGen b i x
| x -> failwithf "Cannot generate code for %A" x

let codeGenHeader builder source =
    let now = System.DateTime.Now
    ibprintfn builder 0 "(*
 *  This file contains generated code.
 *
 *  Source: %s
 *  Date: %s
 *  Time: %s
 *)" source (now.ToLongDateString()) (now.ToLongTimeString())

let codeGenPreamble = "(* 
The parser expects a type similar to this to be defined
type ParseResult<'a> =
| Parsed of 'a
| Unmatched
| TerminalSymbol of string
| Production of ParseResult<'a> list
| EmptyMatch
*)

let parse line =
    let matchTerminal (terminal:string) (input:string) offset = 
        let e = offset + terminal.Length - 1 in
        if e < input.Length && input.[offset..e] = terminal then (TerminalSymbol terminal, e + 1) else (Unmatched, offset)

    let currentCharacter (s:string) i =
        if i >= s.Length then None
        else if Char.IsSurrogatePair(s, i) then Some s.[i..i+1] else Some s.[i..i]

    let matchTerminalOneOf (terminal:string) (input:string) offset = 
        match currentCharacter input offset with
        | Some c -> if terminal.Contains(c) then (TerminalSymbol c, offset + c.Length) else (Unmatched, offset)
        | None -> (Unmatched, offset)

    let matchTerminalWildcard (input:string) offset =
        match currentCharacter input offset with
        | Some c -> (TerminalSymbol c, offset + c.Length)
        | None -> (Unmatched, offset)

    let matchTerminalUnicode (category:System.Globalization.UnicodeCategory) (input:string) offset =
        if offset < input.Length && System.Char.GetUnicodeCategory(input, offset) = category
        then (TerminalSymbol input.[offset..offset], offset + 1)
        else (Unmatched, offset)

    let rec matchSequence input offset lst =
        let rec matchItems o l =
            seq {
                match l with 
                | [] -> ()
                | (fn :: t) -> match fn o with
                                  | (Unmatched, _) as x -> yield x
                                  | (_, op) as x -> yield x;
                                                    yield! matchItems op t
            } in
        let s = List.ofSeq <| matchItems offset lst in
        if List.exists (function | (Unmatched, _) -> true | _ -> false) s then (Unmatched, offset)
        else (Production <| List.map fst s, snd <| List.maxBy snd s)

    let rec matchChoice input offset = function
    | [] -> (Unmatched, offset)
    | (fn :: t) -> match fn offset with
                   | (Unmatched, _) -> matchChoice input offset t
                   | x -> x

    let rec matchItems input offset fn =
        seq {
            match fn offset with
            | (Unmatched, _) -> ()
            | (_, o) as m -> yield m;
                             yield! matchItems input o fn
        }

    let matchZeroOrMore input offset fn =
        match List.ofSeq <| matchItems input offset fn with
        | [] -> (EmptyMatch, offset)
        | lst -> (Production <| List.map fst lst, snd <| List.maxBy snd lst)

    let matchOneOrMore input offset fn =
        match List.ofSeq <| matchItems input offset fn with
        | [] -> (Unmatched, offset)
        | lst -> (Production <| List.map fst lst, snd <| List.maxBy snd lst)

    let matchOptional input offset fn =
        match fn offset with | (Unmatched, _) -> (EmptyMatch, offset) | x -> x

    let matchAnd input offset fn =
        match fn offset with | (Unmatched, _ ) -> (Unmatched, offset) | _ -> (EmptyMatch, offset)

    let matchNot input offset fn =
        match fn offset with | (Unmatched, _) -> (EmptyMatch, offset) | _ -> (Unmatched, offset)
"

let codeGenPostamble rule = 
    sprintf "
    matchRule%s line 0

" <| capitalIdentifier rule