open Peg

let unexpected x =
    printfn "Unexpected pattern %A" x
    failwith "parse error"

let g = 
    // bool <- "true" / "false"
    let boolRule = GrammarRule<bool>(Choice [Terminal "true"; Terminal "false"], (fun s _ -> Parsed (s = "true")))

    // paren <- "(" expr ")"
    let parseParen _ = function
    | Production [TerminalSymbol "("; x; TerminalSymbol ")"] -> x
    | x -> unexpected x
    let parenRule = GrammarRule<bool>(Sequence [Terminal "("; NonTerminal "expr"; Terminal ")"], parseParen)

    // not <- "!" atom
    let parseNot _ = function
    | Production [TerminalSymbol "!"; Parsed x] -> Parsed <| not x
    | x -> unexpected x
    let notRule = GrammarRule<bool>(Sequence [Terminal "!"; NonTerminal "atom"], parseNot)

    // atom <- bool / paren / not
    let atomRule = GrammarRule<bool>(Choice [NonTerminal "bool"; NonTerminal "paren"; NonTerminal "not"], (fun _ x -> x))

    // and <- atom ("&" and)?
    // or <- and ("!" or)?
    let parseBin _ = function
    | Production [x; EmptyMatch] -> x
    | Production [Parsed x; Production [TerminalSymbol "&"; Parsed y]] -> Parsed (x && y)
    | Production [Parsed x; Production [TerminalSymbol "|"; Parsed y]] -> Parsed (x || y)
    | x -> unexpected x
    let andRule = GrammarRule<bool>(Sequence [NonTerminal "atom"; Optional (Sequence [Terminal "&"; NonTerminal "and"])], parseBin)
    let orRule = GrammarRule<bool>(Sequence [NonTerminal "and"; Optional (Sequence [Terminal "|"; NonTerminal "or"])], parseBin)

    // expr <- or
    let exprRule = GrammarRule<bool>(NonTerminal "or", (fun _ x -> x))

    // start <- expr <epsilon>
    let parseStart _ = function
    | Production [x; EmptyMatch] -> x
    | x -> unexpected x
    let startRule = GrammarRule<bool>(Sequence [NonTerminal "expr"; Epsilon], parseStart)

    Map.ofList [("bool", boolRule); ("paren", parenRule); ("not", notRule); ("atom", atomRule);
                ("and", andRule); ("or", orRule); ("expr", exprRule); ("start", startRule)]

type BooleanExpr =
| Atom of bool
| Binary of BooleanExpr * string * BooleanExpr
| Not of BooleanExpr

let rec evalBool = function
| Atom x -> x
| Binary (left, "&", right) -> if evalBool left then evalBool right else false
| Binary (left, "|", right) -> if evalBool left then true else evalBool right
| Binary (_, x, _) -> failwith <| "Unexpected binary operator: " + x
| Not x -> not <| evalBool x

let boolGrammar = 
    // bool <- "true" / "false"
    let boolRule = GrammarRule<BooleanExpr>(Choice [Terminal "true"; Terminal "false"], (fun s _ -> Parsed (Atom (s = "true"))))
    
    // space <- " "*
    let spaceRule = GrammarRule<BooleanExpr>(ZeroOrMore (Terminal " "), (fun _ _ -> EmptyMatch))

    // paren <- "(" space expr space ")"
    let boolParen _ = function
    | Production [TerminalSymbol "("; EmptyMatch; x; EmptyMatch; TerminalSymbol ")"] -> x
    | x -> unexpected x
    let parenRule = GrammarRule<BooleanExpr>(Sequence [Terminal "("; NonTerminal "space"; NonTerminal "expr"; NonTerminal "space"; Terminal ")"], boolParen)

    // not <- "!" atom
    let boolNot _ = function
    | Production [TerminalSymbol "!"; Parsed x] -> Parsed (Not x)
    | x -> unexpected x
    let notRule = GrammarRule<BooleanExpr>(Sequence [Terminal "!"; NonTerminal "atom"], boolNot)

    // atom <- "bool" / "paren" / "not"
    let atomRule = GrammarRule<BooleanExpr>(Choice [NonTerminal "bool"; NonTerminal "paren"; NonTerminal "not"], (fun _ x -> x))

    // or <- atom space ("|" space or)?
    // and <- or space ("&" space and)?
    let boolBinary _ = function
    | Production [x; EmptyMatch; EmptyMatch] -> x
    | Production [Parsed x; EmptyMatch; Production [TerminalSymbol op; EmptyMatch; Parsed y]] -> Parsed (Binary (x, op, y))
    | x -> unexpected x
    let orRule = GrammarRule<BooleanExpr>(Sequence [NonTerminal "atom"; NonTerminal "space"; Optional (Sequence [Terminal "|"; NonTerminal "space"; NonTerminal "or"])], boolBinary)
    let andRule = GrammarRule<BooleanExpr>(Sequence [NonTerminal "or"; NonTerminal "space"; Optional (Sequence [Terminal "&"; NonTerminal "space"; NonTerminal "and"])], boolBinary)

    // expr <- and
    let exprRule = GrammarRule<BooleanExpr>(NonTerminal "and", (fun _ x -> x))

    // start <- space expr space <epsilon>
    let boolStart _ = function
    | Production [EmptyMatch; x; EmptyMatch; EmptyMatch] -> x
    | x -> unexpected x
    let startRule = GrammarRule<BooleanExpr>(Sequence [NonTerminal "space"; NonTerminal "expr"; NonTerminal "space"; Epsilon], boolStart)

    Map.ofList [("bool", boolRule); ("space", spaceRule); ("paren", parenRule); ("not", notRule);
                ("atom", atomRule); ("or", orRule); ("and", andRule); ("expr", exprRule); ("start", startRule)]

let parseAndEvalBool expr =
    match parseExpression boolGrammar "start" expr with
    | (Parsed b, _) -> printfn "%s = %A" expr (evalBool b)
    | x -> unexpected x