module PegOfPeg

open Peg
open System
open System.Globalization

let emptyParser _ x = x

let unexpected x =
    failwithf "unexpected %A" x

let pegGrammar =
    // hexDigit <- [0123456789abcdefABCDEF]
    let hexDigitRule = GrammarRule<Expression>(TerminalOneOf "0123456789abcdefABCDEF", emptyParser)

    // escapedCharacter <- "\\" ([abfnrtv0\\"'[\]] / ('u' hexDigit hexDigit hexDigit hexDigit))
    let parseEscapedCharacter (s:string) = function
    | Production [TerminalSymbol "\\"; TerminalSymbol x] -> 
        TerminalSymbol <| match x with | "a" -> "\a" | "b" -> "\b" | "f" -> "\f" | "n" -> "\n" | "r" -> "\r" | "t" -> "\t" | "v" -> "\v" | "0" -> "\0" | _ -> x
    | Production [TerminalSymbol "\\"; Production [ TerminalSymbol _; _; _; _; _]] ->
        TerminalSymbol <| Char.ConvertFromUtf32(Int32.Parse(s.[2..5], NumberStyles.AllowHexSpecifier))
    | x -> unexpected x
    let escapedCharacterRule = GrammarRule<Expression>(Sequence [Terminal "\\"; Choice [TerminalOneOf "abfnrtv0\\\"'[]"; Sequence [Terminal "u"; NonTerminal "hexDigit"; NonTerminal "hexDigit"; NonTerminal "hexDigit"; NonTerminal "hexDigit"]]], parseEscapedCharacter)

    // safeCharacter <- escapedCharacter / (!["\\\n] <anychar>)
    // oneofCharacter <- escapedCharacter / (![\\\]] <anychar>)
    let parseSafeCharacter _ = function
    | TerminalSymbol _ as x -> x
    | Production [EmptyMatch; TerminalSymbol _ as x] -> x
    | x -> unexpected x
    let safeCharacterRule = GrammarRule<Expression>(Choice [NonTerminal "escapedCharacter"; Sequence [Not (TerminalOneOf "\"\\\n"); TerminalWildcard]], parseSafeCharacter)
    let oneofCharacterRule = GrammarRule<Expression>(Choice [NonTerminal "escapedCharacter"; Sequence [Not (TerminalOneOf "\\]"); TerminalWildcard]], parseSafeCharacter)

    // terminalUnicode <- "{" ( "Lu" / "Ll" / "Lt" / "Lm" / "Lo" /  "Mn" / "Mc" / "Me" / "Nd" / "Nl" / "No" / "Pc" / "Pd" / 
    //                          "Ps" / "Pe" / "Pi" / "Pf" / "Po" / "Sm" / "Sc" / "Sk" / "So" / "Zs" / "Zl" / "Zp" / "Cc" / 
    //                          "Cf" / "Cs" / "Co" / "Cn" ) "}"
    let parseTerminalUnicode _ = function
    | Production [TerminalSymbol "{"; TerminalSymbol x; TerminalSymbol "}"] ->
        Parsed (TerminalUnicode <| match x with
                                   | "Lu" -> UnicodeCategory.UppercaseLetter | "Ll" -> UnicodeCategory.LowercaseLetter | "Lt" -> UnicodeCategory.TitlecaseLetter
                                   | "Lm" -> UnicodeCategory.ModifierLetter | "Lo" -> UnicodeCategory.OtherLetter  | "Mn" -> UnicodeCategory.NonSpacingMark
                                   | "Mc" -> UnicodeCategory.SpacingCombiningMark | "Me" -> UnicodeCategory.EnclosingMark | "Nd" -> UnicodeCategory.DecimalDigitNumber
                                   | "Nl" -> UnicodeCategory.LetterNumber | "No" -> UnicodeCategory.OtherNumber | "Pc" -> UnicodeCategory.ConnectorPunctuation 
                                   | "Pd" -> UnicodeCategory.DashPunctuation | "Ps" -> UnicodeCategory.OpenPunctuation | "Pe" -> UnicodeCategory.ClosePunctuation
                                   | "Pi" -> UnicodeCategory.InitialQuotePunctuation | "Pf" -> UnicodeCategory.FinalQuotePunctuation | "Po" -> UnicodeCategory.OtherPunctuation 
                                   | "Sm" -> UnicodeCategory.MathSymbol | "Sc" -> UnicodeCategory.CurrencySymbol | "Sk" -> UnicodeCategory.ModifierSymbol 
                                   | "So" -> UnicodeCategory.OtherSymbol | "Zs" -> UnicodeCategory.SpaceSeparator | "Zl" -> UnicodeCategory.LineSeparator
                                   | "Zp" -> UnicodeCategory.ParagraphSeparator | "Cc" -> UnicodeCategory.Control | "Cf" -> UnicodeCategory.Format
                                   | "Cs" -> UnicodeCategory.Surrogate | "Co" -> UnicodeCategory.PrivateUse | "Cn" -> UnicodeCategory.OtherNotAssigned
                                   | x -> unexpected x)
    | x -> unexpected x
    let terminalUnicodeRule = GrammarRule<Expression>(Sequence [ Terminal "{"; Choice [Terminal "Lu"; Terminal "Ll"; Terminal "Lt"; Terminal "Lm"; Terminal "Lo";  
                                                                                        Terminal "Mn"; Terminal "Mc"; Terminal "Me"; Terminal "Nd"; Terminal "Nl"; 
                                                                                        Terminal "No"; Terminal "Pc"; Terminal "Pd"; Terminal "Ps"; Terminal "Pe"; 
                                                                                        Terminal "Pi"; Terminal "Pf"; Terminal "Po"; Terminal "Sm"; Terminal "Sc"; 
                                                                                        Terminal "Sk"; Terminal "So"; Terminal "Zs"; Terminal "Zl"; Terminal "Zp"; 
                                                                                        Terminal "Cc"; Terminal "Cf"; Terminal "Cs"; Terminal "Co"; Terminal "Cn"]; 
                                                                                        Terminal "}"], parseTerminalUnicode)

    // terminalOneOf <- "[" oneofCharacter+ "]"
    let parseTerminalOneOf _ = function
    | Production [TerminalSymbol "["; Production x; TerminalSymbol "]"] ->
        Parsed <| TerminalOneOf (List.map (function | TerminalSymbol x -> x | q -> unexpected q) >> List.reduce (+) <| x)
    | x -> unexpected x
    let terminalOneOfRule = GrammarRule<Expression>(Sequence [Terminal "["; OneOrMore (NonTerminal "oneofCharacter"); Terminal "]"],  parseTerminalOneOf)

    // terminalCharacter <- "'" safeCharacter "'"
    let parseTerminalCharacter _ = function
    | Production [TerminalSymbol "'"; TerminalSymbol x; TerminalSymbol "'"] -> Parsed <| Terminal x
    | x -> unexpected x
    let terminalCharacterRule = GrammarRule<Expression>(Sequence [Terminal "'"; NonTerminal "safeCharacter"; Terminal "'"], parseTerminalCharacter)

    // terminalWord <- "\"" safeCharacter+ "\""
    let parseTerminalWord _ = function
    | Production [TerminalSymbol "\""; Production x; TerminalSymbol "\""] ->
        Parsed <| Terminal (List.map (function | TerminalSymbol x -> x | q -> unexpected q) >> List.reduce (+) <| x)
    | x -> unexpected x
    let terminalWordRule = GrammarRule<Expression>(Sequence [Terminal "\""; OneOrMore (NonTerminal "safeCharacter"); Terminal "\""],  parseTerminalWord)

    // terminal <- terminalWord / terminalCharacter / terminalOneOf / terminalUnicode / "<anychar>" / "<epsilon>"
    let parseTerminalRule _ = function
    | TerminalSymbol "<anychar>" -> Parsed TerminalWildcard
    | TerminalSymbol "<epsilon>" -> Parsed Epsilon
    | x -> x
    let terminalRule = GrammarRule<Expression>(Choice [NonTerminal "terminalWord"; NonTerminal "terminalCharacter"; NonTerminal "terminalOneOf"; 
                                                        NonTerminal "terminalUnicode"; Terminal "<anychar>"; Terminal "<epsilon>"],
                                                        parseTerminalRule)

    // space <- [ \t\r\n]+
    let spaceRule = GrammarRule<Expression>(OneOrMore (TerminalOneOf " \t\r\n"), emptyParser)

    // name <- ({Lu} / {Ll} / '-') ({Lu} / {Ll} / {Nd} / [_-])* 
    let nameRule = GrammarRule<Expression>(Sequence [Choice [TerminalUnicode UnicodeCategory.UppercaseLetter; TerminalUnicode UnicodeCategory.LowercaseLetter; Terminal "_"];
                                                     ZeroOrMore (Choice [TerminalUnicode UnicodeCategory.UppercaseLetter; TerminalUnicode UnicodeCategory.LowercaseLetter; TerminalUnicode UnicodeCategory.DecimalDigitNumber; TerminalOneOf "_-"])],
                                                     (fun s _ -> TerminalSymbol s))

    // nonterminal <- name
    let parseNonTerminal _ = function
    | TerminalSymbol x -> Parsed <| NonTerminal x
    | x -> unexpected x
    let nonTerminalRule = GrammarRule<Expression>(NonTerminal "name", parseNonTerminal)

    // atom <- terminal / nonterminal / ("(" space? expr space? ")")
    let parseAtom _ = function
    | Parsed _ as x -> x
    | Production [TerminalSymbol "("; _; Parsed _ as x; _; TerminalSymbol ")"] -> x
    | x -> unexpected x
    let atomRule = GrammarRule<Expression>(Choice [NonTerminal "terminal"; NonTerminal "nonterminal"; 
                                                   Sequence [Terminal "("; Optional (NonTerminal "space"); NonTerminal "expr"; Optional (NonTerminal "space"); Terminal ")"]],
                                                   parseAtom)

    // unary <- ([!&] unary) / (atom [*+?])?
    let parseUnary _ = function
    | Production [TerminalSymbol "!"; Parsed x] -> Parsed <| Not x
    | Production [TerminalSymbol "&"; Parsed x] -> Parsed <| And x
    | Production [Parsed x; TerminalSymbol "*"] -> Parsed <| ZeroOrMore x
    | Production [Parsed x; TerminalSymbol "+"] -> Parsed <| OneOrMore x
    | Production [Parsed x; TerminalSymbol "?"] -> Parsed <| Optional x
    | Production [Parsed _ as x; EmptyMatch] -> x
    | x -> unexpected x
    let unaryRule = GrammarRule<Expression>(Choice [Sequence [TerminalOneOf "!&"; NonTerminal "unary"]; 
                                                    Sequence [NonTerminal "atom"; Optional(TerminalOneOf "*+?")]], parseUnary)

    // sequence <- unary (space unary)*
    let parseSequence _ = function
    | Production [Parsed _ as x; EmptyMatch] -> x
    | Production [Parsed x; Production y] -> 
        Parsed <| Sequence (x :: List.map (function | Production [_; Parsed a] -> a | q -> unexpected q) y)
    | x -> unexpected x
    let sequenceRule = GrammarRule<Expression>(Sequence [NonTerminal "unary"; ZeroOrMore (Sequence [NonTerminal "space"; NonTerminal "unary"])], parseSequence)

    // choice <- sequence (space? '/' space? sequence)*
    let parseChoice _ = function
    | Production [Parsed _ as x; EmptyMatch] -> x
    | Production [Parsed x; Production y] ->
        Parsed <| Choice (x :: List.map (function | Production [_; TerminalSymbol "/"; _; Parsed a] -> a | q -> unexpected q) y)
    | x -> unexpected x
    let choiceRule = GrammarRule<Expression>(Sequence [NonTerminal "sequence"; ZeroOrMore (Sequence [Optional (NonTerminal "space"); Terminal "/"; Optional (NonTerminal "space"); NonTerminal "sequence"])],
                                             parseChoice)
    // expr <- choice
    let exprRule = GrammarRule<Expression>(NonTerminal "choice", emptyParser)

    // codechar  <- (![{}\] <anychar>) / ('\\' [{}\\]) / ('{' codechar* '}')
    let codecharRule = GrammarRule<Expression>(Choice [Sequence [Not (TerminalOneOf "{}\\"); TerminalWildcard];
                                                       Sequence [Terminal "\\"; TerminalOneOf "{}\\"];
                                                       Sequence [Terminal "{"; ZeroOrMore (NonTerminal "codechar"); Terminal "}"]], emptyParser)

    // codeblock <- '{' codechar* '}'
    let parseCodeblock (s:string) _ =
        let inner = s.[1..s.Length - 2]
        let unescaped = inner.Replace("\\{", "{").Replace("\\}", "}").Replace("\\\\", "\\")
        TerminalSymbol unescaped
    let codeblockRule = GrammarRule<Expression>(Sequence [Terminal "{"; ZeroOrMore (NonTerminal "codechar"); Terminal "}"], parseCodeblock)

    // rule <- name space? "<-" space? expr space? codeblock? space? <epsilon>
    let parseRule _ = function
    | Production [TerminalSymbol x; _; TerminalSymbol "<-"; _; Parsed y; _; EmptyMatch; _; EmptyMatch] -> Parsed <| Rule (x, y, "")
    | Production [TerminalSymbol x; _; TerminalSymbol "<-"; _; Parsed y; _; TerminalSymbol z; _; EmptyMatch] -> Parsed <| Rule (x, y, z)
    | x -> unexpected x
    let ruleRule = GrammarRule<Expression>(Sequence [NonTerminal "name"; Optional (NonTerminal "space"); Terminal "<-"; Optional (NonTerminal "space"); NonTerminal "expr"; Optional (NonTerminal "space");
                                                     Optional (NonTerminal "codeblock"); Optional (NonTerminal "space"); Epsilon],
                                           parseRule)

    Map.ofList [("hexdigit", hexDigitRule); ("escapedCharacter", escapedCharacterRule); ("safeCharacter", safeCharacterRule);
                ("oneofCharacter", oneofCharacterRule); ("terminalUnicode", terminalUnicodeRule); ("terminalOneOf", terminalOneOfRule);
                ("terminalCharacter", terminalCharacterRule); ("terminalWord", terminalWordRule); ("terminal", terminalRule);
                ("space", spaceRule); ("name", nameRule); ("nonterminal", nonTerminalRule); ("atom", atomRule); ("unary", unaryRule);
                ("sequence", sequenceRule); ("choice", choiceRule); ("expr", exprRule); ("codechar", codecharRule); ("codeblock", codeblockRule); 
                ("rule", ruleRule)]

let applyPegGrammar s =
    parseExpression pegGrammar "rule" s
