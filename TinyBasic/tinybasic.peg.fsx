module tinybasic

open System

type Expression =
| Number of int
| Variable of string
| String of string
| Binary of Expression * string * Expression
| Unary of string * Expression
| VarList of string list
| ExprList of Expression list
| PrintSeparator of string

type Statement =
| Command of string
| Gosub of Expression
| Let of string * Expression
| Input of string list
| Goto of Expression
| If of Expression * Statement
| Print of Expression list
| Rem

type ParseResult =
| Line of int * Statement
| Immediate of Statement
| Parsed of Expression
| Unmatched
| TerminalSymbol of string
| Production of ParseResult list
| EmptyMatch

let syntaxError s = failwithf "Syntax Error %A" s

let parseString (s:string) _ =
    Parsed <| (Expression.String (s.[1..s.Length - 2].Replace("\"\"","\"")))

let parseFactor s = function
| Parsed _ as x -> x
| Production [TerminalSymbol "("; _; x ; _; TerminalSymbol ")"] -> x
| _ -> syntaxError s

let parseBinary s = function
| Production [x; EmptyMatch] -> x
| Production [Parsed x; Production y] ->
    Parsed <| List.fold (fun l -> function | Production [_; TerminalSymbol op; _; Parsed r] -> Binary (l, op, r) | _ -> syntaxError s) x y
| _ -> syntaxError s

let parseUnary s = function
| Production [EmptyMatch; _; x] -> x
| Production [TerminalSymbol op; _; Parsed x] -> Parsed <| Unary (op, x)
| _ -> syntaxError s

let parseVarList s = function
| Production [Parsed (Variable x); EmptyMatch] -> Parsed <| VarList [x]
| Production [Parsed (Variable x); Production y] ->
    Parsed (VarList (x :: List.map (function | Production [_; TerminalSymbol ","; _; Parsed (Variable y)] -> y | _ -> syntaxError s) y))
| _ -> syntaxError s

let parseExprList s = function
| Production [Parsed x; EmptyMatch; _; TerminalSymbol y] -> Parsed <| ExprList [x; PrintSeparator y]
| Production [Parsed x; EmptyMatch; _; EmptyMatch] -> Parsed <| ExprList [x]
| Production [Parsed x; Production y; _; z] ->
    let right = List.collect (function | Production [_; TerminalSymbol s; _; Parsed y] -> [PrintSeparator s; y] | _ -> syntaxError s) y
    let term = match z with | TerminalSymbol t -> [PrintSeparator t] | EmptyMatch -> [] | _ -> syntaxError z
    Parsed (ExprList (x :: List.append right term))
| _ -> syntaxError s

let parsePredicate s = function
| Production [Parsed l; _; TerminalSymbol op; _; Parsed r] -> Parsed <| Binary (l, op, r)
| _ -> syntaxError s

let parseStatement s = function
| Production [TerminalSymbol "PRINT"; _; Parsed (ExprList x)] -> Immediate <| Print x
| Production [TerminalSymbol "IF"; _; Parsed x; _; TerminalSymbol "THEN"; _; Immediate y] -> Immediate <| If (x, y)
| Production [TerminalSymbol "GOTO"; _; Parsed x] -> Immediate <| Goto x
| Production [TerminalSymbol "INPUT"; _; Parsed (VarList x)] -> Immediate <| Input x
| Production [TerminalSymbol "LET"; _; Parsed (Variable x); _; TerminalSymbol "="; _; Parsed y] -> Immediate <| Let (x, y)
| Production [TerminalSymbol "GOSUB"; _; Parsed x] -> Immediate <| Gosub x
| Production (TerminalSymbol "REM" :: _) -> Immediate <| Rem
| Immediate (Command _)  as x -> x
| _ -> syntaxError s

let parseLine s = function
| Production [EmptyMatch; _; x; _; EmptyMatch] -> x
| Production [Parsed (Number x); _; Immediate y; _; EmptyMatch] -> Line (x, y)
| _ -> syntaxError s

(*%%

line        <- number? space statement space <epsilon>              { parseLine }

statement   <- print / if / goto / input / let / gosub / rem / command    { parseStatement }

print       <- printkey space expr-list
if          <- ifkey space predicate space "THEN" space statement
goto        <- gotokey space expression
input       <- inputkey space var-list
let         <- letkey space var space '=' space expression
gosub       <- gosubkey space expression
rem         <- remkey <anychar>*

command     <- ({Lu} / {Ll})+                               { (fun (s:string) _ -> Immediate <| Command (s.ToUpper())) }

printkey    <- [Pp] [Rr] ([Ii] [Nn] [Tt])?                  { (fun _ _ -> TerminalSymbol "PRINT") }
ifkey       <- [Ii] [Ff]                                    { (fun _ _ -> TerminalSymbol "IF") }
gotokey     <- [Gg] [Oo] [Tt] [Oo]                          { (fun _ _ -> TerminalSymbol "GOTO") }
inputkey    <- [Ii] [Nn] ([Pp] [Uu] [Tt])?                  { (fun _ _ -> TerminalSymbol "INPUT") }
letkey      <- ([Ll] [Ee] [Tt])?                            { (fun _ _ -> TerminalSymbol "LET") }
gosubkey    <- [Gg] [Oo] [Ss] [Uu] [Bb]                     { (fun _ _ -> TerminalSymbol "GOSUB") }
remkey      <- ([Rr] [Ee] [Mm]) / "'"                       { (fun _ _ -> TerminalSymbol "REM") }

expr-list   <- (string / expression) (space [,;] space (string / expression))* space [,;]?  { parseExprList }

var-list    <- var (space ',' space var)*                   { parseVarList }

predicate   <- expression space relop space expression      { parsePredicate }

expression  <- term (space [+-] space term)*                { parseBinary }

term        <- unary (space [*/] space unary)*              { parseBinary }

unary       <- [+-]? space factor                           { parseUnary }

factor      <- var / number / ('(' space expression space ')')      { parseFactor }

var         <- {Lu} / {Ll}                                  { (fun (s:string) _ -> Parsed <| (Variable (s.ToUpperInvariant()))) }

number      <- {Nd}+                                        { (fun s _ -> Parsed <| Number (Int32.Parse(s))) }

relop       <- "<>" / "<=" / '<' / "><" / ">=" / '>' / '='

string      <- '\"' ((!'\"' <anychar>) / "\"\"")* '\"'      { parseString }

space       <- [ \t]*                                       { (fun _ _ -> EmptyMatch) }

%%*)

type Context (program:(int * Statement * string) list, variables:Map<string,Expression>, next:int, stack:int list) =
    member this.Program = program
    member this.Variables = variables
    member this.Next = next
    member this.Stack = stack

let comparison op = 
    let fn = match op with | ">" -> (>) | ">=" -> (>=) | "<>" | "><" -> (<>) | "<" -> (<) | "<=" -> (<=) | "=" -> (=) | x -> failwithf "Unexpected operator %A" x
    (fun a b -> if fn a b then -1 else 0)

let rec evalAsNumber (context:Context) x =
    match evalExpression context x with
    | Number a -> a
    | a -> failwithf "Expecting number not %A" a
and evalExpression (context:Context) = function
| Number _ as x -> x
| Variable x -> match Map.tryFind x context.Variables with | Some y -> y | None -> Number 0
| String _ as x -> x
| Binary (x, op, y) -> 
    let left = evalAsNumber context x
    let right = evalAsNumber context y
    let fn = match op with | "+" -> (+) | "-" -> (-) | "*" -> (*) | "/" -> (/) | a -> comparison a 
    Number <| fn left right
| Unary ("+", x) -> evalExpression context x
| Unary ("-", x) -> Number <| -evalAsNumber context x
| PrintSeparator _ as x -> x
| x -> failwith "Internal Error"
                       
let listProgram (context:Context) = for (_,_,o) in context.Program do printfn "%s" o

let setVariable (context:Context) name value = Context(context.Program, Map.add name value context.Variables, context.Next, context.Stack)

let setLine (context:Context) line = Context(context.Program, context.Variables, line, context.Stack)

let setGosub (context:Context) line = Context(context.Program, context.Variables, line, (context.Next :: context.Stack))

let gosubReturn (context:Context) =
    match context.Stack with
    | [] -> failwith "RETURN without GOSUB"
    | (head :: tail) -> Context(context.Program, context.Variables, head, tail)

let findLine (context:Context) = List.tryFind (fun (l, _, _) -> l >= context.Next) context.Program

let rec runProgram (context:Context) =
    if context.Next < 0 then context
    else
        match findLine context with
        | Some (n, s, _) -> evalImmediate (setLine context (n + 1)) s |> runProgram
        | None -> setLine context -1
and evalImmediate context = function
| Command "CLEAR" -> Context([], Map.empty, 0, [])
| Command "LIST" -> 
    listProgram context
    context
| Command "RUN" -> runProgram <| setLine context 0
| Command "END" -> Context(context.Program, context.Variables, -1, [])
| Command "RETURN" -> gosubReturn context
| Command x -> failwithf "Unknown command %A" x
| Goto x -> 
    let cp = setLine context <| evalAsNumber context x
    if context.Next < 0 then runProgram cp else cp
| Gosub x -> 
    let cp = setGosub context <| evalAsNumber context x
    if context.Next < 0 then runProgram cp else cp
| Let (x,y) -> setVariable context x <| evalExpression context y
| Print x ->
    let rec doPrint = function
    | [] -> printfn ""
    | [PrintSeparator ";"] -> ()
    | [PrintSeparator ","] -> printf "\t"
    | (head :: tail) -> 
        match head with
        | Number n -> printf "%d" n
        | String s -> printf "%s" s
        | PrintSeparator "," -> printf "\t"
        | PrintSeparator ";" -> ()
        | _ -> failwithf "Eval error %A" head
        doPrint tail

    doPrint <| List.map (evalExpression context) x
    context
| If (x, y) ->
    match evalExpression context x with
    | Number 0 -> context
    | Number _ -> evalImmediate context y
    | a -> failwithf "Eval error %A" a
| Input x ->
    let rec doInput context = function
    | [] -> context
    | (head :: tail) ->
        printf "?"
        match Int32.TryParse(Console.ReadLine()) with
        | (true, i) -> doInput (setVariable context head (Number i)) tail
        | (false, _) -> failwith "Input error"
    doInput context x
| Rem -> context

let addLine (context:Context) l s o =
    let rec insertLine = function
    | [] -> [(l,s,o)]
    | ((currLine, _, _) as curr :: tail) ->
        if currLine = l then ((l,s,o) :: tail)
        else if currLine < l then (curr :: insertLine tail)
        else ((l,s,o) :: curr :: tail)

    Context(insertLine context.Program, context.Variables, context.Next, context.Stack)

do
    let rec runBasic context =
        printf "?"
        let line = Console.ReadLine()
        if line.StartsWith("!") then
            match line.[1..].Split([|' '|], 2) with
            | [|"load"; file|] -> 
                let lines = Seq.ofArray <| System.IO.File.ReadAllLines(file)
                let program = Seq.fold (fun c l -> match parse l with | (Line (x, y), _) -> ((x, y, l) :: c) | _ -> failwith "Error reading file") [] lines
                runBasic <| Context(List.rev program, Map.empty, 0, [])
            | [|"save"; file|] ->
                System.IO.File.WriteAllLines(file, List.map (fun (_,_,l) -> l) context.Program)
                runBasic context
            | [|"quit"; file|] | [|"exit"; file|] -> ()
            | _ -> printfn "Did not understand %A" line
                   runBasic context
        else
            try
                match parse line with
                | (Immediate x, _) -> 
                    runBasic <| evalImmediate context x
                | (Line (x, y), _) ->
                    runBasic <| addLine context x y line
                | (Unmatched, _) ->
                    printfn "Syntax error"
                    runBasic context
                | x -> printfn "%A" x
                       runBasic context
            with
            | ex -> printfn "%s" ex.Message
                    runBasic context

    printfn "Running Tiny Basic (F# edition)"
    runBasic <| Context([], Map.empty, 0, [])
