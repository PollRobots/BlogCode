module tinybasic

type Expression =
| Number of float
| Variable of string
| String of string
| Array of string * Expression
| ArrayInstance of float []
| Binary of Expression * string * Expression
| Unary of string * Expression
| VarList of (string * Expression) list
| ExprList of Expression list
| PrintSeparator of string
| Procedure of string * Expression list
| Function of string list * Expression
| Empty

type Statement =
| Command of string
| Gosub of Expression
| Let of string * Expression
| LetArray of string * Expression * Expression
| Input of (string * Expression) list
| Goto of Expression
| If of Expression * Statement
| Print of Expression list
| Rem
| Dim of (string * Expression) list
| For of (string * Expression * Expression * Expression)
| Next of string
| DefFn of (string * string list * Expression)

type ParseResult =
| Line of int * Statement list
| Unnumbered of Statement list
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

let parseArray s = function
| Production [Parsed (Variable x); TerminalSymbol "("; Parsed y; TerminalSymbol ")"] -> Parsed <| Array (x, y)
| _ -> syntaxError s

let parseDimList s = function
| Production [Parsed x; EmptyMatch] -> Parsed <| ExprList [x]
| Production [Parsed x; Production y] ->
    Parsed <| ExprList (x :: List.map (function | Production[_; TerminalSymbol ","; _; Parsed e] -> e | _ -> syntaxError s) y)
| _ -> syntaxError s

let parseVarList s = function
| Production [Parsed (Variable x); EmptyMatch] -> Parsed <| VarList [(x, Empty)]
| Production [Parsed (Array (x,y)); EmptyMatch] -> Parsed <| VarList [(x,y)]
| Production [Parsed x; Production y] ->
    let head = match x with | Variable a -> (a, Empty) | Array (a,b) -> (a,b) | _ -> syntaxError s
    Parsed (VarList (head :: List.map (function 
                                       | Production [_; TerminalSymbol ","; _; Parsed (Variable a)] -> (a,Empty) 
                                       | Production [_; TerminalSymbol ","; _; Parsed (Array (a,b))] -> (a,b)
                                       | _ -> syntaxError s) y))
| _ -> syntaxError s

let parseExprList s = function
| Production [Parsed x; EmptyMatch; _; TerminalSymbol y] -> Parsed <| ExprList [x; PrintSeparator y]
| Production [Parsed x; EmptyMatch; _; EmptyMatch] -> Parsed <| ExprList [x]
| Production [Parsed x; Production y; _; z] ->
    let right = List.collect (function 
                              | Production [_; TerminalSymbol s; _; Parsed y] -> [PrintSeparator s; y] 
                              | Production [_; EmptyMatch; _; Parsed y] -> [PrintSeparator ";"; y]
                              | _ -> syntaxError s) y
    let term = match z with | TerminalSymbol t -> [PrintSeparator t] | EmptyMatch -> [] | _ -> syntaxError z
    Parsed (ExprList (x :: List.append right term))
| _ -> syntaxError s

let parseProcedure s = function
| Production [TerminalSymbol x; TerminalSymbol "("; _; Parsed (ExprList y); _; TerminalSymbol ")"] -> Parsed <| Procedure (x, y)
| _ -> syntaxError s

let parsePredicate s = function
| Production [Parsed l; _; TerminalSymbol op; _; Parsed r] -> Parsed <| Binary (l, op, r)
| _ -> syntaxError s

let parseStatement s = function
| Production [TerminalSymbol "PRINT"; EmptyMatch] -> Immediate <| Print [Expression.String ""] 
| Production [TerminalSymbol "PRINT"; Production [_; Parsed (ExprList x)]] -> Immediate <| Print x
| Production [TerminalSymbol "IF"; _; Parsed x; _; TerminalSymbol "THEN"; _; Parsed (Number _ as y)] -> Immediate <| If (x, Goto y)
| Production [TerminalSymbol "IF"; _; Parsed x; _; TerminalSymbol "THEN"; _; Immediate y] -> Immediate <| If (x, y)
| Production [TerminalSymbol "GOTO"; _; Parsed x] -> Immediate <| Goto x
| Production [TerminalSymbol "INPUT"; _; Parsed (VarList x)] -> Immediate <| Input x
| Production [TerminalSymbol "LET"; _; Parsed (Variable x); _; TerminalSymbol "="; _; Parsed y] -> Immediate <| Let (x, y)
| Production [TerminalSymbol "LET"; _; Parsed (Array (x,y)); _; TerminalSymbol "="; _; Parsed z] -> Immediate <| LetArray (x, y, z)
| Production [TerminalSymbol "GOSUB"; _; Parsed x] -> Immediate <| Gosub x
| Production (TerminalSymbol "REM" :: _) -> Immediate <| Rem
| Production [TerminalSymbol "DIM"; _; Parsed (ExprList x)] -> Immediate (Dim <| List.map (function | Array (n,e) -> (n,e) | _ -> syntaxError s) x)
| Production [TerminalSymbol "FOR"; _; Parsed (Variable x); _; TerminalSymbol "="; _; Parsed y; _; TerminalSymbol "TO"; _; Parsed z; EmptyMatch] -> Immediate <| For (x, y, z, Number 1.0)
| Production [TerminalSymbol "FOR"; _; Parsed (Variable x); _; TerminalSymbol "="; _; Parsed y; _; TerminalSymbol "TO"; _; Parsed z; 
              Production [_; TerminalSymbol "STEP"; _; Parsed s]] -> Immediate <| For (x, y, z, s)
| Production [TerminalSymbol "NEXT"; EmptyMatch] -> Immediate <| Next ""
| Production [TerminalSymbol "NEXT"; Production [_; Parsed (Variable x)]] -> Immediate <| Next x
| Production [TerminalSymbol "DEF"; _; TerminalSymbol "FN"; Parsed (Variable x); _; TerminalSymbol "("; _; Parsed (VarList y); _; TerminalSymbol ")"; _; Parsed z] ->
    Immediate <| DefFn (x, List.map fst y, z)
| Immediate (Command _)  as x -> x
| _ -> syntaxError s

let parseLine s = function
| Production [EmptyMatch; _; x; _; EmptyMatch] -> x
| Production [Parsed (Number x); _; Unnumbered y; _; EmptyMatch] -> Line (int(x), y)
| _ -> syntaxError s

let parseStatementList s = function
| Production [Immediate x; EmptyMatch] -> Unnumbered [x]
| Production [Immediate x; Production y] ->
    Unnumbered <| (x :: List.map (function | Production [_; TerminalSymbol ":"; _; Immediate a] -> a| a -> syntaxError a) y)
| _ -> syntaxError s

let parseKeyword (s:string) _ = TerminalSymbol <| s.ToUpperInvariant()

(*%%

line        <- line-number? space stmnt-list space <epsilon>     { parseLine }

line-number <- {Nd}+                                        { (fun s _ -> Parsed (Number (float(s)))) }

stmnt-list  <- statement (space ':' space statement)*       { parseStatementList }

statement   <- print / if / goto / input / let / gosub / rem / dim / for / next / deffn / command    { parseStatement }

print       <- printkey (space expr-list)?
if          <- ifkey space predicate space thenkey space (line-number / statement)
goto        <- gotokey space expression
input       <- inputkey space var-list
let         <- letkey space (array / var) space '=' space expression
gosub       <- gosubkey space expression
rem         <- remkey <anychar>*
dim         <- dimkey space dim-list
for         <- forkey space var space '=' space expression space tokey space expression (space stepkey space expression)?
next        <- nextkey (space var)?
deffn       <- defkey space funkey var space '(' space fn-args space ')' space expression

command     <- name                                         { (fun s -> function | TerminalSymbol x -> Immediate <| Command x | _ -> syntaxError s) }

printkey    <- [Pp] [Rr] ([Ii] [Nn] [Tt])?                  { (fun _ _ -> TerminalSymbol "PRINT") }
ifkey       <- [Ii] [Ff]                                    { parseKeyword }
thenkey     <- [Tt] [Hh] [Ee] [Nn]                          { parseKeyword }
gotokey     <- [Gg] [Oo] [Tt] [Oo]                          { parseKeyword }
inputkey    <- [Ii] [Nn] ([Pp] [Uu] [Tt])?                  { parseKeyword }
letkey      <- ([Ll] [Ee] [Tt])?                            { (fun _ _ -> TerminalSymbol "LET") }
gosubkey    <- [Gg] [Oo] [Ss] [Uu] [Bb]                     { parseKeyword }
remkey      <- ([Rr] [Ee] [Mm]) / "'"                       { (fun _ _ -> TerminalSymbol "REM") }
dimkey      <- [Dd] [Ii] [Mm]                               { parseKeyword }
forkey      <- [Ff] [Oo] [Rr]                               { parseKeyword }
tokey       <- [Tt] [Oo]                                    { parseKeyword }
nextkey     <- [Nn] [Ee] [Xx] [Tt]                          { parseKeyword }
stepkey     <- [Ss] [Tt] [Ee] [Pp]                          { parseKeyword }
defkey      <- [Dd] [Ee] [Ff]                               { parseKeyword }
funkey      <- [Ff] [Nn]                                    { parseKeyword }

expr-list   <- (string / expression) (space [,;]? space (string / expression))* space [,;]?  { parseExprList }

var-list    <- (array / var) (space ',' space (array / var))*                   { parseVarList }

fn-args     <- var (space ',' space var)*                   { parseVarList }

dim-list    <- array (space ',' space array)*               { parseDimList }

procedure   <- name '(' space expr-list space ')'           { parseProcedure }

name        <- ({Lu} / {Ll})+                               { (fun (s:string) _ -> TerminalSymbol (s.ToUpper())) }

predicate   <- expression space relop space expression      { parsePredicate }

expression  <- term (space [+-] space term)*                { parseBinary }

term        <- unary (space [*/] space unary)*              { parseBinary }

unary       <- [+-]? space factor                           { parseUnary }

factor      <- array / procedure / var / number / ('(' space expression space ')')      { parseFactor }

var         <- ({Lu} / {Ll}) {Nd}?                          { (fun (s:string) _ -> Parsed <| (Variable (s.ToUpperInvariant()))) }

number      <- real / int                                   { (fun s _ -> Parsed <| Number (float(s))) }

real        <- {Nd}* '.' {Nd}+

int         <- {Nd}+

array       <- var '(' expression ')'                       { parseArray }

relop       <- "<>" / "<=" / '<' / "><" / ">=" / '>' / '='

string      <- '\"' ((!'\"' <anychar>) / "\"\"")* '\"'      { parseString }

space       <- [ \t]*                                       { (fun _ _ -> EmptyMatch) }

%%*)

type Context (program:(int * Statement list * string) list, variables:Map<string,Expression>, next:int, stack:int list, fors:(string * int * float * float) list) =
    member this.Program = program
    member this.Variables = variables
    member this.Next = next
    member this.Stack = stack
    member this.For = fors

let comparison op = 
    let fn = match op with | ">" -> (>) | ">=" -> (>=) | "<>" | "><" -> (<>) | "<" -> (<) | "<=" -> (<=) | "=" -> (=) | x -> failwithf "Unexpected operator %A" x
    (fun a b -> if fn a b then -1.0 else 0.0)

let doTab = function
| [Number n] -> String <| System.Text.StringBuilder().Append(' ', int(n)).ToString()
| x -> failwith "Invalid arguments to TAB"

let random = System.Random()

let doRnd = function
| [Number n] -> Number <| random.NextDouble() * n
| x -> failwith "Invalid arguments to RND"

let doMath fn = function
| [Number n] -> Number <| fn n
| x -> failwith "Illegal function call"

let procedures = Map.ofList [("TAB", doTab); ("INT", doMath floor); ("RND", doRnd);
                             ("SIN", doMath sin); ("COS", doMath cos); ("TAN", doMath tan); ("ABS", doMath abs);
                             ("ATN", doMath atan); ("CINT", doMath ceil); ("EXP", doMath exp); ("FIX", doMath (int >> float));
                             ("LOG", doMath log); ("SGN", doMath (sign >> float)); ("SQR", doMath sqrt);]

let rec evalAsNumber (context:Context) x =
    match evalExpression context x with
    | Number a -> a
    | a -> failwithf "Expecting number not %A" a
and evalExpression (context:Context) = function
| Number _ as x -> x
| Variable x -> match Map.tryFind x context.Variables with 
                | Some (ArrayInstance y) -> Number y.[0]
                | Some y -> y 
                | None -> Number 0.0
| String _ as x -> x
| Binary (x, op, y) -> 
    let left = evalAsNumber context x
    let right = evalAsNumber context y
    let fn = match op with | "+" -> (+) | "-" -> (-) | "*" -> (*) | "/" -> (/) | a -> comparison a 
    Number <| fn left right
| Unary ("+", x) -> evalExpression context x
| Unary ("-", x) -> Number <| -evalAsNumber context x
| PrintSeparator _ as x -> x
| Procedure (x, y) ->
    if x.StartsWith("FN") then
        let mapName = "FN " + x.[2..]
        match Map.tryFind mapName context.Variables with
        | Some (Function (syms, e)) -> 
            if List.length syms <> List.length y then failwithf "%s expects %d args, got %d" x (List.length syms) (List.length y)
            let variables = Map.remove mapName context.Variables
            let cf = List.fold2 (fun v s t -> Map.add s (evalExpression context t) v) variables syms y
            evalExpression (Context([], cf, -1, [], [])) e
        | _ -> failwithf "Undefined user function %s" x
    else 
        match Map.tryFind x procedures with
        | Some fn -> fn <| List.map (evalExpression context) y
        | _ -> failwithf "Unknown procedure %s" x
| Array (x, y) ->
    let index = int(evalAsNumber context y)
    match Map.tryFind x context.Variables with
    | Some (ArrayInstance a) -> 
        if index < 1 || index >= a.Length then failwithf "Index out of bounds for %A" x
        Number a.[index]
    | _ -> failwithf "No DIM for %s" x
| x -> failwith "Internal Error"
                       
let listProgram (context:Context) = for (_,_,o) in context.Program do printfn "%s" o

let setVariable (context:Context) name value =
    match Map.tryFind name context.Variables with
    | Some (ArrayInstance a) -> 
        a.[0] <- match value with | Number n -> n | _ -> failwith "Value must be number"
        context
    | _ -> Context(context.Program, Map.add name value context.Variables, context.Next, context.Stack, context.For)

let addArray (context:Context) name size = 
    let (array:float []) = Array.zeroCreate (size + 1)
    Context(context.Program, Map.add name (ArrayInstance array) context.Variables, context.Next, context.Stack, context.For)

let setArrayItem (context:Context) name index value =
    match Map.tryFind name context.Variables with
    | Some (ArrayInstance a) ->
        if index < 1 || index >= a.Length then failwithf "Index out of bounds for %A" a
        a.[index] <- value
    | _ -> failwithf "No DIM for %s" name
    context

let setLine (context:Context) line = Context(context.Program, context.Variables, line, context.Stack, context.For)

let setGosub (context:Context) line = Context(context.Program, context.Variables, line, (context.Next :: context.Stack), context.For)

let gosubReturn (context:Context) =
    match context.Stack with
    | [] -> failwith "RETURN without GOSUB"
    | (head :: tail) -> Context(context.Program, context.Variables, head, tail, context.For)

let addFor (context:Context) line name from limit step =
    Context(context.Program, Map.add name (Number from) context.Variables, context.Next, context.Stack, ((name, line, limit, step) :: context.For))

let processNext (context:Context) name =
    match context.For with
    | [] -> failwith "NEXT without FOR"
    | ((n,l,t,s) :: tail) ->
        if name <> "" && name <> n then failwithf "NEXT %s doesn't match FOR %s" name n
        match Map.tryFind n context.Variables with
        | Some (Number c) ->
            let next = c + s
            if (s >= 0.0 && next > t) || ( s < 0.0 && next < t) then 
                Context(context.Program, Map.add n (Number <| next) context.Variables, context.Next, context.Stack, tail)
            else Context(context.Program, Map.add n (Number <| next) context.Variables, l, context.Stack, context.For)
        | _ -> failwith "Internal Error in NEXT"

let findLine (context:Context) = List.tryFind (fun (l, _, _) -> l >= context.Next) context.Program

let rec runProgram (context:Context) =
    if context.Next < 0 then context
    else
        match findLine context with
        | Some (n, s, _) -> runProgram <| List.fold evalImmediate (setLine context (n + 1)) s 
        | None -> setLine context -1
and evalImmediate context statement = 
    let inner = function
    | Command "CLEAR" -> Context([], Map.empty, 0, [], [])
    | Command "LIST" -> 
        listProgram context
        context
    | Command "RUN" -> runProgram <| setLine context 0
    | Command "END" -> Context(context.Program, context.Variables, -1, [], [])
    | Command "RETURN" -> gosubReturn context
    | Command x -> failwithf "Unknown command %A" x
    | Goto x -> 
        let cp = setLine context <| int(evalAsNumber context x)
        if context.Next < 0 then runProgram cp else cp
    | Gosub x -> 
        let cp = setGosub context <| int(evalAsNumber context x)
        if context.Next < 0 then runProgram cp else cp
    | Let (x,y) -> setVariable context x <| evalExpression context y
    | LetArray (x,y,z) -> setArrayItem context x (int(evalAsNumber context y)) (evalAsNumber context z)
    | Print x ->
        let rec doPrint = function
        | [] -> printfn ""
        | [PrintSeparator ";"] -> ()
        | [PrintSeparator ","] -> printf "\t"
        | (head :: tail) -> 
            match head with
            | Number n -> printf "%O" n
            | String s -> printf "%s" s
            | PrintSeparator "," -> printf "\t"
            | PrintSeparator ";" -> ()
            | _ -> failwithf "Eval error %A" head
            doPrint tail

        doPrint <| List.map (evalExpression context) x
        context
    | If (x, y) ->
        match evalExpression context x with
        | Number 0.0 -> context
        | Number _ -> evalImmediate context y
        | a -> failwithf "Eval error %A" a
    | Input x ->
        let rec doInput context = function
        | [] -> context
        | (head :: tail) ->
            printf "?"
            match System.Double.TryParse(System.Console.ReadLine()) with
            | (true, i) -> 
                match head with
                | (n, Empty) -> doInput (setVariable context n (Number i)) tail
                | (n, a) -> doInput (setArrayItem context n (int(evalAsNumber context a)) i) tail
            | (false, _) -> failwith "Input error"
        doInput context x
    | Rem -> context
    | Dim x ->
        List.fold (fun c (n,e) -> addArray c n <| int(evalAsNumber c e)) context x
    | For (x,y,z,s) ->
        if context.Next < 0 then failwith "FOR only works in a program"
        let from = evalAsNumber context y
        let limit = evalAsNumber context z 
        let step = evalAsNumber context s
        addFor context context.Next x from limit step
    | Next x -> processNext context x
    | DefFn (x,y,z) -> setVariable context ("FN " + x) <| Function (y, z)

    try
        inner statement
    with
    | ex -> 
        if context.Next < 0 then 
            printfn "Error: '%s'" ex.Message
            context
        else 
            printfn "Error: '%s' at line %d" ex.Message (context.Next - 1)
            setLine context -1

let addLine (context:Context) l s o =
    let rec insertLine = function
    | [] -> [(l,s,o)]
    | ((currLine, _, _) as curr :: tail) ->
        if currLine = l then ((l,s,o) :: tail)
        else if currLine < l then (curr :: insertLine tail)
        else ((l,s,o) :: curr :: tail)

    Context(insertLine context.Program, context.Variables, context.Next, context.Stack, context.For)

do
    let context = ref <| Context([], Map.empty, 0, [], [])

    let rec runBasic () =
        printfn "Ok"
        let line = System.Console.ReadLine()
        try
            if line.StartsWith("!") then
                match line.[1..].Split([|' '|], 2) with
                | [|"load"; file|] -> 
                    let lines = Seq.ofArray <| System.IO.File.ReadAllLines(file)
                    let filtered = Seq.filter (fun l -> not (System.String.IsNullOrWhiteSpace(l))) lines
                    let program = Seq.fold (fun c l -> match parse l with | (Line (x, y), _) -> ((x, y, l) :: c) | _ -> failwithf "Error reading line %A" l) [] filtered
                    context := Context(List.rev program, Map.empty, 0, [], [])
                    runBasic ()
                | [|"save"; file|] ->
                    System.IO.File.WriteAllLines(file, List.map (fun (_,_,l) -> l) (!context).Program)
                    runBasic ()
                | [|"vars"|] ->
                    for i in (!context).Variables do
                        printfn "%s = %A" i.Key i.Value
                    runBasic ()
                | [|"quit"|] | [|"exit"|] -> ()
                | _ -> printfn "Did not understand %A" line
                       runBasic ()
            else
                match parse line with
                | (Unnumbered x, _) -> 
                    context := List.fold evalImmediate !context x
                | (Line (x, y), _) ->
                    context := addLine !context x y line
                | (Unmatched, _) ->
                    printfn "Syntax error"
                | x -> printfn "%A" x
                runBasic ()
        with
        | ex -> printfn "%s" ex.Message
                runBasic ()

    printfn "Running Tiny Basic (F# edition)"
    runBasic ()

