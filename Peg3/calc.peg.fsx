open System

type ParseResult =
| Result of float 
| Unmatched
| TerminalSymbol of string
| Production of ParseResult list
| EmptyMatch

let unexpected x = failwithf "Parse Error! %A is unexpected here." x

let parseBin _ = function
| Production [x; EmptyMatch] -> x
| Production [Result x; Production y] ->
    let rec processRhs lhs = function
    | [] -> lhs
    | (Production [_; TerminalSymbol "^"; _; Result rhs] :: tail) ->
        lhs ** (processRhs rhs tail) // exponentiation is right associative
    | (Production [_; TerminalSymbol op; _; Result rhs] :: tail) -> 
        let fn = match op with | "*" -> (*) | "/" -> (/) | "+" -> (+) | "-" -> (-) | q -> unexpected q
        processRhs (fn lhs rhs) tail // common arithmetic operators are left associative
    | x -> unexpected x
    Result <| processRhs x y
| x -> unexpected x

(*%%

start   <- expr

number  <- [+-]? {Nd}+ ('.' {Nd}+)? exp?        { (fun s _ -> Result <| Double.Parse(s)) }
exp     <- [Ee] [+-]? {Nd}+
space   <- [ \t]*
atom    <- number / paren
paren   <- '(' space expr space ')'             { (fun _ -> function | Production [_; _; x; _; _] -> x | q -> unexpected q) }
power   <- atom (space '^' space atom)*         { parseBin }
product <- power (space [*/] space power)*      { parseBin }
sum     <- product (space [+-] space product)*  { parseBin }
expr    <- sum

%%*)

let rec runParser () =
    printf ">"
    match Console.ReadLine() with
    | "exit" | "quit" -> ()
    | line ->
        match parse line with
        | (Result x, _) -> printfn "%O" x
        | x -> printfn "%A" x
        runParser ()

runParser ()
