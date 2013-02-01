module Program =
    open System
    open System.CodeDom.Compiler
    open System.Diagnostics
    open System.IO
    open System.Text

    open Microsoft.FSharp.Compiler.CodeDom

    open Peg
    open PegOfPeg

    let codeGenGrammar source grammar = 
        let builder = StringBuilder()
        let rules = ref Map.empty

        codeGenHeader builder source
        
        let rec processMethods line = function
        | [] -> ()
        | (head :: tail) ->
            ibprintfn builder 0 "%s" head
            processMethods (line + 1) tail
        
        let rec processRules start line index = function
        | [] -> ()
        | ("%%*)" :: methods) -> 
            ibprintf builder 0 "%s" <| codeGenPostamble start
            ibprintfn builder 0 "#line %d \"%s\"" (line + 2) source
            processMethods (line + 1) methods
        | (rule :: rest) ->
            if String.IsNullOrWhiteSpace(rule) then processRules start (line + 1) index rest
            else
                match applyPegGrammar rule with
                | (Parsed (Rule (n, e, _) as x), _) ->
                    if Map.containsKey n !rules then 
                        eprintfn "Duplicate rule %s at line %d" n line
                        failwith "Invalid rules"
                    rules := Map.add n e !rules
                    ibprintfn builder 4 "// %s" <| pegToString x
                    let isFirst = index = 0
                    codeGenRule builder 4 isFirst x
                    let newStart = if isFirst then n else start
                    processRules newStart (line + 1) (index + 1) rest
                | (Unmatched, x) -> 
                    if x = 0 then 
                        eprintfn "Expression at line %d does not parse!" line
                        eprintfn "--->%s" rule
                    else
                        eprintfn "Expression at line %d does not parse!" line
                        eprintfn "----%s" rule.[0..x-1]
                        eprintfn "%*s--->%s" (x-1) "" rule.[x..]
                    failwith "Cannot parse rule."
                | x -> 
                    eprintfn "Unexpected result %A at line %d" x line
                    failwith "Cannot parse rule."

        let rec processDefinitions line = function
        | [] -> ()
        | ("(*%%" :: rules) ->
            ibprintfn builder 0 "#line 0 \"generated-code\""
            ibprintfn builder 0 "%s" codeGenPreamble
            processRules "<unknown>" (line + 1) 0 rules
        | (defn :: rest) -> 
            ibprintfn builder 0 "%s" defn
            processDefinitions (line + 1) rest

        ibprintfn builder 0 "#line 0 \"%s\"" source
        
        processDefinitions 0 <| Seq.toList grammar

        checkForLeftRecursion !rules
        
        builder.ToString()

    let require m n =
        if not <| Map.containsKey n m then failwithf "Missing required argument -%s" n

    let rec parseArgs m = function
    | [] -> m
    | ((arg:string) :: rest) ->
        if not (arg.StartsWith("-") || arg.StartsWith("/")) then failwithf "Invalid argument: %s" arg
        let (n,v) = match List.ofArray <| arg.[1..].Split(List.toArray [':'], 2) with
                    | [a; b] -> (a,b)
                    | [a] -> (a, "true")
                    | _ -> failwith "Internal Error"
        let mp = match Map.tryFind n m with
                 | Some x -> failwithf "Only one -%s is expected" n
                 | None -> Map.add n v m
        parseArgs mp rest

    let usage = "Usage:
PegParser.exe -i:<inputfile> -o:<outputfile> [-x]

Arguments:
    -i:<inputfile>      The grammar file to import. Required.
    -o:<outputfile>     The F# file to write. Required.
    -x                  Execute the generated file. Optional."

    [<EntryPoint>]
    let Main(args) =
        try
            let parsed = parseArgs Map.empty <| List.ofArray args

            require parsed "i"
            require parsed "o"
            let input = match Map.tryFind "i" parsed with
                        | Some i -> if File.Exists(i) then i else failwithf "File not found: %s" i
                        | None -> failwithf "Internal Error"
            let output = match Map.tryFind "o" parsed with
                         | Some o -> o
                         | None -> failwith "Internal Error"
            let execute = Map.containsKey "x" parsed

            let rec readInput (reader:StreamReader) =
                seq {
                    match reader.ReadLine() with
                    | null -> reader.Close()
                              printfn "Generating..."
                              ()
                    | x -> yield x
                           yield! readInput reader 
                }

            let reader = File.OpenText(input) 
            printfn "Reading..."
            let stopwatch = Stopwatch()
            stopwatch.Start()
            let parser = codeGenGrammar (Path.GetFullPath(input)) (readInput reader) 
            printfn "    generated in %Os" <| float(stopwatch.ElapsedMilliseconds) / 1000.0
            if execute then
                printfn "Compiling..."
                stopwatch.Restart()
                use compiler = new FSharpCodeProvider()
                let parameters = CompilerParameters()
                parameters.GenerateExecutable <- true
                parameters.GenerateInMemory <- true
                let result = compiler.CompileAssemblyFromSource(parameters, parser)
                printfn "    compiled in %Os" <| float(stopwatch.ElapsedMilliseconds) / 1000.0
                if result.Errors.Count > 0 then
                    for error in result.Errors do eprintfn "%O" error done
                else
                    for output in result.Output do printfn "%O" output done
                    printfn "Executing..."
                    try
                        result.CompiledAssembly.EntryPoint.Invoke(null, null) |> ignore
                    with
                    | ex -> eprintfn "Error executing generated code: %s" ex.Message
            let outputWriter = File.CreateText(output)
            outputWriter.Write(parser)
            outputWriter.Flush();
            outputWriter.Close()
            printfn "Done."
            0

        with
        | ex -> eprintfn "Error: %s" ex.Message
                eprintfn "%s" usage
                -1