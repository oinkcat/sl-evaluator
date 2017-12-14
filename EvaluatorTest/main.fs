namespace Test

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics
open System.Text.RegularExpressions
open Evaluator

/// Interpreter test app
module main =

    /// Command to execute
    type Command =
        | ShowUsage
        | ExecuteScript of string * (string option)
        | WriteModules
        | WriteDefinitions of string

    /// Load data from file
    let loadDataFile (name: string) : Dictionary<string, Object> =
        let dataDict = new Dictionary<string, Object>() in
        let dataLines = Seq.filter (fun (l: string) -> l.Length > 0)
                                   (File.ReadLines(name))
        Seq.iter (fun (line: string) ->
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let (name, typeName) = parts.[0], parts.[1].ToLower()
            let value = String.Join(" ", parts.[2..parts.Length - 1]) in
            let typedValue : Object =
                match typeName with
                | "num" -> upcast(float(value))
                | "string" -> upcast(value)
                | "bool" -> upcast(bool.Parse(value))
                | "date" -> upcast(DateTime.Parse(value))
                | _ -> failwithf "Incorrect type name: %s!" typeName
                in dataDict.Add(name, typedValue)
        ) dataLines

        dataDict

    /// Get command from command line
    let parseCommandLine() : Command =
        try
            let args: string array = Environment.GetCommandLineArgs()
            if args.Length > 1 then
                match args.[1].ToLower() with
                | "-d" -> WriteDefinitions args.[2]
                | "-m" -> WriteModules
                | invalid when invalid.StartsWith("-") ->
                    ShowUsage
                | other ->
                    let scriptFileName = other
                    if args.Length > 2
                        then ExecuteScript (scriptFileName, Some (args.[2]))
                        else ExecuteScript (scriptFileName, None)
            else
                ShowUsage
        with | _ -> ShowUsage
          
    /// Show command usage      
    let showUsage() =
        printf "usage: EvaluatorTest -d <module> | <file> [<data-file>]"

    /// Execute script file
    let executeScript (fileName: string) (dataFileName: string option) =

        Console.WriteLine("Instructions file: {0}", fileName)

        if dataFileName.IsSome then
            Console.WriteLine("Data file: {0}", dataFileName.Value)

        // Interpret instructions and dump results
        try
            use fileStream = File.Open(fileName, FileMode.Open)
            // Load data file
            let data = match dataFileName with
                       | Some(fileName) -> loadDataFile fileName
                       | None -> new Dictionary<string, Object>()

            // Measure time
            let watch = new Stopwatch() in
            watch.Start()

            // Interpreter frontend
            let interpreter = Interpreter.FrontEnd() in do
                interpreter.LoadScript fileStream
                interpreter.SetData data
                interpreter.Run()

            // Output results
            if interpreter.TextOutput.Count > 0 then do
                Console.WriteLine()
                Console.WriteLine("Results:")
                interpreter.TextOutput |> Seq.iter Console.WriteLine
            // Output named results
            if interpreter.DataResults.Count > 0 then do
                Console.WriteLine()
                Console.WriteLine("Named results:")
                interpreter.DataResults
                |> Seq.iter (fun kv -> printf "%s: %A" kv.Key kv.Value)

            // Time elapsed
            watch.Stop()
            let elapsed = watch.Elapsed in do
            Console.WriteLine()
            printf "Execution time: %A" elapsed
        with e -> do
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine(e.Message)
            Console.WriteLine(e.StackTrace)

    /// Write available module names and exit
    let writeModuleNames() =
        printfn "Available modules:"
        List.iter (fun (name: string) -> Console.WriteLine(name))
                  (Evaluator.Interpreter.GetModuleNames())

    /// Write module contents and exit
    let writeModuleContents (moduleName: string) =
        printfn "Module %s contents:" moduleName

        let (constants, functions) =
            Evaluator.Interpreter.GetModuleContents(moduleName) in

        Console.WriteLine("# Constants")
        List.iter (fun (name: string) -> Console.WriteLine(name)) constants

        Console.WriteLine("# Functions")
        List.iter (fun (name: string) -> Console.WriteLine(name)) functions

    // Entry point
    Console.WriteLine("Scripting lang interpreter")
    Console.WriteLine()

    match parseCommandLine() with
        | ShowUsage -> showUsage()
        | ExecuteScript (file, dataFile) -> executeScript file dataFile
        | WriteModules -> writeModuleNames()
        | WriteDefinitions modName -> writeModuleContents modName

    if Debugger.IsAttached then
        Console.WriteLine()
        Console.WriteLine("Press any key to finish debugging...")
        Console.ReadKey() |> ignore
