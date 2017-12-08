namespace Test

open System
open System.IO
open System.Collections.Generic
open Evaluator

/// Interpreter test app
module main =

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

    // Entry point
    Console.WriteLine("Interpreter test")
    Console.WriteLine()

    let args: string array = Environment.GetCommandLineArgs()

    if args.Length > 1 then do
        let instructionsFile = args.[1].Trim('"')

        Console.WriteLine("Instructions file: {0}", instructionsFile)

        let dataFile = if args.Length > 2 then Some(args.[2].Trim('"'))
                                          else None
        if dataFile.IsSome then
            Console.WriteLine("Data file: {0}", dataFile)

        // Interpret instructions and dump results
        try
            use fileStream = File.Open(instructionsFile, FileMode.Open)
            // Load data file
            let data = match dataFile with
                       | Some(fileName) -> loadDataFile fileName
                       | None -> new Dictionary<string, Object>()

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
        with e -> do
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine(e.Message)
            Console.WriteLine(e.StackTrace)
    else
        Console.Error.WriteLine("usage: EvaluatorTest <file> [<data-file>]")

    Console.ReadKey() |> ignore