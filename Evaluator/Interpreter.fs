namespace Evaluator

open System
open System.IO
open System.Collections.Generic

open Errors

/// General evaluator exception
type EvaluatorException(baseException: Exception) =
    inherit Exception(
        match baseException with
        | :? LoadException as le ->
            String.Format("Script load error!\n" +
                          "Line: {0}\n" +
                          "Message: {1}", le.LineNumber, le.Message)
        | :? ExecutionException as ee ->
            String.Format("Script runtime error!\n" +
                          "Instruction index: {0}\n" +
                          "Instruction: {1}\n" +
                          "Message: {2}\n" +
                          "Stack trace: {3}\n" +
                          "Frame dump:\n {4}",
                          ee.OpCodeIndex, ee.OpCodeInfo,
                          ee.Message, ee.Stack, ee.FrameDump)
        | _ -> "Unknown exception!")

/// Interface for results evaluator
module Interpreter =

    /// Evaluation results
    type Results = {
        Text: List<string>;
        Named: Dictionary<string, Object>
    }

    /// Interpreter frontend
    type FrontEnd() =
        
        do Functions.initializeModules()

        // Core reference
        let mutable interpreter: Core.SequenceInterpreter =
            Unchecked.defaultof<Core.SequenceInterpreter>

        /// Script text output
        member this.TextOutput = interpreter.TextResults

        /// Structured data to save
        member this.DataResults = interpreter.NamedResults

        /// Load script instructions
        member this.LoadScript(instructionsStream : Stream) : unit =
            interpreter <- Core.SequenceInterpreter()
            let scriptContent = Loader.Load(instructionsStream) in
                interpreter.SetSequence(scriptContent)

        /// Set all input data (obsolete)
        member this.SetData (data: Dictionary<string, Object>) : unit =
            interpreter.SetData data

        /// Convert and get shared variable value
        member this.GetSharedVar(name: string) : Object =
            interpreter.Shared name

        /// Convert and set shared variable value
        member this.SetSharedVal (name: string) (value: Object) =
            interpreter.Shared(name) <- value

        /// Set callback function
        member this.SetCallback() = ()

        /// Run loaded script
        member this.Run() = interpreter.Interpret()
    
    /// Load script and evaluate data (obsolete)
    let public Evaluate (sequenceStream: Stream)
                        (data: Dictionary<string, Object>) : Results =

        // Load extension modules
        Functions.initializeModules()

        try
            let interpreter = Core.SequenceInterpreter() in do
                interpreter.SetSequence(Loader.Load(sequenceStream))
                interpreter.SetData data
                interpreter.Interpret()
            { 
                Text = interpreter.TextResults
                Named = interpreter.NamedResults
            }
        with
            | :? LoadException as e -> raise (new EvaluatorException(e))
            | :? ExecutionException as e -> raise (new EvaluatorException(e))
            | _ -> reraise()