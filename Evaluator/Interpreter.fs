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

    /// Instructions execution state
    type ExecutionState =
        | NotRunning = 0
        | Running = 1
        | Paused = 2
        | Finished = 3

    /// Interpreter frontend
    type FrontEnd() =
        
        do Modules.initialize()

        let mutable state: ExecutionState = ExecutionState.NotRunning

        // Core reference
        let mutable interpreter: Core.SequenceInterpreter =
            Unchecked.defaultof<Core.SequenceInterpreter>

        /// Processing state
        member this.State = state

        /// Script text output
        member this.TextOutput = interpreter.TextResults

        /// Structured data to save
        member this.DataResults = interpreter.NamedResults

        /// Load script instructions
        member this.LoadScript(instructionsStream : Stream) : unit =
            // Interpreter core and events
            interpreter <- Core.SequenceInterpreter()
            interpreter.Resumed.Add (fun _ -> state <- ExecutionState.Running)
            interpreter.Suspended.Add (fun _ -> state <- ExecutionState.Paused)
            interpreter.Ended.Add (fun _ -> state <- ExecutionState.Finished)

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
        member this.SetCallback (name : string) = ()

        /// Raise custom external event
        member this.RaiseEvent (name : string) (arg : Object) =
            interpreter.RaiseEvent name arg

        /// Run loaded script
        member this.Run() = 
            if state <> ExecutionState.Finished then
                state <- ExecutionState.Running
                interpreter.Run()

    /// Get names of all loaded native modules
    let public GetModuleNames() : string list =
        Modules.initialize()
        Modules.getModuleNames()

    /// Get names of all constants/functions from module with given name
    let public GetModuleContents(name: string) : string list * string list =
        Modules.initialize()
        Modules.getModuleContents(name)
    
    /// Load script and evaluate data (obsolete)
    let public Evaluate (sequenceStream: Stream)
                        (data: Dictionary<string, Object>) : Results =

        // Load extension modules
        Modules.initialize()

        try
            let interpreter = Core.SequenceInterpreter() in do
                interpreter.SetSequence(Loader.Load(sequenceStream))
                interpreter.SetData data
                interpreter.Run()
            { 
                Text = interpreter.TextResults
                Named = interpreter.NamedResults
            }
        with
            | :? LoadException as e -> raise (new EvaluatorException(e))
            | :? ExecutionException as e -> raise (new EvaluatorException(e))
            | _ -> reraise()