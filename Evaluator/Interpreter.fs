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

    // Recent instructions cache
    let private instructionsCache = new Dictionary<int, Loader.Sequence>()

    /// Random values table
    let private rndTable: int array =
        let rnd = new System.Random() in
        Array.ofSeq(Seq.map (fun _ -> rnd.Next()) [0..255])

    /// Get hash of stream contents
    let private computeStreamHash(stream: Stream) =
        let contents: byte array = Array.create (int32 stream.Length) (byte 0)
        stream.Read(contents, 0, int32(stream.Length)) |> ignore
        let intitial = int32 rndTable.[0]
        let hash = Array.fold (fun s e -> s ^^^ (rndTable.[int32 e])) intitial contents
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        hash

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

            // Load sequence using cache
            let streamHash = computeStreamHash instructionsStream
            let sequence =
                if instructionsCache.ContainsKey streamHash
                    then instructionsCache.[streamHash]
                    else
                        let scriptContent = Loader.Load(instructionsStream)
                        instructionsCache.Add(streamHash, scriptContent)
                        scriptContent
            in interpreter.SetSequence sequence

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