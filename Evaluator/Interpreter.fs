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
    type Results = { Text: List<string>; Named: Dictionary<string, Object> }
    
    /// Evaluate results based on instruction sequence and input data
    let public Evaluate (sequenceStream: Stream)
                        (data: Dictionary<string, Object>) : Results =

        try
            let interpreter = Core.SequenceInterpreter()
            let sequence = Loader.Load(sequenceStream) in do
                interpreter.SetSequence sequence
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