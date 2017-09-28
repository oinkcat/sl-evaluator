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
            String.Format("Error while loading instructions!\n" +
                          "Line: {0}\n" +
                          "Message: {1}", le.LineNumber, le.Message)
        | :? ExecutionException as ee ->
            String.Format("Error while executing instruction!\n" +
                          "Instruction: {0}\n" +
                          "Message: {1}", ee.OpCodeInfo, ee.Message)
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