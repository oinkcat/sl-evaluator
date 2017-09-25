namespace Evaluator

open System

/// Error reporting
module internal Errors =

    /// Exception while loading instructions file
    type LoadException(line: int, msg: string) =
        inherit Exception(msg)

        member this.LineNumber = line

    /// Exception while executing instruction sequence
    type ExecutionException(opCodeInfo: string, msg: string) =
        inherit Exception(msg)

        member this.OpCodeInfo = opCodeInfo
