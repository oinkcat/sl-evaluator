namespace Evaluator

open System

/// Error reporting
module internal Errors =

    /// Information about script error
    type RuntimeErrorInfo = {
        Index : int
        OpCodeName : string
        Error : Exception
        Dump : string
    }

    /// Exception while loading instructions file
    type LoadException(line: int, msg: string) =
        inherit Exception(msg)

        member this.LineNumber = line

    /// Exception while executing instruction sequence
    type ExecutionException(info: RuntimeErrorInfo) =
        inherit Exception(info.Error.Message)

        member this.OpCodeInfo = info.OpCodeName

        member this.OpCodeIndex = info.Index

        member this.Stack = info.Error.StackTrace

        member this.FrameDump = info.Dump
