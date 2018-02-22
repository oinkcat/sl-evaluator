namespace Evaluator

open System

/// Error reporting
module internal Errors =

    /// Information about script error
    type RuntimeErrorInfo = {
        Index : int
        OpCodeName : string
        SourceModuleName : string
        SourceLineNumber : int
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

        let modName = info.SourceModuleName
        let lineNo = info.SourceLineNumber

        member this.OpCodeInfo = info.OpCodeName

        member this.OpCodeIndex = info.Index

        member this.Stack = info.Error.StackTrace

        member this.FrameDump = info.Dump

        override this.Message =
            let mappingMessage =
                if lineNo > 0 then
                    sprintf "Source module: %s, line number: %d" modName lineNo
                else
                    "No source mapping information available!"
            in String.Concat(base.Message, Environment.NewLine, mappingMessage)
