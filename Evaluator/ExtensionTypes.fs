namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext

/// Types for native extension modules
module internal ExtensionTypes =

    /// Native function type
    type FuncType = (Context -> unit)

    /// Function information
    type FuncInfo = string * FuncType * int

    /// Represent module contents
    type ModuleContents = (string list) * (string list)

    /// Native defined functions and constants
    [<AbstractClass>]
    type NativeModule(name: string) =

        /// Get first element of 3-tuple
        let firstOfThree (first, _, _) = first

        /// Get second element of 3-tuple
        let secondOfThree (_, second, _) = second

        /// All constants
        abstract member AllConstantsInfo: Map<string, Data>

        /// All functions
        abstract member AllFunctionsInfo: FuncInfo list

        /// Name of module
        member this.Name: string = name
            
        // All Constants
        member this.ConstantNames
            with get() = List.map fst (Map.toList this.AllConstantsInfo)

        /// All functions
        member this.FunctionNames
            with get() = List.map (fun (name, _, argc) ->
                                    String.Concat(name, '.', argc))
                                    this.AllFunctionsInfo

        /// Get constant value by it's name
        member this.GetConstantByName(name: string) : Data =
            if this.AllConstantsInfo.ContainsKey(name)
                then this.AllConstantsInfo.[name]
                else failwithf "Invalid constant name: %s!" name

        /// Get function by it's name
        member this.GetFunctionByName(name: string) : FuncType =
            let funcInfo = List.find (fun t -> (firstOfThree t) = name)
                                                this.AllFunctionsInfo in
            secondOfThree funcInfo