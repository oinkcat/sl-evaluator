namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext

/// Builtin functions
module internal Functions =

    /// Native function type
    type FuncType = (Context -> unit)

    /// Function information
    type FuncInfo = string * FuncType * int

    /// Represent module contents
    type ModuleContents = (string list) * (string list)

    /// Native defined functions and constants
    type INativeModule =

        /// Name of module
        abstract member Name: string

        /// Names of all constants
        abstract member ConstantNames: string list

        /// Names of all functions
        abstract member FunctionNames: string list

        /// Get constant value by it's name
        abstract member GetConstantByName: string -> Data
        
        /// Get function by it's name
        abstract member GetFunctionByName: string -> FuncType

    let builtinModuleName = "$builtin"

    /// Builtin functions
    type BuiltinModule() =

        /// Random value generator
        let random: Random = new Random()

        let fn__data_ (ctx: Context) =
            let dataKey = ctx.PopStringFromStack() in
            ctx.PushToStack (ctx.Input.[dataKey])

        let fn_abs (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Abs(number) in
            ctx.PushToStack(Number result)
    
        let fn_append (ctx: Context) =
            let newElement: Data = ctx.PopFromStack()
            let array = ctx.PopArrayFromStack() in
            array.Add(newElement)
    
        let fn_cos (ctx: Context) =            
            let number = ctx.PopNumberFromStack()
            let result = Math.Cos(number) in
            ctx.PushToStack(Number result)
    
        let fn_datediff (ctx: Context) = // !
            let diffWith : DateTime = ctx.PopDateFromStack()
            let diffWhat : DateTime = ctx.PopDateFromStack()
            let unit: string = ctx.PopStringFromStack()
            let diffSpan: TimeSpan = diffWhat.Subtract(diffWith)
            let result = match unit with
                            | "y" -> diffSpan.TotalDays / 365.0
                            | "m" -> diffSpan.TotalDays / 30.0
                            | "d" -> diffSpan.TotalDays
                            | _ -> failwith "Invalid DateDiff unit!"
            ctx.PushToStack (Number(result))
    
        let fn_datenow (ctx: Context) =
            ctx.PushToStack (Date(DateTime.Now))
    
        let fn_defined (ctx: Context) =            
            let dataToCheck = ctx.PopFromStack() in
            let result = match dataToCheck with
                            | Empty -> false
                            | _ -> true
            ctx.PushToStack(Boolean(result))
    
        let fn_delete (ctx: Context) =
            let elemKey: Data = ctx.PopFromStack()
            let array: Data = ctx.PopFromStack() in
            match array with
            | DataArray(arr) ->
                match elemKey with
                | Number(num) -> arr.RemoveAt(int(num))
                | _ -> failwith "Expected numeric index for array!"
            | DataHash(hash) ->
                match elemKey with
                | Text(key) -> hash.Remove(key) |> ignore
                | _ -> failwith "Expected string key for hash!"
            | _ -> failwith "Expected array or hash!"
    
        let fn_find (ctx: Context) =
            let elemToFind: Data = ctx.PopFromStack()
            let array = ctx.PopArrayFromStack() in
            () // TODO!!!
    
        let fn_format (ctx: Context) =
            let fmtParams: string = ctx.PopAsResult()
            let fmtName: string = ctx.PopStringFromStack()
            let fmtDirective = sprintf "FORMAT: %s %s" fmtName fmtParams
            let fmtString = sprintf "!== %s ==!" fmtDirective in
            ctx.TextOutput.Add(fmtString)
    
        let fn_fract (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = number - Math.Truncate(number) in
            ctx.PushToStack(Number result)
    
        let fn_int (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Floor(number) in
            ctx.PushToStack(Number result)
    
        let fn_length (ctx: Context) =
            let compoundElement: Data = ctx.PopFromStack()
            let length: int = match compoundElement with
                                | Text(str) -> str.Length
                                | DataArray(arr) -> arr.Count
                                | DataHash(hash) -> hash.Count
                                | _ -> failwith "Invalid value for Length!"
            ctx.PushToStack (Number(float(length)))
    
        let fn_pow (ctx: Context) =            
            let power = ctx.PopNumberFromStack()
            let number = ctx.PopNumberFromStack()
            let result = Math.Pow(number, power) in
            ctx.PushToStack(Number result)
    
        let fn_rand (ctx: Context) =
            let result = random.NextDouble() in
            ctx.PushToStack(Number result)

        let fn_round (ctx : Context) =
            let numDecimals = ctx.PopNumberFromStack()
            let number = ctx.PopNumberFromStack()
            let result = Math.Round(number, int(numDecimals)) in
            ctx.PushToStack(Number result)
    
        let fn_sin (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Sin(number) in
            ctx.PushToStack(Number result)
    
        let fn_sqrt (ctx: Context) =            
            let number = ctx.PopNumberFromStack()
            let result = Math.Sqrt(float(number)) in
            ctx.PushToStack(Number result)
    
        let fn_tan (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Tan(number) in
            ctx.PushToStack(Number result)
    
        let fn_todate (ctx: Context) =
            let dateString = ctx.PopStringFromStack()
            let date = DateTime.Parse(dateString) in
            ctx.PushToStack(Date date)
    
        let fn_tonumber (ctx: Context) =
            let strNum = ctx.PopStringFromStack()
            let number = Double.Parse(strNum) in
            ctx.PushToStack(Number number)
    
        let fn_type (ctx: Context) =
            let typeName = match ctx.PopFromStack() with
                            | Number _ -> "number"
                            | Text _ -> "string"
                            | Boolean _ -> "bool"
                            | Date _ -> "date"
                            | DataArray _ -> "array"
                            | DataHash _ -> "hash"
                            | Iterator _ -> "iterator"
                            | Empty -> "null"
                            in ctx.PushToStack(Text typeName)

        let fn_iter_create (ctx: Context) =
            let target = ctx.PopFromStack()
            let iterInstance = IteratorInfo(target) in
            ctx.PushToStack(Iterator iterInstance)

        let fn_iter_hasnext (ctx: Context) =
            match ctx.PopFromStack() with
            | Iterator info -> ctx.PushToStack(Boolean(info.HasNext))
            | _ -> failwith "Expected: iterator!"

        let fn_iter_next_elem (ctx: Context) =
            match ctx.PopFromStack() with
            | Iterator info -> ctx.PushToStack(info.Iterate())
            | _ -> failwith "Expected: iterator!"

        let fn_rangearray (ctx: Context) =
            let toNumber = ctx.PopNumberFromStack()
            let fromNumber = ctx.PopNumberFromStack()
            let step = if fromNumber <= toNumber then 1.0 else -1.0
            let items = Seq.map (fun n -> Number n)
                                [fromNumber .. step .. toNumber]
            let array = new List<Data>(items) in
            ctx.PushToStack(DataArray array)

        let allConstantsInfo : Map<string, Data> =
            Map.ofList [
                ("null", Empty)
                ("true", Boolean(true))
                ("false", Boolean(false))
                ("pi", Number(Math.PI))
                ("e", Number(Math.E))]

        let allFunctionsInfo : FuncInfo list =  [ 
            // Math
            ("abs", fn_abs, 1)
            ("int", fn_int, 1)
            ("fract", fn_fract, 1)
            ("sqrt", fn_sqrt, 1)
            ("pow", fn_pow, 2)
            ("sin", fn_sin, 1)
            ("cos", fn_cos, 1)
            ("tan", fn_tan, 1)
            ("rand", fn_rand, 1)
            ("round", fn_round, 2)
            // Type conversion
            ("tonumber", fn_tonumber, 1)
            ("todate", fn_todate, 1)
            // Type checking
            ("defined", fn_defined, 1)
            ("type", fn_type, 1)
            // Data access
            ("$", fn__data_, 1)
            // Date functions
            ("datenow", fn_datenow, 0)
            ("datediff", fn_datediff, 3)
            // Array functions
            ("length", fn_length, 1)
            ("add", fn_append, 2)
            ("find", fn_find, 2)
            ("delete", fn_delete, 2)
            ("rangearray", fn_rangearray, 2)
            // Iterator functions
            ("_iter_create$", fn_iter_create, 1)
            ("_iter_hasnext$", fn_iter_hasnext, 1)
            ("_iter_next$", fn_iter_next_elem, 1)
            // Other
            ("format", fn_format, 2)]

        /// Get first element of 3-tuple
        let firstOfThree (first, _, _) = first

        /// Get second element of 3-tuple
        let secondOfThree (_, second, _) = second

        let allFunctions: (Context -> unit) list =
            List.map secondOfThree allFunctionsInfo

        interface INativeModule with

            /// Module name
            member this.Name: string = builtinModuleName

            /// All constants
            member this.ConstantNames
                with get() = List.map fst (Map.toList allConstantsInfo)

            /// All functions
            member this.FunctionNames
                with get() = List.map (fun (name, _, argc) ->
                                        String.Concat(name, '.', argc))
                                      allFunctionsInfo

            /// Get constant value by it's name
            member this.GetConstantByName(name: string) : Data =
                if allConstantsInfo.ContainsKey(name)
                    then allConstantsInfo.[name]
                    else failwithf "Invalid constant name: %s!" name

            /// Get function by it's name
            member this.GetFunctionByName(name: string) : FuncType =
                let funcInfo = List.find (fun t -> (firstOfThree t) = name)
                                                   allFunctionsInfo in
                secondOfThree funcInfo

    /// All available modules
    let private loadedModules = new Dictionary<string, INativeModule>()

    /// Get native module by it's name
    let private getModuleByName (name: string) : INativeModule =
        let nameOfModule = if String.IsNullOrEmpty(name)
                            then builtinModuleName
                            else name
        in loadedModules.[nameOfModule]

    /// Load native modules
    let initializeModules() =
        loadedModules.Add(builtinModuleName, new BuiltinModule())

    /// Get constant value by it's name
    let resolveConstant (moduleName: string) (name: string) : Data =
        getModuleByName(moduleName).GetConstantByName(name)

    /// Get function by it's name
    let resolveFunction (moduleName: string) (name: string) : FuncType =
        getModuleByName(moduleName).GetFunctionByName(name)

    /// Get names of all modules
    let getModuleNames() : string list = List.ofSeq loadedModules.Keys

    /// Get module contents
    let getModuleContents(moduleName: string) : ModuleContents =
        let nativeModule = getModuleByName moduleName in
        (nativeModule.ConstantNames, nativeModule.FunctionNames)