namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext
open ExtensionTypes

/// Predefined modules
module internal BuiltinFunctions =

    /// Name of default module
    let builtinModuleName = "$builtin"

    /// Default module
    type BuiltinModule() =
        inherit NativeModule(builtinModuleName)

        let fn__data_ (ctx: Context) =
            let dataKey = ctx.PopStringFromStack() in
            ctx.PushToStack (ctx.Input.[dataKey])
    
        let fn_append (ctx: Context) =
            let newElement: Data = ctx.PopFromStack()
            let array = ctx.PopArrayFromStack() in
            array.Add(newElement)
    
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
            match ctx.PopFromStack() with
            | DataArray arr ->
                let found = Seq.tryFind (fun item -> item = elemToFind) arr in
                if found.IsSome
                    then ctx.PushToStack(found.Value)
                    else ctx.PushToStack Empty
            | _ -> failwithf "Expected: array!"
    
        let fn_format (ctx: Context) =
            let fmtParams: string = ctx.PopAsResult()
            let fmtName: string = ctx.PopStringFromStack()
            let fmtDirective = sprintf "FORMAT: %s %s" fmtName fmtParams
            let fmtString = sprintf "!== %s ==!" fmtDirective in
            ctx.TextOutput.Add(fmtString)
    
        let fn_length (ctx: Context) =
            let compoundElement: Data = ctx.PopFromStack()
            let length: int = match compoundElement with
                                | Text(str) -> str.Length
                                | DataArray(arr) -> arr.Count
                                | DataHash(hash) -> hash.Count
                                | _ -> failwith "Invalid value for Length!"
            ctx.PushToStack (Number(float(length)))
    
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
                            | FunctionRef _ -> "function"
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

        let fn_flatten (ctx: Context) =
            let array = ctx.PopArrayFromStack()
            let result = new List<Data>()
            let rec appendElements (array: List<Data>) =
                Seq.iter (fun (elem: Data) ->
                            match elem with
                            | DataArray arr -> appendElements arr
                            | other -> result.Add(other)) array in
            appendElements array
            ctx.PushToStack(DataArray result)

        /// All constants
        override this.AllConstantsInfo =
            Map.ofList [
                ("null", Empty)
                ("true", Boolean(true))
                ("false", Boolean(false))]

        /// All functions
        override this.AllFunctionsInfo = [
            // Type conversion
            ("ToNumber", fn_tonumber, 1)
            ("ToDate", fn_todate, 1)
            // Type checking
            ("Defined", fn_defined, 1)
            ("Type", fn_type, 1)
            // Data access
            ("$", fn__data_, 1)
            // Date functions
            ("DateNow", fn_datenow, 0)
            ("DateDiff", fn_datediff, 3)
            // Array functions
            ("Length", fn_length, 1)
            ("Add", fn_append, 2)
            ("Find", fn_find, 2)
            ("Delete", fn_delete, 2)
            ("RangeArray", fn_rangearray, 2)
            ("Flatten", fn_flatten, 1)
            // Iterator functions
            ("_iter_create$", fn_iter_create, 1)
            ("_iter_hasnext$", fn_iter_hasnext, 1)
            ("_iter_next$", fn_iter_next_elem, 1)
            // Other
            ("Format", fn_format, 2)]

    /// Name of default module
    let mathModuleName = "math"

    /// Math functions
    type MathModule() =
        inherit NativeModule(mathModuleName)

        /// Random value generator
        let random: Random = new Random()

        let fn_abs (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Abs(number) in
            ctx.PushToStack(Number result)
    
        let fn_sin (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Sin(number) in
            ctx.PushToStack(Number result)
    
        let fn_cos (ctx: Context) =            
            let number = ctx.PopNumberFromStack()
            let result = Math.Cos(number) in
            ctx.PushToStack(Number result)
    
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
    
        let fn_sqrt (ctx: Context) =            
            let number = ctx.PopNumberFromStack()
            let result = Math.Sqrt(float(number)) in
            ctx.PushToStack(Number result)
    
        let fn_tan (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Tan(number) in
            ctx.PushToStack(Number result)
    
        let fn_fract (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = number - Math.Truncate(number) in
            ctx.PushToStack(Number result)
    
        let fn_int (ctx: Context) =
            let number = ctx.PopNumberFromStack()
            let result = Math.Floor(number) in
            ctx.PushToStack(Number result)

        /// All constants
        override this.AllConstantsInfo =
            Map.ofList [
                ("pi", Number(Math.PI))
                ("e", Number(Math.E))
            ]

        /// All functions
        override this.AllFunctionsInfo = [
            ("Abs", fn_abs, 1)
            ("Int", fn_int, 1)
            ("Fract", fn_fract, 1)
            ("Sqrt", fn_sqrt, 1)
            ("Pow", fn_pow, 2)
            ("Sin", fn_sin, 1)
            ("Cos", fn_cos, 1)
            ("Tan", fn_tan, 1)
            ("Rand", fn_rand, 1)
            ("Round", fn_round, 2)]