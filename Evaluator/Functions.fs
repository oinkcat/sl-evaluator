namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext

/// Builtin functions
module internal Functions =

    /// Native function type
    type FuncType = (Context -> unit)

    /// Random value generator
    let private random: Random = new Random()

    let private fn__data_ (ctx: Context) =
        let dataKey = ctx.PopStringFromStack() in
        ctx.PushToStack (ctx.Input.[dataKey])

    let private fn_abs (ctx: Context) =
        let number = ctx.PopNumberFromStack()
        let result = Math.Abs(number) in
        ctx.PushToStack(Number result)
    
    let private fn_append (ctx: Context) =
        let newElement: Data = ctx.PopFromStack()
        let array = ctx.PopArrayFromStack() in
        array.Add(newElement)
    
    let private fn_cos (ctx: Context) =            
        let number = ctx.PopNumberFromStack()
        let result = Math.Cos(number) in
        ctx.PushToStack(Number result)
    
    let private fn_datediff (ctx: Context) = // !
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
    
    let private fn_datenow (ctx: Context) =
        ctx.PushToStack (Date(DateTime.Now))
    
    let private fn_defined (ctx: Context) =            
        let dataToCheck = ctx.PopFromStack() in
        let result = match dataToCheck with
                        | Empty -> false
                        | _ -> true
        ctx.PushToStack(Boolean(result))
    
    let private fn_delete (ctx: Context) =
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
    
    let private fn_find (ctx: Context) =
        let elemToFind: Data = ctx.PopFromStack()
        let array = ctx.PopArrayFromStack() in
        () // TODO!!!
    
    let private fn_format (ctx: Context) =
        let fmtParams: string = ctx.PopAsResult()
        let fmtName: string = ctx.PopStringFromStack()
        let fmtDirective = sprintf "FORMAT: %s %s" fmtName fmtParams
        let fmtString = sprintf "!== %s ==!" fmtDirective in
        ctx.TextOutput.Add(fmtString)
    
    let private fn_fract (ctx: Context) =
        let number = ctx.PopNumberFromStack()
        let result = number - Math.Truncate(number) in
        ctx.PushToStack(Number result)
    
    let private fn_int (ctx: Context) =
        let number = ctx.PopNumberFromStack()
        let result = Math.Floor(number) in
        ctx.PushToStack(Number result)
    
    let private fn_length (ctx: Context) =
        let compoundElement: Data = ctx.PopFromStack()
        let length: int = match compoundElement with
                            | Text(str) -> str.Length
                            | DataArray(arr) -> arr.Count
                            | DataHash(hash) -> hash.Count
                            | _ -> failwith "Invalid value for Length!"
        ctx.PushToStack (Number(float(length)))
    
    let private fn_pow (ctx: Context) =            
        let power = ctx.PopNumberFromStack()
        let number = ctx.PopNumberFromStack()
        let result = Math.Pow(number, power) in
        ctx.PushToStack(Number result)
    
    let private fn_rand (ctx: Context) =
        let result = random.NextDouble() in
        ctx.PushToStack(Number result)

    let private fn_round (ctx : Context) =
        let numDecimals = ctx.PopNumberFromStack()
        let number = ctx.PopNumberFromStack()
        let result = Math.Round(number, int(numDecimals)) in
        ctx.PushToStack(Number result)
    
    let private fn_sin (ctx: Context) =
        let number = ctx.PopNumberFromStack()
        let result = Math.Sin(number) in
        ctx.PushToStack(Number result)
    
    let private fn_sqrt (ctx: Context) =            
        let number = ctx.PopNumberFromStack()
        let result = Math.Sqrt(float(number)) in
        ctx.PushToStack(Number result)
    
    let private fn_tan (ctx: Context) =
        let number = ctx.PopNumberFromStack()
        let result = Math.Tan(number) in
        ctx.PushToStack(Number result)
    
    let private fn_todate (ctx: Context) =
        let dateString = ctx.PopStringFromStack()
        let date = DateTime.Parse(dateString) in
        ctx.PushToStack(Date date)
    
    let private fn_tonumber (ctx: Context) =
        let strNum = ctx.PopStringFromStack()
        let number = Double.Parse(strNum) in
        ctx.PushToStack(Number number)
    
    let private fn_type (ctx: Context) =
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

    let private fn_iter_create (ctx: Context) =
        let target = ctx.PopFromStack()
        let iterInstance = IteratorInfo(target) in
        ctx.PushToStack(Iterator iterInstance)

    let private fn_iter_hasnext (ctx: Context) =
        match ctx.PopFromStack() with
        | Iterator info -> ctx.PushToStack(Boolean(info.HasNext))
        | _ -> failwith "Expected: iterator!"

    let private fn_iter_next_elem (ctx: Context) =
        match ctx.PopFromStack() with
        | Iterator info -> ctx.PushToStack(info.Iterate())
        | _ -> failwith "Expected: iterator!"

    let private fn_rangearray (ctx: Context) =
        let toNumber = ctx.PopNumberFromStack()
        let fromNumber = ctx.PopNumberFromStack()
        let step = if fromNumber <= toNumber then 1.0 else -1.0
        let items = Seq.map (fun n -> Number n)
                            [fromNumber .. step .. toNumber]
        let array = new List<Data>(items) in
        ctx.PushToStack(DataArray array)

    let private allFunctionInfo =  [ 
        // Math
        ("abs", fn_abs)
        ("int", fn_int)
        ("fract", fn_fract)
        ("sqrt", fn_sqrt)
        ("pow", fn_pow)
        ("sin", fn_sin)
        ("cos", fn_cos)
        ("tan", fn_tan)
        ("rand", fn_rand)
        ("round", fn_round)
        // Type conversion
        ("tonumber", fn_tonumber)
        ("todate", fn_todate)
        // Type checking
        ("defined", fn_defined)
        ("type", fn_type)
        // Data access
        ("$", fn__data_)
        // Date functions
        ("datenow", fn_datenow)
        ("datediff", fn_datediff)
        // Array functions
        ("length", fn_length)
        ("add", fn_append)
        ("find", fn_find)
        ("delete", fn_delete)
        ("rangearray", fn_rangearray)
        // Iterator functions
        ("_iter_create$", fn_iter_create)
        ("_iter_hasnext$", fn_iter_hasnext)
        ("_iter_next$", fn_iter_next_elem)
        // Other
        ("format", fn_format)]

    let private allFunctions: (Context -> unit) list =
        List.map snd allFunctionInfo

    let resolveFunctionName (name: string) : FuncType =
        let funcInfo = List.find (fun t -> (fst t) = name) allFunctionInfo in
        snd funcInfo