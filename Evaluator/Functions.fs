namespace Evaluator

open System
open System.Collections.Generic
open DataContext

/// Builtin functions
module Functions =

    /// Random value generator
    let private random: Random = new Random()

    /// Invoke named function
    let callFunction (name: string) (ctx: Context) =
        match name with
        // Math
        | "abs" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Abs(number) in
            ctx.PushToStack(Number result)
        | "int" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Floor(number) in
            ctx.PushToStack(Number result)
        | "fract" ->
            let number = ctx.PopNumberFromStack()
            let result = number - Math.Truncate(number) in
            ctx.PushToStack(Number result)
        | "sqrt" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Sqrt(float(number)) in
            ctx.PushToStack(Number result)
        | "pow" ->
            let power = ctx.PopNumberFromStack()
            let number = ctx.PopNumberFromStack()
            let result = Math.Pow(number, power) in
            ctx.PushToStack(Number result)
        | "sin" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Sin(number) in
            ctx.PushToStack(Number result)
        | "cos" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Cos(number) in
            ctx.PushToStack(Number result)
        | "tan" ->
            let number = ctx.PopNumberFromStack()
            let result = Math.Tan(number) in
            ctx.PushToStack(Number result)
        | "rand" ->
            let result = random.NextDouble() in
            ctx.PushToStack(Number result)
        // Type conversion
        | "tonumber" ->
            let strNum = ctx.PopStringFromStack()
            let number = Double.Parse(strNum) in
            ctx.PushToStack(Number number)
        | "todate" ->
            let dateString = ctx.PopStringFromStack()
            let date = DateTime.Parse(dateString) in
            ctx.PushToStack(Date date)
        // Type checking
        | "defined" ->
            let dataToCheck = ctx.PopFromStack() in
            let result = match dataToCheck with
                            | Empty -> false
                            | _ -> true
            ctx.PushToStack(Boolean(result))
        | "type" ->
            let typeName = match ctx.PopFromStack() with
                            | Number _ -> "number"
                            | Text _ -> "string"
                            | Boolean _ -> "bool"
                            | Date _ -> "date"
                            | DataArray _ -> "array"
                            | DataHash _ -> "hash"
                            | Empty -> "null"
                            in ctx.PushToStack(Text typeName)
        // Data access
        | "$" ->
            let dataKey = ctx.PopStringFromStack() in
            ctx.PushToStack (ctx.Input.[dataKey])
        // Date functions
        | "datenow" -> ctx.PushToStack (Date(DateTime.Now))
        | "datediff" -> // !
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
        // Array functions
        | "length" ->
            let compoundElement: Data = ctx.PopFromStack()
            let length: int = match compoundElement with
                                | Text(str) -> str.Length
                                | DataArray(arr) -> arr.Count
                                | DataHash(hash) -> hash.Count
                                | _ -> failwith "Invalid value for Length!"
            ctx.PushToStack (Number(float(length)))
        | "append" ->
            let newElement: Data = ctx.PopFromStack()
            let array = ctx.PopArrayFromStack() in
            array.Add(newElement)
        | "find" ->
            let elemToFind: Data = ctx.PopFromStack()
            let array = ctx.PopArrayFromStack() in
            () // TODO!!!
        | "delete" ->
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
        // Other
        | "format" ->
            let fmtParams: string = ctx.PopAsResult()
            let fmtName: string = ctx.PopStringFromStack()
            let fmtDirective = sprintf "FORMAT: %s %s" fmtName fmtParams
            let fmtString = sprintf "!== %s ==!" fmtDirective in
            ctx.TextOutput.Add(fmtString)
        | _ -> failwithf "Invalid function name: %s!" name


