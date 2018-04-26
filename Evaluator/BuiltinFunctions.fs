namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext
open ExtensionTypes

/// Predefined modules
module internal BuiltinFunctions =

    /// Defined modules
    let definedModules: string array = [|
        "$builtin"; "math"; "events"
    |]

    /// Default module
    type BuiltinModule() =
        inherit NativeModule(definedModules.[0])
    
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
            | DataHash hash ->
                match elemToFind with
                | Text key ->
                    let hasElementKey = hash.ContainsKey(key) in
                    ctx.PushToStack(Boolean hasElementKey)
                | _ -> failwith "Expected string key for hash!"
            | _ -> failwithf "Expected: array!"
    
        let fn_format (ctx: Context) =
            let fmtParams: string = ctx.PopAsResult()
            let fmtName: string = ctx.PopStringFromStack()
            let fmtDirective = sprintf "FORMAT: %s %s" fmtName fmtParams
            let fmtString = sprintf "!== %s ==!" fmtDirective in
            ctx.TextOutput.Add(fmtString)

        let fn_setcontext (ctx: Context) =
            let textCtxName = match ctx.PopFromStack() with
                              | Text name -> name
                              | Empty -> null
                              | _ -> failwith "Expected: string!"
            in ctx.SetTextOutputContext(textCtxName)
    
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
            let date: DateTime ref = ref DateTime.MinValue in
            if DateTime.TryParse(dateString, date)
                then ctx.PushToStack(Date date.Value)
                else ctx.PushToStack Empty
    
        let fn_tonumber (ctx: Context) =
            let strNum = ctx.PopStringFromStack()
            let number: Double ref = ref Double.NaN in
            if Double.TryParse(strNum, number)
                then ctx.PushToStack(Number number.Value)
                else ctx.PushToStack Empty
    
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

        let fn_sort_with (ctx: Context) =
            let (funcAddr, boundObj) = ctx.PopAddrStack()
            let array = ctx.PopArrayFromStack() in do
                array.Sort(fun x y ->
                            ctx.PushToStack(x)
                            ctx.PushToStack(y)
                            ctx.ExecuteFunctionRef funcAddr boundObj
                            int(ctx.PopNumberFromStack()))
                ctx.PushToStack(DataArray array)

        let fn_slice (ctx: Context) =
            let length = ctx.PopFromStack()
            let startIdx = int(ctx.PopNumberFromStack())
            match ctx.PopFromStack() with
            | Text str ->
                // Substring
                let subStrLength = match length with
                                   | Number num -> int(num)
                                   | Empty -> str.Length
                                   | _ -> failwith "Expected: number or null!"
                let subStr = str.Substring(startIdx, subStrLength) in
                ctx.PushToStack(Text subStr)
            | DataArray arr ->
                // Array slice
                let sliceLength = match length with
                                  | Number num -> int(num)
                                  | Empty -> arr.Count
                                  | _ -> failwith "Expected: number or null!"
                let sliceArray: Data array = Array.zeroCreate(sliceLength) in
                arr.CopyTo(startIdx, sliceArray, 0, sliceLength)
                ctx.PushToStack(DataArray(new List<Data>(sliceArray)))
            | _ -> failwith "String or array expected!"

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
            ("SortWith", fn_sort_with, 2)
            ("Slice", fn_slice, 3)
            // Iterator functions
            ("_iter_create$", fn_iter_create, 1)
            ("_iter_hasnext$", fn_iter_hasnext, 1)
            ("_iter_next$", fn_iter_next_elem, 1)
            // Other
            ("Format", fn_format, 2)
            ("Context", fn_setcontext, 1)]

    /// Math functions
    type MathModule() =
        inherit NativeModule(definedModules.[1])

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
                ("PI", Number(Math.PI))
                ("E", Number(Math.E))
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

    /// Attached event handlers
    type private EventHanlers = Dictionary<string, int>

    /// Event handling
    type EventsModule() =
        inherit NativeModule(definedModules.[2])

        // Exit loop event name
        let exitEventName = "exit"

        // Event handlers for contexts
        let contextEventHandlers = new Dictionary<Context, EventHanlers>()

        // Contexts with event dispatchers set
        let eventedContexts = new HashSet<Context>()

        // Get event handlers mapping for context
        let getEventHandlers (ctx: Context) : EventHanlers =
            if not(contextEventHandlers.ContainsKey(ctx))
                then contextEventHandlers.Add(ctx, new EventHanlers())
            
            contextEventHandlers.[ctx]

        // Set handler address for specified client
        let setEventHandlerAddr (ctx: Context) (evtName: string) (addr: int) =
            let handlersMapping = getEventHandlers ctx
            if handlersMapping.ContainsKey(evtName)
                then handlersMapping.[evtName] = addr |> ignore
                else handlersMapping.Add(evtName, addr) |> ignore

        /// Set event handler
        let fn_set_handler (ctx: Context) =
            let handlerAddress = fst(ctx.PopAddrStack())
            let eventName = ctx.PopStringFromStack() in
            setEventHandlerAddr ctx eventName handlerAddress

        /// Map events to handlers
        let fn_map_handlers (ctx: Context) =
            match ctx.PopFromStack() with
            | DataHash handlers ->
                for kv in handlers do
                    match kv.Value with
                    | FunctionRef (addr, _) ->
                        let eventName = kv.Key in
                        setEventHandlerAddr ctx eventName addr
                    | _ -> ()
            | _ -> failwith "Hash required!"

        // Suspend execution and wait for events
        let fn_start_loop (ctx: Context) =
            // Dispatch events to handlers
            let eventsDispatcher (data: string * Data) =
                let eventName = fst data
                let isExitEvent = eventName = exitEventName
                let allHandlers = getEventHandlers ctx in

                if allHandlers.ContainsKey(fst data) then
                    // Jump to handler address
                    let handlerAddress = allHandlers.[eventName] in
                    ctx.PushToStack (snd data)
                    ctx.ExecuteEventHandler handlerAddress isExitEvent
                else if isExitEvent then
                    // Continue standard control flow
                    ctx.Resume()
            
            // Attach event dispatcher to context
            if not(eventedContexts.Contains(ctx))
                then
                    ctx.ExternalEvent.Add eventsDispatcher
                    eventedContexts.Add ctx |> ignore
            ctx.Suspend()

        // Stop waiting for events and resume main execution flow
        let fn_exit_loop (ctx: Context) = ()

        /// All constants
        override this.AllConstantsInfo =
            Map.ofList [
                ("Start", Text "start")
                ("End", Text exitEventName)
            ]

        /// All functions
        override this.AllFunctionsInfo = [
            ("SetHandler", fn_set_handler, 2)
            ("MapHandlers", fn_map_handlers, 1)
            ("StartLoop", fn_start_loop, 0)
            ("ExitLoop", fn_exit_loop, 0)]