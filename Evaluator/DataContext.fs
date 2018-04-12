﻿namespace Evaluator

open System
open System.Collections.Generic
open System.Text
open DataTypes

/// All data context structures
module internal DataContext =

    /// Function address and parameters count
    type internal FunctionInfo = {
        Address : int
        ParamsCount : int
        FrameSize : int
    }

    /// Global/function scope data frame
    type DataFrame(parent : DataFrame option, size: int) =

        /// Data stack
        let stack = new LinkedList<Data>()

        /// Data registers
        let registers: Data array = Array.zeroCreate size

        /// Caller data frame
        let parentFrame = parent

        /// Is Executed as function reference
        let mutable isExecutedAsRef: bool = false
        
        (* ******************** Instance members ******************** *)

        /// Caller frame reference
        member this.Caller with get() = parent

        /// Executed as function reference
        member this.IsReferenced
            with get() = isExecutedAsRef and
                 set(isRef: bool) = isExecutedAsRef <- isRef

        /// Global frame reference
        member this.Global
            with get() =
                match parentFrame with
                | None -> this
                | Some(frame) -> frame.Global

        /// Create frame for calee function
        member this.CreateChildFrame(size) = DataFrame(Some(this), size)

        /// Get register value
        member this.GetRegister (index : int) = registers.[index]

        /// Set register value
        member this.SetRegister (index : int) (value : Data) : unit = 
            registers.[index] <- value

        /// Push value on top of stack
        member this.PushStack (value : Data) = stack.AddFirst(value) |> ignore

        /// Add element to stack bottom
        member this.PushStackBottom (value : Data) = stack.AddLast(value) |> ignore

        /// Pop value from top of stack
        member this.PopStack() =
            if stack.Count > 0
                then
                    let item = stack.First
                    stack.RemoveFirst()
                    item.Value
                else failwith "Stack is empty!"

        /// Is there a result of function?
        member this.HasResult with get() = stack.Count > 0

        /// Dump contents of frame and all parent frames
        member this.Dump(dumpFunc: Data -> string) : string =
            let buffer = new StringBuilder()

            buffer.AppendLine("Stack contents:") |> ignore
            for stackItem in stack do
                buffer.AppendLine(dumpFunc(stackItem)) |> ignore

            buffer.AppendLine("Registers:") |> ignore
            for regData in registers do
                buffer.AppendLine(dumpFunc(regData)) |> ignore

            buffer.ToString()

    /// Data context
    type Context(allFunctions : Dictionary<int, FunctionInfo>) =

        // Native function requested execution of user's function reference
        let nestedExecutionRequested: Event<unit> = Event<unit>()

        // Execution state changed
        let stateChanged: Event<bool> = Event<bool>()

        // External event occured
        let externalEvent: Event<string * Data> = Event<string * Data>()

        /// Defined functions info
        let mutable functions : Dictionary<int, FunctionInfo> = allFunctions

        /// Function return addresses
        let returnAddresses = new Stack<int>()

        /// Current instruction index
        let mutable index: int = functions.[-1].Address

        /// Script is running
        let mutable running: bool = true

        /// Already performed change of instruction pointer
        let mutable jumped : bool = false

        /// Current frame
        let mutable frame: DataFrame = DataFrame(None, functions.[-1].FrameSize)

        /// Event handler frame
        let mutable handlerFrame: DataFrame = Unchecked.defaultof<DataFrame>

        /// Name of current text output context
        let mutable currentTextContextName = "default"

        /// Input data
        let input = new Dictionary<string, Data>()

        /// Results as text
        let textContexts = new Dictionary<string, List<string>>()

        /// Named results
        let namedResults = new Dictionary<string, Object>()

        /// Convert data value to string
        let rec dataToString (data: Data) =
            match data with
            | Number(num) -> num.ToString()
            | Text(str) -> str
            | Boolean(bln) -> bln.ToString()
            | Date(date) -> date.ToString()
            | Empty -> "null"
            | DataArray(arr) ->
                let elemStrings = Seq.map dataToString arr in
                String.Concat('[', String.Join(";", elemStrings), ']')
            | DataHash(hash) ->
                let elemStrings = Seq.map (fun (kv: KeyValuePair<string, Data>) ->
                                    let repr = dataToString(kv.Value)
                                    String.Concat(kv.Key, ':', repr))
                                    hash in
                String.Concat('[', String.Join(";", elemStrings), ']')
            | Iterator info -> String.Concat("<Forward iterator>")
            | FunctionRef (addr, obj) ->
                match obj with
                | Empty -> sprintf "<Function at: %d>" addr
                | _ -> sprintf "<Bound function at: %d>" addr

        do
            textContexts.Add(currentTextContextName, new List<string>())
        
        /// Execution in progress
        member this.Running = running

        /// Current function frame
        member this.Frame with get() = frame and 
                               set(newFrame: DataFrame) = frame <- newFrame

        /// Current instruction index
        member this.Index = index

        /// Input data (obsolete)
        member this.Input = input

        /// Output text
        member this.TextOutput = textContexts.[currentTextContextName]

        /// All text output contexts
        member this.AllTextOutput = textContexts

        /// Output data
        member this.NamedResults = namedResults

        /// Increment instruction pointer
        member this.AdvanceIndex() =
            if not jumped
                then index <- index + 1
                else jumped <- false

        /// Is there a data on top of the stack
        member this.HasDataOnStack = frame.HasResult

        /// Put value to stack
        member this.PushToStack (data: Data) = frame.PushStack(data)

        /// Get value from stack
        member this.PopFromStack() : Data = frame.PopStack()

        /// Pop integer number from stack
        member this.PopNumberFromStack() : float =
            match this.PopFromStack() with
            | Number(num) -> num
            | _ -> failwith "Expected: Number!"

        /// Pop text string from stack
        member this.PopStringFromStack()  : string =
            match this.PopFromStack() with
            | Text(str) -> str
            | _ -> failwith "Expected: Text!"

        /// Pop date value from stack
        member this.PopDateFromStack() : DateTime =
            match this.PopFromStack() with
            | Date(date) -> date
            | _ -> failwith "Expected: Date!"

        /// Pop array from stack
        member this.PopArrayFromStack() : List<Data> =
            match this.PopFromStack() with
            | DataArray(list) -> list
            | _ -> failwith "Expected: Array!"

        /// Pop function address from stack
        member this.PopAddrStack() : int * Data =
            match this.PopFromStack() with
            | FunctionRef (addr, obj) -> (addr, obj)
            | _ -> failwith "Expected: Function!"

        /// Pop value from stack and convert to string
        member this.PopAsResult() = dataToString(this.PopFromStack())

        /// Pop value from stack and coerce to boolean type
        member this.PopAsBoolean() : bool =
            match this.PopFromStack() with
            | Empty -> false
            | Number(num) -> num > 0.0
            | Text(txt) -> txt.Length > 0
            | Boolean(bln) -> bln
            | Date(date) -> date.Year > 1 || date.Month > 1 || date.Day > 1
            | DataArray(arr) -> arr.Count > 0
            | DataHash(hash) -> hash.Count > 0
            | Iterator info -> info.HasNext
            | FunctionRef _ -> true

        /// Pop value as plain .NET object
        member this.PopAsNativeObject() : Object =
            dataToNative(this.PopFromStack())

        /// Duplicate data on top of stack
        member this.DuplicateStackData() =
            let data = this.PopFromStack() in do
            this.PushToStack data
            this.PushToStack data

        /// Set instruction pointer to address
        member this.Jump(address : int) =
            index <- address
            jumped <- true

        /// Create function frame and store return address
        member this.JumpAsFunction(address : int) =
            let funcInfo = functions.[address]
            let numOfVars: int = funcInfo.FrameSize
            let childFrame = frame.CreateChildFrame(numOfVars)
            // Add parameters to function's locals
            for i = funcInfo.ParamsCount - 1 downto 0 do
                let paramValue = frame.PopStack() in
                childFrame.SetRegister i paramValue
            frame <- childFrame
            returnAddresses.Push(index + 1)
            this.Jump(address)

        /// Go to referenced function with bound object as first argument
        member this.JumpAsFunctionRef (address: int) (boundObj: Data) =
            match boundObj with
            | Empty -> ()
            | obj -> frame.PushStackBottom obj
            this.JumpAsFunction address

        /// Go to and execute event handler routine
        member this.ExecuteEventHandler (address : int) (isTerminal : bool) =
            // Don't advance instruction pointer
            index <- index - 1
            this.JumpAsFunction address
            // Store handler frame for return to dispatcher
            if not isTerminal then handlerFrame <- frame
            // Resume execution
            this.Resume()

        /// Go to and execute function reference
        member this.ExecuteFunctionRef (address: int) (boundObj: Data) =
            index <- index - 1
            this.JumpAsFunctionRef address boundObj
            // Not a usual jump
            jumped <- false
            frame.IsReferenced <- true
            nestedExecutionRequested.Trigger()
            running <- true

        /// Return result and control to caller
        member this.ReturnCallerFrame() =
            if frame.HasResult then
                let fnResult = frame.PopStack() in
                frame.Caller.Value.PushStack(fnResult)

            // Terminate nested execution of function reference
            if frame.IsReferenced then running <- false

            // Suspend execution after event handling
            if frame = handlerFrame then this.Suspend()
            frame <- frame.Caller.Value
            this.Jump(returnAddresses.Pop())

        /// Request of execution user's function reference
        member this.NestedExecutionRequested = nestedExecutionRequested.Publish

        /// Execution state changed event
        member this.ExecutionStateChanged = stateChanged.Publish

        /// External event occurence
        member this.ExternalEvent = externalEvent.Publish

        /// Suspend execution
        member this.Suspend() = 
            running <- false
            stateChanged.Trigger false

        /// Resume execution
        member this.Resume() =
            running <- true
            jumped <- false
            stateChanged.Trigger true

        /// External event is occured
        member this.ExternalEventOccured (name : string) (param : Data) =
            externalEvent.Trigger(name, param)

        /// Change text output context
        member this.SetTextOutputContext(name: string) =
            let textCtxName = if name <> null then name else "default" in
            currentTextContextName <- textCtxName
            if not(textContexts.ContainsKey(currentTextContextName)) then
                textContexts.Add(currentTextContextName, new List<string>())

        /// Dump frame contents
        member this.DumpFrame() = frame.Dump(dataToString)