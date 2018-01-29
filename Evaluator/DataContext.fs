namespace Evaluator

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
        let stack = new Stack<Data>(10)

        /// Data registers
        let registers: Data array = Array.zeroCreate size

        /// Caller data frame
        let parentFrame = parent
        
        (* ******************** Instance members ******************** *)

        /// Caller frame reference
        member this.Caller with get() = parent

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
        member this.PushStack (value : Data) = stack.Push(value)

        /// Pop value from top of stack
        member this.PopStack() =
            if stack.Count > 0
                then stack.Pop()
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

        /// Input data
        let input = new Dictionary<string, Data>()

        /// Results as text
        let textResults = new List<string>()

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
            | FunctionRef addr -> String.Concat("<Function at: ", addr, ">")
        
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
        member this.TextOutput = textResults

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
        member this.PopAddrStack() : int =
            match this.PopFromStack() with
            | FunctionRef(addr) -> addr
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

        /// Go to event handler routine
        member this.JumpAsEventHandler (address : int) (isTerminal : bool) =
            // Don't advance instruction pointer
            index <- index - 1
            this.JumpAsFunction address
            // Store handler frame for return to dispatcher
            if not isTerminal then handlerFrame <- frame

        /// Return result and control to caller
        member this.ReturnCallerFrame() =
            if frame.HasResult then
                let fnResult = frame.PopStack() in
                frame.Caller.Value.PushStack(fnResult)

            if frame = handlerFrame then this.Suspend()
            frame <- frame.Caller.Value
            this.Jump(returnAddresses.Pop())

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

        /// Dump frame contents
        member this.DumpFrame() = frame.Dump(dataToString)