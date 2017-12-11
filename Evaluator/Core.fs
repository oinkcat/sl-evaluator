namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext
open Functions

/// Main functionality
module internal Core =

    /// Binary math operation
    type MathOp = Add | Sub | Mul | Div | Mod

    /// Binary comparsion operation
    type CmpOp = | Equal | NotEqual
                 | Less | Greater
                 | LessOrEqual | GreaterOrEqual

    /// String operation
    type StringOp = Concat | Format

    /// Logic operation
    type LogicOp = Or | And | Xor | Not

    /// Jump condition types
    type JumpCondition = | IfEqual | IfNotEqual 
                         | IfLess | IfGreater
                         | IfLessOrEqual | IfGreatOrEqual

    /// Data comparsion result
    type ComparsionResult = IsLess | IsEqual | IsGreater | Undefined

    /// Function type and access information
    type FunctionDisposition = Extern of FuncType | Defined of int

    /// Instruction opcode
    type OpCode =
        | LoadStr of string
        | LoadNum of float
        | LoadReg of int
        | LoadData of string
        | LoadRegGlobal of int
        | LoadConst of string
        | LoadDataArray of int
        | Duplicate
        | Unload
        | Store of int
        | StoreGlobal of int
        | Reset of int
        | MakeArray of int
        | MakeHash of int
        | ArrayGet
        | ArraySet
        | ArraySetMath of MathOp
        | Math of MathOp
        | String of StringOp
        | Compare of CmpOp
        | Logic of LogicOp
        | Jump of int
        | CondJump of JumpCondition * int
        | Call of FunctionDisposition
        | Invoke
        | Ret
        | Emit
        | EmitNamed of string

    /// Function address and parameters count
    type FunctionInfo = {
        Address : int;
        ParamsCount : int
    }

    /// Instructions sequence
    type Sequence = {
        SharedVarNames : List<string>
        Data : List<Data>
        Functions : List<FunctionInfo>
        FrameSizes : Dictionary<int, int>
        Instructions : List<OpCode>
        EntryPoint : int
    }

    /// Instruction sequence interpreter
    type SequenceInterpreter() =

        /// Last comparsion result
        let mutable cmpResult : ComparsionResult = Undefined

        /// Shared variable names
        let mutable sharedVarNames : List<string> = null

        /// Constant data arrays
        let mutable constData : List<Data> = null

        /// Defined functions info
        let mutable functions : List<FunctionInfo> = null

        /// Frame sizes of each function
        let mutable functionFrameSizes: Dictionary<int, int> = null

        /// Function return addresses
        let returnAddresses = new Stack<int>()

        /// Data context
        let mutable context: Context = Unchecked.defaultof<Context>

        /// Sequence to interpret (module ?)
        let mutable programModule: Sequence option = None

        /// Current instruction index
        let mutable index: int = 0

        /// Set comparsion result
        let setCmpResult (result : int) :unit =
            cmpResult <- match result with
                         | -1 -> IsLess
                         | 0 -> IsEqual
                         | 1 -> IsGreater
                         | _ -> failwith "Impossible!"

        /// Compare two data items
        let compareDataItems (item1: Data) (item2: Data) : unit =
            // TODO: other comparsions
            let result: int =
                match item1 with
                | Number(num1) ->
                    match item2 with
                    | Number(num2) -> num1.CompareTo(num2)
                    | _ -> 0
                | Text(txt1) ->
                    match item2 with
                    | Text(txt2) -> txt1.CompareTo(txt2)
                    | _ -> 0
                | Boolean(bln1) ->
                    match item2 with
                    | Boolean(bln2) -> bln1.CompareTo(bln2)
                    | _ -> 0
                | Date(date1) ->
                    match item2 with
                    | Date(date2) -> date1.CompareTo(date2)
                    | _ -> 0
                | _ -> 0
            in setCmpResult result

        /// Check if we can perform conditional jump
        let checkCanJump (condition: JumpCondition) : bool =
            let (op2, op1) = (context.PopFromStack(), context.PopFromStack()) in
            compareDataItems op1 op2
            match condition with
            | IfEqual -> cmpResult = IsEqual
            | IfNotEqual -> cmpResult <> IsEqual
            | IfLess -> cmpResult = IsLess
            | IfGreater -> cmpResult = IsGreater
            | IfLessOrEqual -> cmpResult = IsLess || cmpResult = IsEqual
            | IfGreatOrEqual -> cmpResult = IsGreater || cmpResult = IsEqual

        /// Binary operations on numbers
        let performMath (operation: MathOp) : unit =
            let second = context.PopNumberFromStack()
            let first = context.PopNumberFromStack()
            let result = match operation with
                         | Add -> first + second
                         | Sub -> first - second
                         | Mul -> first * second
                         | Div -> first / second 
                         | Mod -> first % second in
            context.PushToStack(Number(result))

        /// Binary operations on strings
        let performString (operation: StringOp) : unit =
            let second: string = context.PopAsResult()
            let first: string = context.PopAsResult()
            let result = match operation with
                         | Concat -> String.Concat(first, second)
                         | _ -> failwith "Unsupported operation!"
            context.PushToStack(Text(result))

        /// Binary comparsion operations
        let performCompare (operation: CmpOp) : unit =
            let second = context.PopFromStack()
            let first = context.PopFromStack() in
            compareDataItems first second
            let boolResult: bool =
                match operation with
                | Equal -> cmpResult = IsEqual
                | NotEqual -> cmpResult <> IsEqual
                | Less -> cmpResult = IsEqual
                | Greater -> cmpResult = IsGreater
                | LessOrEqual -> cmpResult = IsLess || cmpResult = IsEqual
                | GreaterOrEqual -> cmpResult = IsGreater || cmpResult = IsEqual
            in context.PushToStack(Boolean(boolResult))

        /// Logic operations
        let performLogic (operation: LogicOp) : unit =
            let second: bool = context.PopAsBoolean()
            let result: bool =
                match operation with
                | Not -> not second
                | binary ->
                    let first: bool = context.PopAsBoolean() in
                    match binary with
                    | Or -> first || second
                    | And -> first && second
                    | Xor -> (first && not second) || (not first && second)
                    | _ -> failwith "Incorrect logic operation!"
            context.PushToStack(Boolean(result))

        /// Make array and move elements from stack to it
        let makeElemsArray (numElems: int) =
            let elemsList = new List<Data>() in
            List.iter (fun _ -> let elem = context.PopFromStack() in
                                elemsList.Insert(0, elem)) 
                      [1..numElems]
            context.PushToStack (Data.DataArray(elemsList))

        /// Make hash and move elements from stack to it
        let makeElemsHash (numElems: int) =
            let elemsHash = new Dictionary<string, Data>() in
            List.iter (fun _ -> let value = context.PopFromStack()
                                let key = context.PopStringFromStack() in
                                elemsHash.Add(key, value))
                      [1..numElems]
            context.PushToStack (Data.DataHash(elemsHash))

        /// Return array/hash element
        let getArrayElem() : Data =
            let index: Data = context.PopFromStack()
            let arrayItem: Data = context.PopFromStack() in
            match arrayItem with
            | DataArray(arr) ->
                match index with
                | Number(idx) -> arr.[int(idx)]
                | _ -> failwith "Expected numeric index for array!"
            | DataHash(hash) ->
                match index with
                | Text(key) -> if hash.ContainsKey(key) 
                                then hash.[key]
                                else Empty
                | _ -> failwith "Expected string index for hash!"
            | _ -> failwith "Array required!" 

        /// Set array/hash element
        let setArrayElem(): unit =
            let index: Data = context.PopFromStack()
            let arrayItem: Data = context.PopFromStack()
            let elem: Data = context.PopFromStack() in
            match arrayItem with
            | DataArray(arr) ->
                match index with
                | Number(num) -> arr.[int(num)] <- elem
                | _ -> failwith "Expected numeric index for array!"
            | DataHash(hash) ->
                match index with
                | Text(key) -> hash.[key] <- elem
                | _ -> failwith "Expected string index for hash!"
            | _ -> failwith "Array required!"

        /// Set array/hash element with performing math operation
        let setArrayElemWithMath (op: MathOp) : unit =
            let index: Data = context.PopFromStack()
            let arrayItem: Data = context.PopFromStack()
            let value: Data = context.PopFromStack() in do
                context.PushToStack(arrayItem)
                context.PushToStack(index)
                // origArrayElem = arrayItem[index]
                let origArrayElem = getArrayElem() in do
                    context.PushToStack(origArrayElem)
                    context.PushToStack(value)
                    // _result = origArrayItem <op> value
                    performMath(op)
                    context.PushToStack(arrayItem)
                    context.PushToStack(index)
                    // arrayItem[index] = _result
                    setArrayElem()

        /// Get constant value
        let getConstant (name: string) : Data =
            match name with
            | "null" -> Empty
            | "true" -> Boolean(true)
            | "false" -> Boolean(false)
            | "pi" -> Number(Math.PI)
            | "e" -> Number(Math.E)
            | _ -> failwithf "Invalid constant name: %s!" name

        /// Create function frame and store return address
        let jumpAsFunction(address : int) (currentAddress : int) =
            let numOfVars: int = functionFrameSizes.[address]
            let childFrame = context.Frame.CreateChildFrame(numOfVars)
            // Add parameters to function's locals
            let funcInfo = Seq.find (fun f -> f.Address = address) functions
            for i = funcInfo.ParamsCount - 1 downto 0 do
                let paramValue = context.Frame.PopStack() in
                childFrame.SetRegister i paramValue
            context.Frame <- childFrame
            returnAddresses.Push(currentAddress)
            address

        /// Return result and control to caller
        let returnCallerFrame() =
            if context.Frame.HasResult() then
                let fnResult = context.Frame.PopStack() in
                context.Frame.Caller.Value.PushStack(fnResult)
            context.Frame <- context.Frame.Caller.Value
            returnAddresses.Pop()

        /// Get shared variable register index
        /// or fail if no such variable exists
        let getSharedVarIndexCheched (name: string) : int =
            let regIdx = sharedVarNames.IndexOf(name) in
            if regIdx > -1
                then regIdx
                else failwithf "Shared variable %s not found!" name

        (* ******************** Instance members ******************** *)

        /// Shared variables
        member this.Shared
            with get (name: string) : Object =
                let regIdx = getSharedVarIndexCheched name in
                dataToNative(context.Frame.Global.GetRegister(regIdx))
            and set (name: string) (value: Object) =
                let regIdx = getSharedVarIndexCheched name in
                context.Frame.Global.SetRegister regIdx (nativeToData value)

        /// Result strings
        member this.TextResults with get() = context.TextOutput

        /// Named results
        member this.NamedResults with get() = context.NamedResults

        /// Execute sequence of instruction opcodes
        member this.ExecuteSequence (program : Sequence) =
            index <- program.EntryPoint

            let seqLength = program.Instructions.Count in

            while index < seqLength do
                let mutable nextIndex = index + 1
                let instruction = program.Instructions.[index] in

                match instruction with
                // Data handling
                | LoadStr str -> context.PushToStack(Text(str))
                | LoadNum num -> context.PushToStack(Number(num))
                | LoadReg regIndex ->
                    let registerValue = context.Frame.GetRegister(regIndex) in
                        context.PushToStack registerValue
                | LoadRegGlobal regIndex ->
                    let globalGet = context.Frame.Global.GetRegister in
                        context.PushToStack (globalGet(regIndex))
                | LoadData key -> context.PushToStack context.Input.[key]
                | LoadDataArray idx -> context.PushToStack constData.[idx]
                | LoadConst name -> context.PushToStack (getConstant name)
                | Duplicate -> context.DuplicateStackData()
                | Unload -> context.PopFromStack() |> ignore
                | Store regIndex ->
                    let dataToStore = context.PopFromStack() in
                        context.Frame.SetRegister regIndex dataToStore
                | StoreGlobal regIndex ->
                    let globalSet = context.Frame.Global.SetRegister in
                        globalSet regIndex (context.PopFromStack())
                | Reset regIndex -> context.Frame.SetRegister regIndex Empty

                // Arrays/hashes
                | MakeArray elemCount -> makeElemsArray elemCount
                | MakeHash elemCount -> makeElemsHash elemCount
                | ArrayGet -> let elem = getArrayElem() in context.PushToStack elem
                | ArraySet -> setArrayElem()
                | ArraySetMath op -> setArrayElemWithMath op

                // Operators
                | Math op -> performMath op
                | String op -> performString op
                | Compare op -> performCompare op
                | Logic op -> performLogic op

                // Flow control
                | Jump newIndex -> nextIndex <- newIndex
                | CondJump (jumpType, newIndex) -> if checkCanJump jumpType
                                                    then nextIndex <- newIndex
                | Call disp -> 
                    match disp with
                    | Extern func -> func context
                    | Defined fnIndex -> nextIndex <- (jumpAsFunction fnIndex nextIndex)
                | Invoke -> () // TODO: Dynamic invoke
                | Ret -> nextIndex <- returnCallerFrame()

                // Data output
                | Emit -> context.TextOutput.Add(context.PopAsResult())
                | EmitNamed key ->
                    let value: Object = context.PopAsNativeObject() in
                    context.NamedResults.Add(key, value)

                index <- nextIndex

        /// Set input data
        member this.SetData (data: Dictionary<string, Object>) : unit =
            Seq.iter (fun key ->
                let value = data.[key]
                let dataValue = nativeToData value
                in context.Input.Add(key, dataValue))
                data.Keys

        /// Set instructions sequence to interpret
        member this.SetSequence (program : Sequence) : unit =
            programModule <- Some(program)
            sharedVarNames <- program.SharedVarNames
            constData <- program.Data
            functions <- program.Functions
            functionFrameSizes <- program.FrameSizes
            context <- Context(DataFrame(None, functionFrameSizes.[-1]))

        /// Execute instructions sequence
        member this.Interpret() : unit =
            match programModule with
            | Some(program) ->
                try
                    this.ExecuteSequence program
                with e ->
                    // Runtime exceptions handling
                    let errorInfo: Errors.RuntimeErrorInfo = {
                        Index = index
                        OpCodeName = program.Instructions.[index].ToString()
                        Error = e
                        Dump = this.Dump()
                        } in raise (new Errors.ExecutionException(errorInfo))
            | None -> failwith "No instructions specified!"

        /// Dump register contents
        member this.Dump() : string =
            Console.WriteLine("Data dump:")
            context.DumpFrame()