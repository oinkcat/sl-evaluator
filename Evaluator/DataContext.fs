namespace Evaluator

open System
open System.Collections.Generic

/// All data context structures
module DataContext =
    
    /// Data value container
    type Data =
        | Empty
        | Number of float
        | Text of string
        | Boolean of bool
        | Date of DateTime
        | DataArray of List<Data>
        | DataHash of Dictionary<string, Data>

    /// Global/function scope data frame
    type DataFrame(parent : DataFrame option, size: int) =

        /// Data stack
        let stack = new Stack<Data>()

        /// Data registers
        let registers: Data array =
            List.toArray (List.map (fun _ -> Data.Empty) [1..size])

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
        member this.HasResult() = stack.Count > 0

        /// Dump contents of frame and all parent frames
        member this.Dump() : unit = ()

    /// Data context
    type Context(globalFrame: DataFrame) =

        /// Current frame
        let mutable frame: DataFrame = globalFrame

        /// Input data
        let input = new Dictionary<string, Data>()

        /// Results as text
        let textResults = new List<string>()

        /// Named results
        let namedResults = new Dictionary<string, Object>()

        /// Convert value to .NET object
        let rec convertToObject = function
            | Empty -> null
            | Number(num) -> num :> Object
            | Text(txt) -> txt :> Object
            | Boolean(bln) -> bln :> Object
            | Date(date) -> date :> Object
            | DataArray(arr) -> Array.ofSeq(Seq.map convertToObject arr) :> Object
            | DataHash(hash) -> 
                let pairs = Seq.map (fun (kv: KeyValuePair<string, Data>) ->
                                        (kv.Key, convertToObject(kv.Value))) hash
                in Map.ofSeq pairs :> Object

        member this.Frame with get() = frame and 
                               set(newFrame: DataFrame) = frame <- newFrame

        member this.Input = input

        member this.TextOutput = textResults

        member this.NamedResults = namedResults

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

        /// Convert data value to string
        member this.DataToString (data: Data) =
            match data with
            | Number(num) -> num.ToString()
            | Text(str) -> str
            | Boolean(bln) -> bln.ToString()
            | Date(date) -> date.ToString()
            | Empty -> "null"
            | DataArray(arr) ->
                let elemStrings = Seq.map this.DataToString arr in
                String.Concat('[', String.Join(";", elemStrings), ']')
            | DataHash(hash) ->
                let elemStrings = Seq.map (fun (kv: KeyValuePair<string, Data>) ->
                                    let repr = this.DataToString(kv.Value)
                                    String.Concat(kv.Key, ':', repr))
                                    hash in
                String.Concat('[', String.Join(";", elemStrings), ']')

        /// Pop value from stack and convert to string
        member this.PopAsResult() = this.DataToString(this.PopFromStack())

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

        /// Pop value as plain .NET object
        member this.PopAsNativeObject() : Object =
            convertToObject(this.PopFromStack())

        /// Duplicate data on top of stack
        member this.DuplicateStackData() =
            let data = this.PopFromStack() in do
            this.PushToStack data
            this.PushToStack data