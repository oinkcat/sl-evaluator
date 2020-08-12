namespace Evaluator

open System
open System.Collections.Generic
open System.Text

/// Data types and conversions
module internal DataTypes =

    /// Data value container
    type Data =
        | Empty
        | Number of float
        | Text of string
        | Boolean of bool
        | Date of DateTime
        | DataArray of List<Data>
        | DataHash of Dictionary<string, Data>
        | Iterator of IteratorInfo
        | FunctionRef of int * Data * DataFrame
        member this.IsNull with get() = match this with
                                        | Empty -> true
                                        | _ -> false

    /// Perform iteration on array/hash
    and IteratorInfo(iterable: Data) =

        let mutable target: Data = iterable

        let hashKeys: (string array) option =
            match target with
            | DataHash hash -> Some(Array.ofSeq hash.Keys)
            | _ -> None

        let mutable index: int = 0

        let elementsCount: int =
            match target with
            | DataArray arr -> arr.Count
            | DataHash hash -> hash.Count
            | other -> 1

        let mutable atEnd: bool = elementsCount = 0

        /// Get next element
        member this.Iterate() : Data =
            let itemIndex = index in
            index <- index + 1

            match target with
            | DataArray arr -> arr.[itemIndex]
            | DataHash hash -> Text hashKeys.Value.[itemIndex]
            | other -> other

        /// Has next element
        member this.HasNext with get() : bool = index < elementsCount

    /// Global/function scope data frame
    and  DataFrame(parent : DataFrame option, size: int, closure: DataFrame option) =

        /// Data stack
        let stack = new LinkedList<Data>()

        /// Data registers
        let registers: Data array = Array.zeroCreate size

        /// Is Executed as function reference
        let mutable isExecutedAsRef: bool = false
        
        (* ******************** Instance members ******************** *)

        /// Caller frame reference
        member this.Caller with get() = parent

        //// Closure frame reference
        member this.Closure with get() = closure

        /// Get outer level closure frame
        member this.GetOuter(level: int) : DataFrame =
            let rec getParent frame left : DataFrame =
                if left = 0
                    then frame
                    else getParent (frame.Caller.Value) (left - 1)

            match closure with
            | Some(frame) -> getParent frame (level - 1)
            | None -> failwith "No closure frame!"

        /// Executed as function reference
        member this.IsReferenced
            with get() = isExecutedAsRef and
                 set(isRef: bool) = isExecutedAsRef <- isRef

        /// Global frame reference
        member this.Global
            with get() =
                match parent with
                | None -> this
                | Some(frame) -> frame.Global

        /// Create frame for calee function
        member this.CreateChildFrame (size: int) (closure: DataFrame option) =
            DataFrame(Some(this), size, closure)

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
 
    /// Convert value to .NET object
    let rec dataToNative = function
        | Empty -> null
        | Number(num) -> num :> Object
        | Text(txt) -> txt :> Object
        | Boolean(bln) -> bln :> Object
        | Date(date) -> date :> Object
        | DataArray(arr) -> Array.ofSeq(Seq.map dataToNative arr) :> Object
        | DataHash(hash) ->
            let nativeDict = new Dictionary<string, Object>() in
            Seq.iter (fun (kv: KeyValuePair<string, Data>) ->
                        nativeDict.Add(kv.Key, dataToNative(kv.Value))) hash
            nativeDict :> Object
        | Iterator(info) -> upcast(info) // Unchanged
        | FunctionRef(addr, _, _) -> addr :> Object

    // Convert .NET object to Data
    let rec nativeToData (value: Object) : Data =
        match value with
        | null -> Empty
        | str when (str :? string) -> Text(str :?> string)
        // Convert to float
        | num when (num :? float) -> Number(num :?> float)
        | num when (num :? int32) -> Number(float(num :?> int32))
        | num when (num :? int64) -> Number(float(num :?> int64))
        // Date
        | date when (date :? DateTime) -> Date(date :?> DateTime)
        // Boolean
        | bln when (bln :? bool) -> Boolean(bln :?> bool)
        // Array
        | arr when arr.GetType().IsArray ->
            let nativeArray = arr :?> Array
            let mapped = Seq.map (fun (idx: int) -> 
                                    nativeToData (nativeArray.GetValue(idx)))
                                    (seq { 0 .. (nativeArray.Length - 1) })
            DataArray(new List<Data>(mapped))
        // Enumerable
        | enum when (enum :? IEnumerable<Object>) ->
            let nativeEnumerable = enum :?> IEnumerable<Object>
            let mapped = Seq.map (fun (elem: obj) -> 
                                    nativeToData elem)
                                    nativeEnumerable
            DataArray(new List<Data>(mapped))
        // Hash
        | hash when (hash :? IDictionary<string, Object>) ->
            let srcDict = hash :?> IDictionary<string, Object>
            let dstDict = new Dictionary<string, Data>() in
            Seq.iter (fun (kv: KeyValuePair<string, Object>) -> 
                        dstDict.Add(kv.Key, nativeToData kv.Value)) 
                     srcDict
            DataHash dstDict
        | _ -> failwith "Incorrect input data type!"