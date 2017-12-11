namespace Evaluator

open System
open System.Collections.Generic

/// Convert data to >NET object and vice versa
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

    /// Convert value to .NET object
    let rec dataToNative = function
        | Empty -> null
        | Number(num) -> num :> Object
        | Text(txt) -> txt :> Object
        | Boolean(bln) -> bln :> Object
        | Date(date) -> date :> Object
        | DataArray(arr) -> Array.ofSeq(Seq.map dataToNative arr) :> Object
        | DataHash(hash) -> 
            let pairs = Seq.map (fun (kv: KeyValuePair<string, Data>) ->
                                    (kv.Key, dataToNative(kv.Value))) hash
            in Map.ofSeq pairs :> Object
        | Iterator(info) -> upcast(info) // Unchanged

    // Convert .NET object to Data
    let rec nativeToData (value: Object) : Data =
        match value with
        | null -> Empty
        | str when (str :? string) -> Text(str :?> string)
        // Convert to float
        | num when (num :? float) -> Number(num :?> float)
        | num when (num :? int) -> Number(float(num :?> int))
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
        | _ -> failwith "Incorrect input data type!"