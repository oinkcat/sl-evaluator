﻿namespace Evaluator

open System
open System.Collections.Generic

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
        | FunctionRef of int * Data
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
        | FunctionRef(addr, _) -> addr :> Object

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
        // Hash
        | hash when (hash :? Dictionary<string, Object>) ->
            let srcDict = hash :?> Dictionary<string, Object>
            let dstDict = new Dictionary<string, Data>() in
            Seq.iter (fun (kv: KeyValuePair<string, Object>) -> 
                        dstDict.Add(kv.Key, nativeToData kv.Value)) 
                     srcDict
            DataHash dstDict
        | _ -> failwith "Incorrect input data type!"