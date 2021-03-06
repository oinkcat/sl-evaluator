﻿namespace Evaluator

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

/// Loads compiled instructions
module internal Loader =

    /// Type short names
    type OpCode = Core.OpCode
    type MathBin = Core.MathOp
    type StringBin = Core.StringOp
    type CmpBin = Core.CmpOp
    type BoolBin = Core.LogicOp
    type Cond = Core.JumpCondition
    type FnDisp = Core.FunctionKind
    type FnInfo = DataContext.FunctionInfo
    type Sequence = Core.Sequence
    type SrcInfo = Core.SourceInfo
    type Data = DataTypes.Data
    type Index = Core.ArrayIndex

    /// Instruction data source types with value
    type private Source =
        | Number of float
        | Text of string
        | Register of int

    /// Script file sections
    type private Section =
        | Unknown
        | NativeReferences
        | SharedVars
        | Data
        | Code

    type CommandInfo = OpCode * string option * SrcInfo option

    /// Get data with source type for instruction
    let private getSource (arg: string) : Source =
        let prefix = arg.[0]
        let rest = arg.Substring(1)
        let tempNum: float ref = ref 0.0 in
        match prefix with
        | '#' -> Register(int(rest))
        | '"' when rest.EndsWith("\"") ->
            let contents = rest.Substring(0, rest.Length - 1) in
                Text(contents)
        | _ when Double.TryParse(arg.Replace('.', ','), tempNum) -> 
                Number(tempNum.Value)
        | _ -> failwith "Invalid data specification!"

    /// Is no argument given
    let private no (arg: string) = String.IsNullOrEmpty(arg)

    /// Get argument as number
    let private asNumber (arg : string) = Int32.Parse(arg)

    /// Get argument as text string
    let private asText (arg: string) =
        if arg.StartsWith("\"") && arg.EndsWith("\"") && arg.Length > 1
        then arg.Substring(1, arg.Length - 2)
        else failwith "Invalid string value!"

    /// Math operation names translation map
    let private mathOps = Map([("add", MathBin.Add); ("sub", MathBin.Sub);
                               ("mul", MathBin.Mul); ("div", MathBin.Div);
                               ("mod", MathBin.Mod)])

    /// Comparsion operation names translation map
    let private cmpOps = Map([("eq", CmpBin.Equal); ("ne", CmpBin.NotEqual);
                              ("lt", CmpBin.Less); ("gt", CmpBin.Greater);
                              ("le", CmpBin.LessOrEqual);
                              ("ge", CmpBin.GreaterOrEqual)])

    /// Boolean operation names translation map
    let private boolOps = Map([("or", BoolBin.Or); ("and", BoolBin.And);
                               ("xor", BoolBin.Xor); ("not", BoolBin.Not)])

    /// String operations
    let private stringOps = Map([("concat", StringBin.Concat);
                                 ("format", StringBin.Format)])

    /// Is operation a math operation
    let private isMathOp (name: string) = mathOps.ContainsKey(name)
    /// Is operation a string operation
    let private isStringOp (name: string) = stringOps.ContainsKey(name)
    /// Is operation a comparsion operation
    let private isCmpOp (name: string) = cmpOps.ContainsKey(name)
    /// Is operation a boolean operation
    let private isBoolOp (name: string) = boolOps.ContainsKey(name)

    /// Conditional jump operation names and translations
    let private jumpConditions = Map([("jmpeq", Cond.IfEqual)
                                      ("jmpne", Cond.IfNotEqual)
                                      ("jmplt", Cond.IfLess)
                                      ("jmpgt", Cond.IfGreater)
                                      ("jmple", Cond.IfLessOrEqual)
                                      ("jmpge", Cond.IfGreatOrEqual)])

    /// Split object qualified name
    let private splitQualifiedName (name: string) =
        let nameParts = name.Split(':')
        let modName = if nameParts.Length = 3
                        then nameParts.[0]
                        else String.Empty
        let objName = if nameParts.Length = 3
                        then nameParts.[2]
                        else name in
        (modName, objName)

    /// Command regex
    let private cmdRegex =
        let opts = RegexOptions.Compiled in
        new Regex("^(\S+)(?:(?! ; #)\s(.+?))?(?: ; #(.+?)\((\d+)\))?$", opts)

    /// Translate one command into opcode
    let private translate (line: string) : CommandInfo =

        // Parse command
        let cmdMatch = cmdRegex.Match(line)
        if not(cmdMatch.Success) then failwithf "Invalid command format: %s" line

        let name = cmdMatch.Groups.[1].Value
        let argument = if cmdMatch.Groups.[2].Success
                           then cmdMatch.Groups.[2].Value.Trim()
                           else String.Empty

        let srcInfo : SrcInfo option =
            if cmdMatch.Groups.[3].Success && cmdMatch.Groups.[4].Success
                then let info : SrcInfo = {
                            ModuleName = cmdMatch.Groups.[3].Value
                            LineNumber = int(cmdMatch.Groups.[4].Value)
                         } in Some(info)
                else None

        let opCode =
            match name.Trim().ToLowerInvariant() with
            // Data accessing
            | "load" -> match getSource argument with
                        | Number num -> OpCode.LoadNum num
                        | Text str -> OpCode.LoadStr str
                        | Register regIdx -> OpCode.LoadReg regIdx
            | "load.global" -> OpCode.LoadRegGlobal(argument |> asNumber)
            | "load.outer" -> let argParts = argument.Split(':')
                              let level = int(argParts.[0])
                              let regIdx = int(argParts.[1]) in
                              OpCode.LoadRegOuter (level, regIdx)
            | "load.const" -> let dataIdx : int ref = ref 0 in
                              if Int32.TryParse(argument, dataIdx)
                                  then OpCode.LoadDataArray dataIdx.Value
                                  else
                                    let names = splitQualifiedName argument
                                    let modName = fst names
                                    let name = snd names
                                    let value = Modules.resolveConstant modName 
                                                                        name in
                                    OpCode.LoadConst (value)
            | "dup" -> OpCode.Duplicate
            | "unload" when no(argument) -> OpCode.Unload
            | "store" -> OpCode.Store(argument |> asNumber)
            | "store.global" -> OpCode.StoreGlobal(argument |> asNumber)
            | "store.outer" -> let argParts = argument.Split(':')
                               let level = int(argParts.[0])
                               let regIdx = int(argParts.[1]) in
                               OpCode.StoreOuter (level, regIdx)
            | "reset" -> OpCode.Reset(argument |> asNumber)

            // Arrays/hashes
            | "mk_array" -> OpCode.MakeArray(argument |> asNumber)
            | "mk_hash" -> OpCode.MakeHash(argument |> asNumber)
            | "mk_ref.udf" -> OpCode.MakeFunctionRef -1
            | "bind_refs" -> OpCode.BindInner
            | "get" -> OpCode.ArrayGet
            | "get.index" -> match getSource argument with
                             | Number idx ->OpCode.ArrayGetIdx(Index.ElemIndex idx)
                             | Text key -> OpCode.ArrayGetIdx(Index.ElemKey key)
                             | _ -> failwithf "Invalid array index: %s!" argument
            | "set" -> OpCode.ArraySet
            | "set.index" -> match getSource argument with
                             | Number idx ->OpCode.ArraySetIdx(Index.ElemIndex idx)
                             | Text key -> OpCode.ArraySetIdx(Index.ElemKey key)
                             | _ -> failwithf "Invalid array index: %s!" argument
            | "set.op" when isMathOp(argument) ->
                let mathOp = mathOps.[argument] in
                OpCode.ArraySetMath(mathOps.[argument])

            // Operations
            | math when isMathOp(math) -> OpCode.Math mathOps.[math]
            | str when isStringOp(str) -> OpCode.String stringOps.[str]
            | cmp when isCmpOp(cmp) -> OpCode.Compare cmpOps.[cmp]
            | bln when isBoolOp(bln) -> OpCode.Logic boolOps.[bln]
            | "jmp" -> OpCode.Jump(-1)
            | jmp when jmp.StartsWith("jmp") -> let cond = jumpConditions.[jmp]
                                                OpCode.CondJump (cond, -1)
            | "emit" -> if String.IsNullOrEmpty(argument)
                        then OpCode.Emit
                        else OpCode.EmitNamed(argument |> asText)

            // Function call/return
            | "call.native" -> if argument.Length > 0
                                then
                                    let names = splitQualifiedName argument
                                    let modName = fst names
                                    let name = snd names
                                    let func = Modules.resolveFunction modName name
                                    OpCode.Call(FnDisp.Native func)
                                else failwith "Function name required!"
            | "call.udf" -> OpCode.Call(FnDisp.Defined -1)
            | "invoke" -> OpCode.Invoke
            | "ret" -> OpCode.Ret
            | _ -> failwithf "Invalid instruction: %s!" name

        let opWithLabel =
            match opCode with
            // Return label name for jump commands
            | OpCode.Jump _ as jOpCode -> (jOpCode, Some(argument))
            | OpCode.CondJump _ as cjOpCode -> (cjOpCode, Some(argument))
            | OpCode.Call disp as callOpCode -> 
                match disp with
                | FnDisp.Defined _ -> (callOpCode, Some(argument))
                | _ -> (callOpCode, None)
            | OpCode.MakeFunctionRef _ as refOpCode -> (refOpCode, Some(argument))
            | nonJumpOpCode -> (nonJumpOpCode, None)

        in (fst opWithLabel, snd opWithLabel, srcInfo)

    /// Is line is a comment
    let private isComment (line: string) = line.StartsWith(";")

    /// Is line is a label
    let private isLabel (line: string) =
        line.EndsWith(":") && line.Length > 1

    /// Is line a directive
    let private isDirective (line: string) = line.StartsWith(".")

    /// Load constant data array
    let private loadConstArray (line: string) : Data =
        // String or number
        let elemTokenRegex = new Regex("(\"[^\"]+\")|([0-9]+(?:\.[0-9]+)?)",
                                       RegexOptions.Compiled)
        let elements = new List<Data>() in
        for m: Match in elemTokenRegex.Matches(line) do
            if m.Value.Length > 0 then
                let element = if m.Value.StartsWith("\"") 
                              then Data.Text(m.Value |> asText)
                              else let floatStr = m.Value.Replace('.', ',') in
                                   Data.Number(Double.Parse(floatStr))
                elements.Add(element)
        Data.DataArray(elements)

    /// Load instructions sequence from stream
    let Load (dataStream: Stream) : Sequence =
        // Program Information to be loaded
        let sharedVariables = new List<string>()
        let data = new List<Data>()
        let sequence = new List<OpCode>()
        let sourceMap = new Dictionary<int, SrcInfo>()

        let functions = new Dictionary<int, FnInfo>()
        functions.Add(-1, {
            Address = Int32.MaxValue
            ParamsCount = 0
            FrameSize = 0
        })

        // Determine functions frame size
        let funcAddress: int ref = ref -1
        let frameSizes = new Dictionary<int, int>()
        frameSizes.Add(funcAddress.Value, 0)

        // Jump labels
        let labelRefs = new Dictionary<int, string>()
        let labelTargets = new Dictionary<string, int>()

        // Currently reading section
        let currentSection: Section ref = ref Unknown

        /// Read next instruction line
        let rec readNext (reader: StreamReader) (index: int) (lineNo: int) =
            let isInstruction : bool ref = ref false

            // Read directive line
            let readDirective (line: string) =
                // Compiler directive
                match line with
                | ".refs" -> currentSection := NativeReferences
                | ".shared" -> currentSection := SharedVars
                | ".data" -> currentSection := Data
                | ".defs" -> currentSection := Code
                | ".entry" ->
                    currentSection := Code
                    funcAddress := -1
                    let mainCodeInfo = functions.[-1]
                    functions.[-1] <- { mainCodeInfo with Address = index }
                | _ -> failwithf "Unknown script file section: %s!" line

            // Read regular instruction
            let readInstruction (line: string) =
                // Regular instruction
                isInstruction.Value <- true
                let (opCode, label, srcInfo) = translate line
                if label.IsSome then
                    labelRefs.Add(index, label.Value)
                // Check frame size
                match opCode with
                    | OpCode.Store(regIdx) ->
                        let currentNum = frameSizes.[funcAddress.Value]
                        let newNum = regIdx + 1 in
                        if newNum > currentNum then
                            frameSizes.[funcAddress.Value] <- newNum
                    | _ -> ()
                sequence.Add(opCode)
                if srcInfo.IsSome then
                    sourceMap.Add(sequence.Count, srcInfo.Value)

            // Read label/function definition
            let readLabel (line: string) =
                let labelName = line.Substring(0, line.Length - 1)
                // Check if function definition
                let dotPos = labelName.IndexOf('.') in
                if dotPos > -1 then do
                    let nArgs = int(labelName.Substring(dotPos + 1))
                    let fnDefInfo : FnInfo = {
                        Address = index
                        ParamsCount = nArgs
                        FrameSize = -1
                    }
                    functions.Add(index, fnDefInfo)
                    let fnName = labelName.Substring(0, dotPos)
                    labelTargets.Add(fnName, index)
                    // Initial frame size
                    funcAddress := index
                    frameSizes.Add(index, nArgs)
                else
                    labelTargets.Add(labelName, index)

            if not reader.EndOfStream then
                try
                    let line = reader.ReadLine().Trim() in
                    if line.Length > 0 then
                        // Determine term type
                        let isComment = isComment(line)
                        let isLabel = not(isComment) && isLabel(line)
                        let isDirective = isDirective(line)

                        if isDirective then
                            readDirective line
                        else if currentSection.Value = NativeReferences then
                            () // Temporary
                        else if currentSection.Value = Data then
                            // Constant data arrays
                            data.Add(loadConstArray(line))
                        else if currentSection.Value = SharedVars then
                            // Shared variable name
                            sharedVariables.Add(line)
                            frameSizes.[-1] <- sharedVariables.Count
                        else if not(isComment || isLabel || isDirective) then
                            readInstruction line
                        else if isLabel then do
                            readLabel line

                // Loading exception handling
                with e -> raise (new Errors.LoadException(lineNo, e.Message))

                let nextIndex = if isInstruction.Value then index + 1 else index
                let nextLineNo = lineNo + 1 in
                readNext reader nextIndex nextLineNo

        use reader = new StreamReader(dataStream) in readNext reader 0 1

        // Resolve label jump addresses
        labelRefs.Keys
        |> Seq.iter (fun opIdx ->
            let labelName = labelRefs.[opIdx]
            if not(labelTargets.ContainsKey(labelName))
                then failwithf "Label name: %s not found!" labelName

            let target = labelTargets.[labelName]
            sequence.[opIdx] <- 
                match sequence.[opIdx] with
                // Rewrite jump opcodes with correct targets
                | OpCode.Jump _ -> OpCode.Jump target
                | OpCode.CondJump (cond, _) -> OpCode.CondJump (cond, target)
                | OpCode.Call _ -> OpCode.Call(FnDisp.Defined target)
                | OpCode.MakeFunctionRef _ -> OpCode.MakeFunctionRef target
                | other -> other (* This never happens *) )

        // Update functions frame size
        Seq.iter (fun (kv : KeyValuePair<int, int>) ->
                    let origFnInfo = functions.[kv.Key]
                    functions.[kv.Key] <- { origFnInfo with FrameSize = kv.Value })
                    frameSizes

        // Return all information about program
        {
            SharedVarNames = sharedVariables
            Data = data
            Functions = functions
            Instructions = sequence
            SourceMap = sourceMap
        }
 