namespace Evaluator

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
    type FnInfo = Core.FunctionInfo
    type Sequence = Core.Sequence
    type Data = DataContext.Data

    /// Instruction data source types with value
    type private Source =
        | Number of float
        | Text of string
        | Register of int
        | DataKey of string

    /// Entry point token
    let private EntryToken = ".entry"

    /// Data section token
    let private DataToken = ".data"

    /// Get data with source type for instruction
    let private getSource (arg: string) : Source =
        let prefix = arg.[0]
        let rest = arg.Substring(1)
        let tempNum: float ref = ref 0.0 in
        match prefix with
        | '#' -> Register(int(rest))
        | '$' -> DataKey(rest)
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

    /// Translate one command into opcode
    let private translate (line: string) : OpCode * string option =

        let spacePos = line.IndexOf(' ')
        let name = if spacePos > -1
                   then line.Substring(0, spacePos)
                   else line
        let argument = if spacePos > -1
                       then line.Substring(spacePos + 1).Trim()
                       else String.Empty
        let opName = name.Trim().ToLowerInvariant() in

        let opCode =
            match opName with
            // Data accessing
            | "load" -> match getSource argument with
                        | Number num -> OpCode.LoadNum num
                        | Text str -> OpCode.LoadStr str
                        | Register regIdx -> OpCode.LoadReg regIdx
                        | DataKey key -> OpCode.LoadData key
            | "load.global" -> OpCode.LoadRegGlobal(argument |> asNumber)
            | "load.const" -> let dataIdx : int ref = ref 0 in
                              if Int32.TryParse(argument, dataIdx)
                                  then OpCode.LoadDataArray dataIdx.Value
                                  else OpCode.LoadConst argument
            | "dup" -> OpCode.Duplicate
            | "unload" when no(argument) -> OpCode.Unload
            | "store" -> OpCode.Store(argument |> asNumber)
            | "store.global" -> OpCode.StoreGlobal(argument |> asNumber)
            | "reset" -> OpCode.Reset(argument |> asNumber)
            // Arrays/hashes
            | "mk_array" -> OpCode.MakeArray(argument |> asNumber)
            | "mk_hash" -> OpCode.MakeHash(argument |> asNumber)
            | "get" -> OpCode.ArrayGet
            | "set" -> OpCode.ArraySet
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
            | "call" -> if argument.Length > 0
                        then OpCode.Call(argument.ToLowerInvariant())
                        else failwith "Function name required!"
            | "invoke" -> OpCode.Invoke(-1)
            | "ret" -> OpCode.Ret
            | _ -> failwithf "Invalid instruction: %s!" opName
        in match opCode with
            // Return label name for jump commands
            | OpCode.Jump _ as jOpCode -> (jOpCode, Some(argument))
            | OpCode.CondJump _ as cjOpCode -> (cjOpCode, Some(argument))
            | OpCode.Invoke _ as invOpCode -> (invOpCode, Some(argument))
            | nonJumpOpCode -> (nonJumpOpCode, None)

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
        let data = new List<Data>()
        let sequence = new List<OpCode>()
        let functions = new List<FnInfo>()
        let entryInstructionIndex = ref 0

        // Determine functions frame size
        let funcAddress: int ref = ref -1
        let frameSizes = new Dictionary<int, int>()
        frameSizes.Add(funcAddress.Value, 0)

        // Jump labels
        let labelRefs = new Dictionary<int, string>()
        let labelTargets = new Dictionary<string, int>()

        let readingData: bool ref = ref false

        /// Read next instruction line
        let rec readNext (reader: StreamReader) (index: int) (lineNo: int) =
            let mutable isInstruction = false

            if not reader.EndOfStream then
                try
                    let line = reader.ReadLine().Trim() in
                    if line.Length > 0 then
                        let isComment = isComment(line)
                        let isLabel = not(isComment) && isLabel(line)
                        let isDirective = isDirective(line)

                        if isDirective then do
                            // Compiler directive
                            if line.Equals(DataToken) then
                                readingData := true
                            else
                                readingData := false
                                if line.Equals(EntryToken) then
                                    funcAddress := -1
                                    entryInstructionIndex := index
                        else if readingData.Value then
                            // Constant data arrays
                            data.Add(loadConstArray(line))
                        else if not(isComment || isLabel || isDirective) then
                            // Regular instruction
                            isInstruction <- true
                            let (opCode, label) = translate line
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
                        else if isLabel then do
                            // Label
                            let labelName = line.Substring(0, line.Length - 1)
                            // Check if function definition
                            let dotPos = labelName.IndexOf('.') in
                            if dotPos > -1 then do
                                let nArgs = int(labelName.Substring(dotPos + 1))
                                let fnDefInfo : FnInfo = {
                                    Address = index;
                                    ParamsCount = nArgs
                                }
                                functions.Add(fnDefInfo)
                                let fnName = labelName.Substring(0, dotPos)
                                labelTargets.Add(fnName, index)
                                // Initial frame size
                                funcAddress := index
                                frameSizes.Add(index, nArgs)
                            else
                                labelTargets.Add(labelName, index)

                // Loading exception handling
                with e -> raise (new Errors.LoadException(lineNo, e.Message))

                let nextIndex = if isInstruction then index + 1 else index
                let nextLineNo = lineNo + 1 in
                readNext reader nextIndex nextLineNo

        use reader = new StreamReader(dataStream) in readNext reader 0 1

        // Resolve label jump addresses
        labelRefs.Keys
        |> Seq.iter (fun opIdx ->
            let labelName = labelRefs.[opIdx]
            let target = labelTargets.[labelName]
            sequence.[opIdx] <- 
                match sequence.[opIdx] with
                // Rewrite jump opcodes with correct targets
                | OpCode.Jump _ -> OpCode.Jump target
                | OpCode.CondJump (cond, _) -> OpCode.CondJump (cond, target)
                | OpCode.Invoke _ -> OpCode.Invoke target
                | other -> other (* This never happens *) )

        // Return all information about program
        {
            Data = data
            Functions = functions
            FrameSizes = frameSizes
            Instructions = sequence
            EntryPoint = entryInstructionIndex.Value
        }
