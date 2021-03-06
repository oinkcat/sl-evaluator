﻿namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open ExtensionTypes
open BuiltinFunctions

/// Loaded modules management
module internal Modules =

    /// All available modules
    let private loadedModules = new Dictionary<string, NativeModule>()

    /// Get native module by it's name
    let private getModuleByName (name: string) : NativeModule =
        let nameOfModule = if String.IsNullOrEmpty(name)
                            then definedModules.[0]
                            else name
        in if loadedModules.ContainsKey(nameOfModule)
            then loadedModules.[nameOfModule]
            else failwithf "Unknown module: %s!" nameOfModule

    /// Load native modules
    let initialize() =
        // Add loaded module
        let addModule (name: string) (loadedModule: NativeModule) =
            if not(loadedModules.ContainsKey(name)) then
                loadedModules.Add(name, loadedModule)

        addModule definedModules.[0] (new BuiltinModule())
        addModule definedModules.[1] (new MathModule())
        addModule definedModules.[2] (new EventsModule())

    /// Get constant value by it's name
    let resolveConstant (moduleName: string) (name: string) : Data =
        getModuleByName(moduleName).GetConstantByName(name)

    /// Get function by it's name
    let resolveFunction (moduleName: string) (name: string) : FuncType =
        getModuleByName(moduleName).GetFunctionByName(name)

    /// Get names of all modules
    let getModuleNames() : string list = List.ofSeq loadedModules.Keys

    /// Get module contents
    let getModuleContents(moduleName: string) : ModuleContents =
        let nativeModule = getModuleByName moduleName in
        (nativeModule.ConstantNames, nativeModule.FunctionNames)
