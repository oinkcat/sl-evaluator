namespace Evaluator

open System
open System.Collections.Generic
open DataTypes
open DataContext
open ExtensionTypes
open BuiltinFunctions

/// Loaded modules management
module internal Modules =

    /// All available modules
    let private loadedModules = new Dictionary<string, NativeModule>()

    /// Get native module by it's name
    let private getModuleByName (name: string) : NativeModule =
        let nameOfModule = if String.IsNullOrEmpty(name)
                            then builtinModuleName
                            else name
        in loadedModules.[nameOfModule]

    /// Load native modules
    let initialize() =
        // Add loaded module
        let addModule (name: string) (loadedModule: NativeModule) =
            if not(loadedModules.ContainsKey(name)) then
                loadedModules.Add(name, loadedModule)

        addModule builtinModuleName (new BuiltinModule())
        addModule mathModuleName (new MathModule())

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
