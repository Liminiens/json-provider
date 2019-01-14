#nowarn "0025"
#nowarn "0067"

namespace FSharp.Data.JsonProvider

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open ProviderImplementation.ProvidedTypes
open System
open System.IO
open System.Text
open System.Collections.Generic

[<AutoOpen>]
module internal TypeProviderHelpers =
    let prettyName (name: string) =
        let mutable fx = Char.ToUpper
        String 
            [| for c in Seq.skipWhile (not << Char.IsLetter) name do
                    if Char.IsLetter c then 
                        yield fx c
                        fx <- id
                    elif Char.IsDigit c then
                        yield c
                    else 
                        fx <- Char.ToUpper
            |]
        
    let getPropertyNameAttribute name =
        { new Reflection.CustomAttributeData() with
            member __.Constructor =  typeof<Newtonsoft.Json.JsonPropertyAttribute>.GetConstructor([|typeof<string>|])
            member __.ConstructorArguments = [|Reflection.CustomAttributeTypedArgument(typeof<string>, name)|] :> IList<_>
            member __.NamedArguments = [||] :> IList<_> }

    let createType typeName =
        let typ = ProvidedTypeDefinition(typeName, baseType = Some typeof<obj>, isErased = false, isSealed = false)    
        // Add default constructor (Otherwise Json.NET deserializer will noy be able to create an instance)
        typ.AddMember <| ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>)
        typ

    let createAutoProperty (propName: string) (targetType: Type) typ =
        let typeProperties = 
            targetType.GetProperties() 
            |> Seq.map (fun prop -> prop.Name)
            |> List.ofSeq

        let propertyName =         
            let prettyPropName = prettyName propName

            let checkName name =
                typeProperties |> List.contains name

            if not <| checkName prettyPropName then
               prettyPropName
            else 
               let rec getNextProprtyName name count = 
                   let uniqueName = sprintf "%s%i" name count
                   if not <| checkName uniqueName then
                      uniqueName
                   else
                      getNextProprtyName name (count + 1)
               getNextProprtyName prettyPropName 1
                
        let providedField = ProvidedField("_" + propertyName, typ)
        let providedProperty =
            ProvidedProperty(propertyName, typ,
                getterCode = (fun [this] -> Expr.FieldGetUnchecked(this, providedField)),
                setterCode = (fun [this; v] -> Expr.FieldSetUnchecked(this, providedField, v)))
        providedProperty.AddXmlDoc(sprintf """<summary>Corresponds to property "%s" in object</summary>""" propName)

        if propName <> propertyName then
            getPropertyNameAttribute propName
            |> providedProperty.AddCustomAttribute

        providedField, providedProperty 
    
    let createArrayType (ty: Type) = ty.MakeArrayType()

    let createNullableType (ty: Type) =
        if ty.IsValueType then
            typedefof<Nullable<_>>.MakeGenericType(ty)
        else
            ty

[<AutoOpen>]
module Utility = 
    open System.Globalization

    let readStream (encoding: Encoding) (stream: Stream) =    
        use reader = new StreamReader(stream, encoding)
        reader.ReadToEnd()

    let tryFirst (parameter: 'T) (actions: list<'T -> 'TResult>) =
        let rec tryExecute actionsToTry lastErrorMsg =
            match actionsToTry with
            | action :: tail ->
                try
                    Ok(action parameter)
                with
                | :? Exception as e -> 
                    tryExecute tail e.Message
            | [] ->
               Error(lastErrorMsg)
        tryExecute actions String.Empty
    
    type BoolParseResult =
        | BoolValue of bool
        | NotBoolValue
        | NullValue

    let stringToBool (str: string) = 
        let str = str.ToLower(CultureInfo.InvariantCulture).Trim()
        if String.IsNullOrWhiteSpace(str) then NullValue
        elif str = "false" then BoolValue(false)
        elif str = "true" then BoolValue(true)
        else NotBoolValue

type internal Context(tp: TypeProviderForNamespaces, resolutionFolder: string) =

    member __.ResolutionFolder = resolutionFolder
    
    member __.GetRelativeFilePath(relativePath: string) =         
        let replaceAltChars (str: string) =         
            match Environment.OSVersion.Platform with
            | PlatformID.Unix | PlatformID.MacOSX ->
                str.Replace('\\', Path.DirectorySeparatorChar)
            | _ ->
                str.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar)
        Path.GetFullPath(Path.Combine(resolutionFolder, replaceAltChars relativePath))
    
    member __.ReadResource(resourceName: string, encoding: Encoding) =
        match resourceName.Split(',') with
        | [| asmName; name |] -> 
            let asmName = asmName.Trim()
            let bindingContext = tp.TargetContext
            match bindingContext.TryBindSimpleAssemblyNameToTarget(asmName) with
            | Choice1Of2 asm -> 
                let name = name.Trim()
                Logging.log <| sprintf "Found assembly for resource: %s" asm.FullName
                asm.GetManifestResourceStream(sprintf "%s.%s" asmName name) 
                |> readStream encoding
                |> Some
            | _ -> 
                None
        | _ -> 
            None

//todo: https://github.com/fsharp/FSharp.Data/blob/85ce4eb5460de0bbec568f1a6bdc6b3a91360848/src/CommonProviderImplementation/Helpers.fs
type DisposableTypeProviderForNamespaces(config, ?assemblyReplacementMap) as x =
    inherit TypeProviderForNamespaces(config, ?assemblyReplacementMap=assemblyReplacementMap, addDefaultProbingLocation=true)
   
    let disposeActions = ResizeArray()
   
    static let mutable idCount = 0
   
    let id = idCount
    let filesToWatch = Dictionary<string, string>()
 
    do idCount <- idCount + 1
   
    let dispose typeNameOpt = 
        lock disposeActions <| fun () -> 
            for i = disposeActions.Count-1 downto 0 do
                let disposeAction = disposeActions.[i]
                let discard = disposeAction typeNameOpt
                if discard then
                    disposeActions.RemoveAt(i)
 
    do x.Disposing.Add <| fun _ -> dispose None
 
    member __.Id = id
 
    member __.SetFileToWatch(fullTypeName, path) =
        lock filesToWatch <| fun () -> 
            Logging.log <| sprintf "Added file to cache, file: %s; type: %s" fullTypeName path
            filesToWatch.[fullTypeName] <- path
 
    member __.GetFileToWath(fullTypeName) =
        lock filesToWatch <| fun () -> 
            match filesToWatch.TryGetValue(fullTypeName) with
            | true, path -> Some path
            | _ -> None
 
    member __.AddDisposeAction action = 
        lock disposeActions <| fun () -> disposeActions.Add action
 
    member __.InvalidateOneType typeName = 
        dispose (Some typeName)
        base.Invalidate()