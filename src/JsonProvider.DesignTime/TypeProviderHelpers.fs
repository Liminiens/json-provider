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
                |> Helpers.readStream encoding
                |> Some
            | _ -> 
                None
        | _ -> 
            None

type DisposableTypeProviderForNamespaces(config, ?assemblyReplacementMap) as x =
    inherit TypeProviderForNamespaces(config, ?assemblyReplacementMap=assemblyReplacementMap)
  
    let disposeActions = ResizeArray()
  
    static let mutable idCount = 0
  
    let id = idCount
    let filesToWatch = Dictionary()

    do idCount <- idCount + 1
  
    let dispose typeNameOpt = 
        lock disposeActions <| fun () -> 
            for i = disposeActions.Count-1 downto 0 do
                let disposeAction = disposeActions.[i]
                let discard = disposeAction typeNameOpt
                if discard then
                    disposeActions.RemoveAt(i)

    do
        Logging.log (sprintf "Creating TypeProviderForNamespaces %O [%d]" x id)
        x.Disposing.Add <| fun _ -> 
            Logging.log (sprintf "DisposingEvent %O [%d]" x id)
            dispose None

    member __.Id = id

    member __.SetFileToWatch(fullTypeName, path) =
        lock filesToWatch <| fun () -> 
            filesToWatch.[fullTypeName] <- path

    member __.GetFileToWatch(fullTypeName) =
        lock filesToWatch <| fun () -> 
            match filesToWatch.TryGetValue(fullTypeName) with
            | true, path -> Some path
            | _ -> None

    member __.AddDisposeAction action = 
        lock disposeActions <| fun () -> disposeActions.Add action

    member __.InvalidateOneType typeName = 
        Logging.log (sprintf "InvalidateOneType %s in %O [%d]" typeName x id)
        dispose (Some typeName)
        Logging.log (sprintf "Calling invalidate for %O [%d]" x id)
        base.Invalidate()