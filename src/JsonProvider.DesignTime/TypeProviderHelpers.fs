#nowarn "0025"
#nowarn "0067"

namespace FSharp.Data.JsonProvider

open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open ProviderImplementation.ProvidedTypes
open System
open System.IO
open System.Text

[<AutoOpen>]
module internal TypeProviderHelpers =
    open System.Collections.Generic

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
    
    let stringToBool (str: string) =
        let str = str.ToLower(CultureInfo.InvariantCulture).Trim()
        if str = "false" then Some(false)
        elif str = "true" then Some(true)
        else None

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