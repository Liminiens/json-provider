namespace FSharp.Liminiens.JsonProvider

open System
open System.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module internal TypeProviderHelpers =
    open System.Globalization

    let createType typeName =
        // You have to specify asm & ns only for root TP type
        ProvidedTypeDefinition(typeName, baseType = Some typeof<obj>, isErased = false, isSealed = false)
       
    let createField name typ = 
        let field = ProvidedField(name, typ)
        field.SetFieldAttributes(FieldAttributes.Public)
        field
      
    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())    
    
    let makeArrayType (ty: Type) = ty.MakeArrayType()

