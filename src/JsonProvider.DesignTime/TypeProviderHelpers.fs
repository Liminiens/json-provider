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
       
    let createProperty propertyName propertyType = 
        ProvidedProperty(propertyName, propertyType)
      
    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())    
    
    let arrayType (ty: Type) = ty.MakeArrayType()

