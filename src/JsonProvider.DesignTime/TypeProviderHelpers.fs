namespace FSharp.Liminiens.JsonProvider

open System.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module internal TypeProviderHelpers =
    open System.Globalization

    let createType (asm: Assembly) ns typeName =
        ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, isErased = false)
       
    let createProperty propertyName propertyType = 
        ProvidedProperty(propertyName, propertyType)
      
    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())    
    
    let listType ty = typedefof<System.Collections.Generic.List<_>>.MakeGenericType[| ty |]

