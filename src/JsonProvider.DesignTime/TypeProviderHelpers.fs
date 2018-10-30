#nowarn "0025"

namespace FSharp.Liminiens.JsonProvider

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module internal TypeProviderHelpers =
    open System.Globalization

    let createType typeName =
        // You have to specify asm & ns only for root TP type
        ProvidedTypeDefinition(typeName, baseType = Some typeof<obj>, isErased = false, isSealed = false)
    
    let createAutoProperty (propName: string) typ =
        let providedField = ProvidedField("_" + propName.ToLower(), typ)
        let providedProperty =
            ProvidedProperty(propName, typ,
                getterCode = (fun [this] -> Expr.FieldGetUnchecked(this, providedField)),
                setterCode = (fun [this; v] -> Expr.FieldSetUnchecked(this, providedField, v)))
        providedField, providedProperty 
    
    let createArrayType (ty: Type) = ty.MakeArrayType()

    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())   

