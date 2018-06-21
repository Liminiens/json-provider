namespace JsonProvider

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Newtonsoft.Json.Linq

module Generator =
    type JsonTokenType = 
        | Object of JObject
        | Property of JProperty
        | Array of JArray
        | FloatValue of decimal
        | StringValue of string

    let readToken (jToken: JToken) = 
        match jToken.Type with
        | JTokenType.Object -> 
            Object(jToken :?> JObject)
        | JTokenType.Property -> 
            Property(jToken :?> JProperty)
        | JTokenType.Array -> 
            Array(jToken :?> JArray)
        | JTokenType.Float -> 
            FloatValue(jToken.Value<decimal>())
        | _ -> 
            StringValue(jToken.Value<string>())
    
    let getRootToken str = 
        JObject.Parse(str).Root

    let iterOver (jObject: JObject) = 
        let directTokens = jObject.Children() |> List.ofSeq
        for token in directTokens do
            ()
    
    let createSampleType (asm: ProvidedAssembly) ns sample =
       let mainTypeName = "ProvidedSampleType"
       let rootToken = getRootToken sample
       let mainType = 
            ProvidedTypeDefinition(asm, ns, "ProvidedSampleType", baseType = Some typeof<obj>, isErased = false)
       let createType typeCount =
            ProvidedTypeDefinition(asm, ns, sprintf "ProvidedSampleType%i" typeCount, baseType = Some typeof<obj>, isErased = false)
       
       let createTypes token (providedType: ProvidedTypeDefinition) (typeCount: int) =
            match readToken token with
            | Object(jObject) ->
                let newType = createType typeCount
                let property = ProvidedProperty(newType.Name, newType.AsType())
                newType.AddMember property
                ()
            | Property(jProperty) ->
                ()
            | Array(jArray) ->
                ()
            | FloatValue(fValue) ->
                ()
            | StringValue(sValue) ->
                ()
       ()
    
