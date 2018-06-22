namespace JsonProvider

open System.Globalization
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Newtonsoft.Json.Linq

module Generator =
    open System

    type JsonValue =
        | String of string
        | Float of decimal

    type JsonTokenType = 
        | Object of JObject
        | Property of JProperty
        | Array of JArray
        | Value of JsonValue

    let readToken (jToken: JToken) = 
        match jToken.Type with
        | JTokenType.Object -> 
            Object(jToken :?> JObject)
        | JTokenType.Property -> 
            Property(jToken :?> JProperty)
        | JTokenType.Array -> 
            Array(jToken :?> JArray)
        | JTokenType.Float -> 
            Value(Float(jToken.Value<decimal>()))
        | _ -> 
            Value(String(jToken.Value<string>()))
    
    let getRootToken str = 
        JObject.Parse(str).Root
    
    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())
    
    type GeneratedProvidedProperty =
        | GeneratedProperty of ProvidedProperty option
        | GeneratedPropertyDelegate of (Type -> ProvidedProperty)

    type GeneratedProvidedType = { ProvidedType: ProvidedTypeDefinition; ProvidedProperty: GeneratedProvidedProperty }
        
    let createSampleType (asm: ProvidedAssembly) ns sample =      
       let createType typeName =
            ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, isErased = false)
       
       let createProperty propertyName propertyType= 
            ProvidedProperty(propertyName, propertyType)
       
       let getTokenType token = 
            match token with
            | String(_) ->
                typeof<string>
            | Float(_) ->
                typeof<decimal>

       let mainTypeName = "ProvidedSampleType"
       let rootToken = getRootToken sample

       let mainType = createType mainTypeName
       
       let rec createTypes (generatedType: GeneratedProvidedType) token =
            match readToken token with            
            | Property(jProperty) ->
                for childToken in jProperty do
                    match childToken.Type with
                    | JTokenType.Object ->
                        let name = jProperty.Name
                        //create new type
                        let propertyType = createType name
                        let providedProperty = createProperty name (propertyType.AsType())
                        generatedType.ProvidedType.AddMember providedProperty

                        let objectType = { ProvidedType = propertyType; ProvidedProperty = GeneratedProperty(Some providedProperty) }
                        createTypes objectType childToken
                    | _ ->
                        let propertyDelegate = 
                            let name = jProperty.Name
                            (fun typeObj -> createProperty name typeObj)

                        let generatedType = {generatedType with ProvidedProperty = GeneratedPropertyDelegate(propertyDelegate)}
                        createTypes generatedType childToken
            | Object(jObject) ->
                jObject |> Seq.iter (createTypes generatedType)
            | Array(jArray) ->
                if jArray.Count > 0 then
                    let firstToken = jArray |> Seq.take 1
                    for childToken in (jArray |> Seq.skip 1) do
                        ()
            | Value(value) ->
                match generatedType.ProvidedProperty with
                | GeneratedPropertyDelegate(propGenerateFunc) ->
                    propGenerateFunc(getTokenType value) |> generatedType.ProvidedType.AddMember 
                | _ ->
                    failwith "Expected property delegate"           
       mainType
    
