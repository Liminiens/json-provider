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
        | Value of Type

    let readToken (jToken: JToken) = 
        match jToken.Type with
        | JTokenType.Object -> 
            Object(jToken :?> JObject)
        | JTokenType.Property -> 
            Property(jToken :?> JProperty)
        | JTokenType.Array -> 
            Array(jToken :?> JArray)
        | JTokenType.Float -> 
            Value(typeof<decimal>)
        | _ -> 
            Value(typeof<string>)
    
    let getRootToken str = 
        JObject.Parse(str).Root
    
    let initCap (str: string) = 
        CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str.ToLower())
    
    type GeneratedProvidedProperty =
        | GeneratedProperty of ProvidedProperty
        | GeneratedPropertyDelegate of (Type -> ProvidedProperty)
    
    type ArrayTypes = ProvidedTypeDefinition list
    
    type GeneratorWorkUnit = { 
        ProvidedType: ProvidedTypeDefinition; 
        ProvidedProperty: GeneratedProvidedProperty;
        }
        
    let createSampleType (asm: ProvidedAssembly) ns sample =      
       let createType typeName =
            ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, isErased = false)
       
       let createProperty propertyName propertyType= 
            ProvidedProperty(propertyName, propertyType)

       let mainTypeName = "ProvidedSampleType"
       let rootToken = getRootToken sample

       let mainType = createType mainTypeName
       
       let rec createTypes (generatedType: GeneratorWorkUnit) token =        
            let createTypeProperty (jProperty: JProperty) = 
                for childToken in jProperty do
                    match childToken.Type with
                    | JTokenType.Object ->
                        let name = jProperty.Name
                        //create new type
                        let propertyType = createType name
                        let providedProperty = createProperty name (propertyType.AsType())
                        generatedType.ProvidedType.AddMember providedProperty

                        let objectType = { ProvidedType = propertyType; ProvidedProperty = GeneratedProperty(providedProperty) }
                        createTypes objectType childToken
                    | _ ->
                        let propertyDelegate = 
                            let name = jProperty.Name
                            (fun typeObj -> createProperty name typeObj)

                        let generatedType = {generatedType with ProvidedProperty = GeneratedPropertyDelegate(propertyDelegate)}
                        createTypes generatedType childToken
            
            let createObjectType jObject =
                jObject |> Seq.iter (createTypes generatedType)
            
            let initTypeProperty value workUnit =
                match workUnit.ProvidedProperty with
                    | GeneratedPropertyDelegate(propertyInitFunc) ->
                        propertyInitFunc value |> workUnit.ProvidedType.AddMember 
                    | _ ->
                        failwith "Expected property delegate"           
            
            let createArrayType (jArray: JArray) =
                if jArray.Count > 0 then
                    let firstToken = jArray |> Seq.take 1
                    for childToken in (jArray |> Seq.skip 1) do
                        ()

            match readToken token with            
            | Property(jProperty) ->
                createTypeProperty jProperty
            | Object(jObject) ->
                createObjectType jObject
            | Array(jArray) ->
                createArrayType jArray
            | Value(value) ->
                initTypeProperty value generatedType         
       mainType
    
