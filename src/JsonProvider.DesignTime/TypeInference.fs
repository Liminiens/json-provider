namespace FSharp.Liminiens.JsonProvider

module internal TypeInference =
    open System
    open System.Linq
    open System.Globalization
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open ProviderImplementation
    open ProviderImplementation.ProvidedTypes

    type JsonValue =
        | String 
        | Float

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
            Value(Float)
        | _ -> 
            Value(String)

    let inferType root asm ns =        
        let getOrCreateTypeDefinition (generatedType: ProvidedTypeDefinition option) nameCreator =
            match generatedType with 
            | Some(genType) -> 
                genType
            | None ->
                createType asm ns (nameCreator())             
        
        let getOrCreateNameFromParent (jToken: JToken) nameCreator =
            match Option.ofObj jToken.Parent with
            | Some(parent) ->
                match readToken parent with
                | Property(jProperty) ->
                    jProperty.Name
                | _ ->
                    nameCreator()
            | None ->
                nameCreator()

        let rec processToken token (generatedType: ProvidedTypeDefinition option) = 
            match readToken token with
            | Property(jProperty) ->                 
                let name = jProperty.Name
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> name)  
                let inferredType = 
                    Some(generatedType) 
                    |> processToken jProperty.First                  
                let property = createProperty name inferredType
                generatedType.AddMember property
                generatedType.AsType()
            | Object(jObject) ->
                let typeName =
                    getOrCreateNameFromParent jObject (fun _ -> "Todo1")
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> typeName)  
                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)
                generatedType.AsType()
            | Array(jArray) ->
                let typeName =
                    getOrCreateNameFromParent jArray (fun _ -> "Todo2")         
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> typeName)  
                let largestToken = jArray |> Seq.maxBy (fun el -> el.Count())
                let propertyNames = 
                    match readToken largestToken with
                    | Object(jObject) ->
                        jObject 
                        |> Seq.map (fun el -> (el :?> JProperty).Name)
                        |> List.ofSeq
                    | _ ->
                        []
                processToken largestToken (Some(generatedType))
                |> arrayType 
            | Value(value) ->
                match value with
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        processToken root None
