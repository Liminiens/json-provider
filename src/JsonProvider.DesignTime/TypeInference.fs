namespace FSharp.Liminiens.JsonProvider

module TypeInference =
    open System
    open System.Collections
    open System.Linq
    open System.Globalization
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open ProviderImplementation
    open ProviderImplementation.ProvidedTypes
    open System.Collections.Generic

    type TypeStore(typesNamespace: string) =
        let store = new List<ProvidedTypeDefinition>()

        member this.Namespace = typesNamespace

        member this.AddType(typ) = 
            store.Add(typ)
        
        member this.GetTypes() =
            store |> List.ofSeq

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

    let getUniqueName prefix = 
        let mutable current = 0
        fun () ->
            current <- current + 1
            sprintf "%s%i" prefix current

    let getUniqueTypeName = getUniqueName "ProvidedType"

    let getUniqueProviderName = getUniqueName "TypeProvider"

    let inferType root asm ns =      
        let typens = ns // sprintf "%s.%s" ns (getUniqueProviderName())
        let store = TypeStore(typens)

        let getOrCreateTypeDefinition (generatedType: ProvidedTypeDefinition option) nameCreator =
            match generatedType with 
            | Some(genType) -> 
                genType
            | None ->
                let typeName = nameCreator()
                let genType = createType asm typens typeName
                store.AddType(genType)
                genType          
        
        let getOrCreateNameFromParent (jToken: JToken) nameCreator =
            match Option.ofObj jToken.Parent with
            | Some(parent) ->
                match readToken parent with
                | Property(jProperty) ->
                    initCap jProperty.Name
                | _ ->
                    nameCreator()
            | None ->
                nameCreator()

        let rec processToken token (generatedType: ProvidedTypeDefinition option) = 
            match readToken token with
            | Property(jProperty) ->                 
                let name = initCap jProperty.Name
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> name)
                let inferredType = processToken jProperty.First (Some(generatedType))
                let property = createProperty name inferredType 
                generatedType.AddMember(property)
                generatedType.AsType()
            | Object(jObject) ->
                let typeName =
                    getOrCreateNameFromParent jObject getUniqueTypeName
                let generatedType = 
                    getOrCreateTypeDefinition None (fun _ -> typeName) 
                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)
                generatedType.AsType()
            | Array(jArray) ->
                let typeName =
                    getOrCreateNameFromParent jArray getUniqueTypeName        
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> typeName) 
                let largestToken = jArray |> Seq.maxBy (fun el -> el.Count())
                (*let propertyNames = 
                    match readToken largestToken with
                    | Object(jObject) ->
                        jObject 
                        |> Seq.map (fun el -> (el :?> JProperty).Name)
                        |> List.ofSeq
                    | _ ->
                        []*)
                let inferredType = processToken largestToken (Some(generatedType))
                listType inferredType
            | Value(value) ->
                match value with
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        (processToken root None, store)
