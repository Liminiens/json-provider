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
    open System.IO
    open System.Reflection
    open System.Diagnostics
    
    let log =
        #if DEBUG
        let file = sprintf """C:\Users\hapin\Documents\Programming\json-provider\log.txt"""
        fun (str: string) -> 
            if File.Exists(file) 
                then
                    use stream = new StreamWriter(File.OpenWrite(file))
                    stream.WriteLine(str) 
                else
                    use stream = new StreamWriter(File.Create(file))
                    stream.WriteLine(str)
        #else
        fun (str: string) -> ()
        #endif

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
        | JTokenType.Integer
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
        let typens = sprintf "%s.%s" ns (getUniqueProviderName())
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

        let rec processToken token (rootGeneratedType: ProvidedTypeDefinition option, generatedType: ProvidedTypeDefinition option) =
            match readToken token with
            | Property(jProperty) ->                 
                let name = initCap jProperty.Name
                log <| sprintf "Property name: %s" name
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> name)
                log <| sprintf "Property type parent name: %s" generatedType.FullName
                let (root, inferredType) = processToken jProperty.First (rootGeneratedType, Some(generatedType))
                let property = createProperty name inferredType 
                log <| sprintf "Property inferred type name: %s" inferredType.FullName
                generatedType.AddMember(property)
                (rootGeneratedType, generatedType.AsType())
            | Object(jObject) ->       
                
                let typeName =
                    getOrCreateNameFromParent jObject getUniqueTypeName                   
                log <| sprintf "Object type name: %s" typeName
                let generatedType = 
                    getOrCreateTypeDefinition None (fun _ -> typeName)
                    
                log <| sprintf "Object generatedTypeName name: %s" generatedType.FullName
                let rootType = 
                    match rootGeneratedType with
                    | None ->
                        log <| sprintf "Object root generatedTypeName name: %s" generatedType.FullName
                        generatedType
                    | Some (typ) ->
                        typ
                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(rootType), Some(generatedType)) |> ignore)
                (Some(rootType), generatedType.AsType())
            | Array(jArray) ->
                let typeName =
                    getOrCreateNameFromParent jArray getUniqueTypeName  
                log <| sprintf "Array typeName name: %s" typeName      
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> typeName) 
                log <| sprintf "Array typeName name: %s" typeName      
                let largestToken = jArray |> Seq.maxBy (fun el -> el.Count())
                (*let propertyNames = 
                    match readToken largestToken with
                    | Object(jObject) ->
                        jObject 
                        |> Seq.map (fun el -> (el :?> JProperty).Name)
                        |> List.ofSeq
                    | _ ->
                        []*)
                let (root, inferredType) = processToken largestToken (rootGeneratedType, Some(generatedType))
                log <| sprintf "Array inferredType name: %s" inferredType.FullName      
                (rootGeneratedType, arrayType inferredType)
            | Value(value) ->
                match value with
                | String ->
                    (rootGeneratedType, typeof<string>)
                | Float ->
                     (rootGeneratedType, typeof<decimal>)

        (processToken root (None, None), store)
