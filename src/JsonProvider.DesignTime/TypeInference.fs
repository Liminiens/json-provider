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
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        let log = Path.Combine(home, "jsoninference_log.txt")    
        fun (msg: string) -> 
            File.AppendAllLines(log, [msg])
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

    let inferType root (asm: ProvidedAssembly) ns =      
        let typens = ns //sprintf "%s.%s" ns (getUniqueProviderName())

        let getOrCreateTypeDefinition (generatedType: ProvidedTypeDefinition option) nameCreator =
            match generatedType with 
            | Some(genType) -> 
                genType
            | None ->
                let typeName = nameCreator()
                let genType = createType asm typens typeName
                asm.AddTypes([genType])               
                log <| sprintf "Root type name: %s" typeName
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

        let rootType =
            let typeName = getUniqueTypeName()                 
            log <| sprintf "Root type name: %s" typeName
            getOrCreateTypeDefinition None (fun _ -> typeName)

        let rec processToken token (generatedType: ProvidedTypeDefinition option) =
            match readToken token with
            | Property(jProperty) ->                 
                let name = initCap jProperty.Name
                log <| sprintf "Property name: %s" name
                let generatedType = 
                    getOrCreateTypeDefinition generatedType (fun _ -> name)
                log <| sprintf "Property type parent name: %s" generatedType.FullName
                let inferredType = processToken jProperty.First (Some(generatedType))
                let property = createProperty name inferredType 
                log <| sprintf "Property inferred type name: %s" inferredType.FullName
                generatedType.AddMember(property)
                generatedType.AsType()
            | Object(jObject) ->          
                let typeName =
                    getOrCreateNameFromParent jObject getUniqueTypeName                   
                log <| sprintf "Object type name: %s" typeName
                let generatedType = 
                    match generatedType with 
                    | None ->
                        rootType
                    | Some(_) ->
                        getOrCreateTypeDefinition None (fun _ -> typeName)
                    
                log <| sprintf "Object generatedTypeName name: %s" generatedType.FullName
                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)
                generatedType.AsType()
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
                let inferredType = processToken largestToken (Some(generatedType))
                log <| sprintf "Array inferredType name: %s" inferredType.FullName      
                arrayType inferredType
            | Value(value) ->
                match value with
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        processToken root None |> ignore
        rootType
