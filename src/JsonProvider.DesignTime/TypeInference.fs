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
            let message =  
                let time = DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss.fff")
                sprintf "[%s]: %s" time msg
            File.AppendAllLines(log,  [message])
        #else
        fun (str: string) -> ()
        #endif

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

    let inferType root (tpType: ProvidedTypeDefinition) =
        log <| sprintf "Start type inference"

        // IDE may call TPDTC multiple times (even for one use of TP)
        // so scope of uniquness should per IDE call (instead of shared across all)
        let getUniqueTypeName = getUniqueName "ProvidedType"

        let getOrCreateTypeDefinition (generatedType: ProvidedTypeDefinition option) nameCreator =
            match generatedType with
            | Some(genType) ->
                genType
            | None ->
                let typeName = nameCreator()
                let genType = createType typeName           
                // Add default constructor (Otherwise Json.NET deserializer will noy be able to create an instance)
                genType.AddMember <| ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>)
                // Only root tpType you add to asm, all other types will be nested
                tpType.AddMember(genType)
                log <| sprintf "Generated type full name: %s" genType.FullName
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
                log <| sprintf "Field name: %s" name

                let generatedType =
                    getOrCreateTypeDefinition generatedType (fun _ -> name)
                log <| sprintf "Field parent type name: %s" generatedType.FullName

                let inferredType = processToken jProperty.First (Some(generatedType))
                let field = createField name inferredType

                log <| sprintf "Field inferred type name: %s" inferredType.FullName
                log <| sprintf "Field type full name: %s" field.FieldType.FullName

                generatedType.AddMember(field)
                
                log <| sprintf "Field declaring type full name: %s" field.DeclaringType.FullName

                generatedType :> Type

            | Object(jObject) ->

                let generatedType =
                    match generatedType with
                    | None ->
                        rootType
                    | Some(_) ->
                        let typeName =
                            getOrCreateNameFromParent jObject getUniqueTypeName
                        log <| sprintf "Object type name: %s" typeName
                        getOrCreateTypeDefinition None (fun _ -> typeName)

                log <| sprintf "Object generatedTypeName name: %s" generatedType.FullName

                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)

                generatedType :> Type
            | Array(jArray) ->
                let typeName =
                    getOrCreateNameFromParent jArray getUniqueTypeName
                log <| sprintf "Array type name: %s" typeName

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
                log <| sprintf "Array element inferred type name: %s" inferredType.FullName

                let arrayType = makeArrayType inferredType
                log <| sprintf "Array type name: %s" arrayType.FullName

                arrayType
            | Value(value) ->
                match value with
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        processToken root None |> ignore
        rootType :> Type
