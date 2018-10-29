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
        | Int
        | Long

    type JsonTokenType =
        | Object of JObject
        | Property of JProperty
        | Array of JArray
        | Value of JsonValue         

    let readToken =
        let minValue = int64 Int32.MinValue
        let maxValue = int64 Int32.MaxValue

        fun (jToken: JToken) ->
            match jToken.Type with
            | JTokenType.Object ->
                Object(jToken :?> JObject)
            | JTokenType.Property ->
                Property(jToken :?> JProperty)
            | JTokenType.Array ->
                Array(jToken :?> JArray)
            | JTokenType.Integer ->
                let value = jToken.Value<int64>()
                if value > maxValue || value < minValue then
                   Value(Long)
                else
                   Value(Int)
            | JTokenType.Float ->
                Value(Float)
            | _ ->
                Value(String)

    let getUniqueName prefix =
        let mutable current = 0
        fun () ->
            current <- current + 1
            sprintf "%s%i" prefix current

    let (|IntType|LongType|DecimalType|StringType|ObjectType|MixedType|) (tokens: JsonTokenType list) = 
        let checkType predicate = 
            tokens |> List.forall predicate
        
        let isPrimitive = (function Value(_) -> true | _ -> false)

        let isInt = (function (Value(Int)) -> true | _ -> false)

        let isLong = (function (Value(Long)) -> true | _ -> false)

        let isFloat = (function (Value(Float)) -> true | _ -> false)

        let isFloatOrInt value = 
            match value with 
            | Value(valueType) ->
                 match valueType with 
                 | Float 
                 | Int 
                 | Long -> 
                    true 
                 | _ -> 
                    false
            | _ ->
                false
        
        let isInt32OrInt64 value = 
            match value with 
            | Value(valueType) ->
                 match valueType with 
                 | Int 
                 | Long -> 
                    true 
                 | _ -> 
                    false
            | _ ->
                false

        let isObject = (function (Object(_)) -> true | _ -> false)

        match checkType isPrimitive with
        | true ->
            if checkType isFloat then
                DecimalType
            elif checkType isLong then
                LongType
            elif checkType isInt then
                IntType
            elif checkType isInt32OrInt64 then
                LongType
            elif checkType isFloatOrInt then
                DecimalType
            else
                StringType
        | false ->   
            if checkType isObject then
                ObjectType  
            else
                MixedType


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
        
        let rec processArrayToken (generatedType: ProvidedTypeDefinition) (jArray: JArray) =
            let tokens =
                jArray 
                |> Seq.map readToken
                |> List.ofSeq
            
            let arrayType = 
                match tokens with
                | DecimalType ->
                    makeArrayType typeof<decimal>
                | LongType ->
                    makeArrayType typeof<int64>
                | IntType ->
                    makeArrayType typeof<int32>
                | StringType ->
                    makeArrayType typeof<string>
                | MixedType ->
                    typeof<JArray>               
                | ObjectType ->
                    (*let allProperties = 
                        tokens |> Seq.distinctBy (fun c -> c)*)
                               
                    let largestToken = jArray |> Seq.maxBy (fun el -> el.Count())

                    let (inferredType: Type) = processToken largestToken (Some(generatedType))
                    log <| sprintf "Array object element inferred type name: %s" inferredType.FullName

                    makeArrayType inferredType
            
            log <| sprintf "Array type name: %s" arrayType.FullName
            arrayType

        and processToken token (generatedType: ProvidedTypeDefinition option) =
            match readToken token with
            | Property(jProperty) ->
                let name = initCap jProperty.Name
                log <| sprintf "Field name: %s" name

                let generatedType =
                    getOrCreateTypeDefinition generatedType (fun _ -> name)
                log <| sprintf "Field parent type name: %s" generatedType.FullName

                let inferredType = processToken jProperty.First (Some(generatedType))
                let field = createField name inferredType
                
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

                log <| sprintf "Object type full name: %s" generatedType.FullName

                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)

                generatedType :> Type
            | Array(jArray) ->
                let generatedType = Option.get generatedType
                processArrayToken generatedType jArray
            | Value(value) ->
                match value with
                | Int ->
                    typeof<int32>
                | Long ->
                    typeof<int64>
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        processToken root None |> ignore
        rootType :> Type
