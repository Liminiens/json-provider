namespace FSharp.Liminiens.JsonProvider

type TypeInferenceSettings = { RootTypeName: string }

module TypeInference =
    open System
    open System.Linq
    open Newtonsoft.Json.Linq
    open ProviderImplementation.ProvidedTypes
    open System.Globalization
    open System.Threading
    open System.Collections.Generic
  
    type JsonValue =
        | String
        | Float
        | Int
        | Boolean
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
            | JTokenType.Boolean ->
                Value(Boolean)
            | _ ->
                let tokenValue = jToken.Value<string>().ToLower(CultureInfo.InvariantCulture).Trim()
                if tokenValue = "false" || tokenValue = "true" then
                    Value(Boolean)
                else
                    Value(String)

    let (|IntType|LongType|DecimalType|BooleanType|StringType|ObjectType|MixedType|) (tokens: JsonTokenType list) = 

        let checkType predicate = 
            tokens |> List.forall predicate
        
        let isPrimitive = (function Value(_) -> true | _ -> false)

        let isInt = (function (Value(Int)) -> true | _ -> false)

        let isLong = (function (Value(Long)) -> true | _ -> false)

        let isFloat = (function (Value(Float)) -> true | _ -> false)

        let isBoolean = (function (Value(Boolean)) -> true | _ -> false)

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
            elif checkType isBoolean then
                BooleanType
            else
                StringType
        | false ->   
            if checkType isObject then
                ObjectType  
            else
                MixedType

    let inferType root (tpType: ProvidedTypeDefinition) (settings: TypeInferenceSettings)=
        Logging.log <| sprintf "Start type inference"

        let getUniqueTypeName =
            let store = new Dictionary<string, int ref>()
            fun name ->
                if store.ContainsKey(name) then
                   sprintf "%s%i" name (Interlocked.Increment(store.[name]))
                else
                   store.Add(name, ref 0)
                   name

        let createTypeDefinition typeName =
            let typeName = typeName |> getUniqueTypeName
            let genType = createType typeName           
            // Only root tpType you add to asm, all other types will be nested
            tpType.AddMember(genType)
            Logging.log <| sprintf "Generated type full name: %s" genType.FullName
            genType

        let tryGetParentNameOrDefault (jToken: JToken) defaultName =
            match Option.ofObj jToken.Parent with
            | Some(parent) ->
                match readToken parent with
                | Property(jProperty) ->
                    prettyName jProperty.Name
                | _ ->
                    defaultName
            | None ->
                defaultName

        let rootType =
            let typeName = settings.RootTypeName
            Logging.log <| sprintf "Root type name: %s" typeName
            createTypeDefinition typeName
        
        let rec processArrayToken (generatedType: ProvidedTypeDefinition) (jArray: JArray) =           
            let tokens =
                jArray 
                |> Seq.map readToken
                |> List.ofSeq
            
            let arrayType = 
                match tokens with
                | DecimalType ->
                    createArrayType typeof<decimal>
                | LongType ->
                    createArrayType typeof<int64>
                | IntType ->
                    createArrayType typeof<int32>
                | BooleanType ->      
                    createArrayType typeof<bool>
                | StringType ->
                    createArrayType typeof<string>
                | MixedType ->
                    typeof<JArray>               
                | ObjectType ->
                    let jObj =
                        jArray 
                        |> Seq.collect (fun jobj -> (jobj :?> JObject).Properties())
                        |> Seq.distinctBy (fun prop -> prop.Name)
                        |> List.ofSeq
                        |> JObject

                    processToken jObj (Some(generatedType))
                    |> createArrayType
            
            Logging.log <| sprintf "Array type name: %s" arrayType.FullName
            arrayType

        and processToken (token: JToken) (generatedType: ProvidedTypeDefinition option) =
            match readToken token with
            | Property(jProperty) ->
                let name = jProperty.Name
                Logging.log <| sprintf "Property name: %s" name
                
                let generatedType = Option.get generatedType
                Logging.log <| sprintf "Property parent type name: %s" generatedType.FullName

                let inferredType = processToken jProperty.First (Some(generatedType))

                let field, prop = createAutoProperty name (generatedType :> Type) inferredType
                
                Logging.log <| sprintf "Property type full name: %s" field.FieldType.FullName
                
                generatedType.AddMember(prop)
                generatedType.AddMember(field)
                
                Logging.log <| sprintf "Property declaring type full name: %s" field.DeclaringType.FullName

                generatedType :> Type

            | Object(jObject) ->
                let generatedType =
                    match generatedType with
                    | None ->
                        rootType
                    | Some(_) ->
                        let typeName = tryGetParentNameOrDefault jObject "ProvidedType"
                        Logging.log <| sprintf "Object type name: %s" typeName
                        createTypeDefinition typeName

                Logging.log <| sprintf "Object type full name: %s" generatedType.FullName

                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)

                generatedType :> Type
            | Array(jArray) ->
                Logging.log "Start array processing"
                let generatedType = Option.get generatedType
                processArrayToken generatedType jArray
            | Value(value) ->
                match value with
                | Boolean ->
                    typeof<bool>
                | Int ->
                    typeof<int32>
                | Long ->
                    typeof<int64>
                | String ->
                    typeof<string>
                | Float ->
                    typeof<decimal>

        processToken root None
