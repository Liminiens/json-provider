namespace FSharp.Data.JsonProvider

type internal TypeInferenceSettings = { RootTypeName: string; NullableTypes: bool }

module internal TypeInference =
    open System
    open System.Linq
    open Newtonsoft.Json.Linq
    open ProviderImplementation.ProvidedTypes
    open System.Globalization
    open System.Threading
    open System.Collections.Generic
    open TypeProviderHelpers
    open Helpers
  
    type JsonValue =
        | String of string
        | Float of decimal
        | Int of int
        | Boolean of bool
        | Long of int64
        | Null

    type JsonTokenType =
        | Object of JObject
        | Property of JProperty
        | Array of JArray
        | Value of JsonValue        
    
    let defaultRootTypeName = "Root"

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
                    Value(Long(value))
                else
                    Value(Int(int value))
            | JTokenType.Float ->
                Value(Float(jToken.Value<decimal>()))
            | JTokenType.Boolean ->
                Value(Boolean(jToken.Value<bool>()))
            | JTokenType.Undefined
            | JTokenType.Null ->
                Value(Null)
            | _ ->
                let strValue = jToken.Value<string>()
                match stringToBool strValue with
                | BoolValue(value) ->
                    Value(Boolean(value))
                | NotBoolValue ->
                    Value(String(strValue))
                | NullValue ->
                    Value(Null)

    let (|IntType|LongType|DecimalType|BooleanType|StringType|ObjectType|MixedType|) (tokens: JsonTokenType list) = 
        let tokens = tokens |> List.filter (function Value(Null) -> false | _ -> true)

        let checkType predicate = 
            tokens |> List.forall predicate

        let isPrimitive = 
            function Value(_) -> true | _ -> false

        let isInt = 
            function (Value(Int(_))) -> true | _ -> false

        let isLong = 
            function (Value(Long(_))) -> true | _ -> false

        let isFloat = 
            function (Value(Float(_))) -> true | _ -> false

        let isBoolean = 
            function (Value(Boolean(_))) -> true | _ -> false

        let isFloatOrInt value = 
            match value with 
            | Value(valueType) ->
                match valueType with 
                | Float(_) 
                | Int(_) 
                | Long(_) -> 
                    true 
                | _ -> 
                    false
            | _ ->
                false
        
        let isInt32OrInt64 value = 
            match value with 
            | Value(valueType) ->
                match valueType with 
                | Int(_) 
                | Long(_) -> 
                    true 
                | _ -> 
                    false
            | _ ->
                false

        let isObject = (function (Object(_)) -> true | _ -> false)
        
        if tokens.Length = 0 then
            StringType
        else
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
    
    let getParentNameOrDefault (jToken: JToken) defaultName =
        match Option.ofObj jToken.Parent with
        | Some(parent) ->
            match (readToken parent) with
            | Property(jProperty) ->
                prettyName jProperty.Name
            | _ ->
                defaultName
        | None ->
            defaultName

    let inferType root (tpType: ProvidedTypeDefinition) (settings: TypeInferenceSettings)=
        Logging.log <| sprintf "Start type inference"

        let inline preprocessType typ =
            if settings.NullableTypes then
                createNullableType typ
            else
                typ

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
            tpType.AddMember(genType)
            Logging.log <| sprintf "Generated type full name: %s" genType.FullName
            genType

        let createRootType() =
            let typeName = 
                match String.IsNullOrWhiteSpace(settings.RootTypeName) with
                | true ->
                    defaultRootTypeName
                | false ->
                    settings.RootTypeName
            Logging.log <| sprintf "Root type name: %s" typeName
            createTypeDefinition typeName

        let rec processToken (token: JToken) (generatedType: ProvidedTypeDefinition option) =
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
                        createRootType()
                    | Some(_) ->
                        let typeName = getParentNameOrDefault jObject "ProvidedType"
                        Logging.log <| sprintf "Object type name: %s" typeName
                        createTypeDefinition typeName
                Logging.log <| sprintf "Object type full name: %s" generatedType.FullName
                
                jObject
                |> Seq.iter (fun prop -> processToken prop (Some(generatedType)) |> ignore)

                generatedType :> Type
            | Array(jArray) ->                     
                processArrayToken jArray generatedType
            | Value(value) ->
                match value with
                | Boolean(_) ->
                    typeof<bool> |> preprocessType
                | Int(_) ->
                    typeof<int32> |> preprocessType
                | Long(_) ->
                    typeof<int64> |> preprocessType
                | Float(_) ->
                    typeof<decimal> |> preprocessType
                | String(_) | Null ->
                    typeof<string>
        and processArrayToken (jArray: JArray) generatedType =                     
            let tokens =
                jArray 
                |> Seq.map readToken
                |> List.ofSeq
            
            match tokens with
            | DecimalType ->
                typeof<decimal> |> preprocessType |> createArrayType 
            | LongType ->
                typeof<int64> |> preprocessType |> createArrayType
            | IntType ->
                typeof<int32> |> preprocessType |> createArrayType
            | BooleanType ->      
                typeof<bool> |> preprocessType |> createArrayType
            | StringType ->
                typeof<string> |> createArrayType
            | MixedType ->
                typeof<JArray>               
            | ObjectType ->
                let jObj =
                    jArray 
                    |> Seq.collect (fun jobj -> (jobj :?> JObject).Properties())
                    |> Seq.distinctBy (fun prop -> prop.Name)
                    |> List.ofSeq
                    |> JObject

                //stub property so we infer type name later
                let name = getParentNameOrDefault jArray "ProvidedArray"
                let parent = JProperty(name, jObj)
                processToken (parent.First) generatedType |> createArrayType

        processToken root None
