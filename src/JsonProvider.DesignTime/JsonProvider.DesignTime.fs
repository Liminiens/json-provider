module JsonProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open FSharp.Liminiens.JsonProvider
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Diagnostics

// Put any utility helpers here

[<TypeProvider>]
type JsonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("JsonProvider.DesignTime", "JsonProvider")])

    let providerTypeName = "JsonProvider"
    let ns = "FSharp.Liminiens.JsonProvider"
    let asm = Assembly.GetExecutingAssembly()

    do asm.Location |> Path.GetDirectoryName |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<Marker>.Assembly.GetName().Name = asm.GetName().Name)  

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let sample = args.[0] :?> string
        let sampleObject = JObject.Parse sample
        
        let ((sampleType, _), store) = TypeInference.inferType sampleObject.Root asm ns
        let sampleType = Option.get sampleType
        let staticPropertyType = createType asm store.Namespace typeName
        this.AddNamespace(store.Namespace, (store.GetTypes()) @ [staticPropertyType])

        let sampleProperty = 
            ProvidedProperty(
                propertyName = "Value", 
                propertyType = sampleType, 
                isStatic = true,
                getterCode = fun _ -> <@@ JsonConvert.DeserializeObject(sample, sampleType) @@>)
        sampleProperty.AddXmlDoc("Gets the sample data value")
        staticPropertyType.AddMember(sampleProperty)

        let parseMethod = 
            ProvidedMethod(
                methodName = "Parse", 
                parameters = [ProvidedParameter("input", typeof<string>)], 
                returnType = sampleType, 
                isStatic = true,
                invokeCode = 
                    fun args -> <@@ JsonConvert.DeserializeObject(sample, sampleType) @@>) 
        parseMethod.AddXmlDoc "Deserializes JSON input string"
        staticPropertyType.AddMember(parseMethod)

        staticPropertyType

    let staticParameters = 
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ] 
    
    let generatedType = 
        let providedType = createType asm ns providerTypeName
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
    do
        this.AddNamespace(ns, [generatedType])


[<TypeProviderAssembly>]
do ()
