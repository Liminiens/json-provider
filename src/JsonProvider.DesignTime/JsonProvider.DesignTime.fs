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

// Put any utility helpers here

[<TypeProvider>]
type JsonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("JsonProvider.DesignTime", "JsonProvider")]) 

    let providerTypeName = "JsonProvider"
    let ns = "FSharp.Liminiens.JsonProvider"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<Marker>.Assembly.GetName().Name = asm.GetName().Name)  

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let sample = args.[0] :?> string
        let sampleObject = JObject.Parse sample

        //let sampleType = listType typeof<string>
        let (sampleType, store) = TypeInference.inferType sampleObject.Root asm ns
        this.AddNamespace(store.Namespace, store.GetTypes())
        let staticPropertyType = createType asm ns typeName
        this.AddNamespace(store.Namespace, [staticPropertyType])
        //this.AddNamespace(ns, [staticPropertyType])   
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
                    fun args -> <@@ JsonConvert.DeserializeObject((%%args.[0] : string), sampleType) @@>) 
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
