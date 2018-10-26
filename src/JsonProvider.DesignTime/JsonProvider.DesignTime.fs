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
type BasicGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("JsonProvider.DesignTime", "JsonProvider")])

    let providerTypeName = "JsonGenerativeProvider"
    let ns = "FSharp.Liminiens.JsonProvider"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let sample = args.[0] :?> string
        let sampleObject = JObject.Parse sample

        let sampleType = TypeInference.inferType sampleObject.Root asm ns //todo

        let staticPropertyType = createType asm ns typeName

        let parseMethod = 
            ProvidedMethod(
                methodName = "Parse", 
                parameters = [ProvidedParameter("input", typeof<string>)], 
                returnType = sampleType, 
                isStatic = true,
                invokeCode = 
                    fun args -> <@@ JsonConvert.DeserializeObject((%%args.[0] : string), sampleType) @@>) 
        let sampleValue = JsonConvert.DeserializeObject(sample, sampleType)
        let prop = 
            ProvidedProperty(
                propertyName = "Sample", 
                propertyType = sampleType, 
                getterCode = fun args -> <@@ sampleValue @@>)
        prop.AddXmlDoc(sprintf @"Gets the sample data value")
        parseMethod.AddXmlDoc "Deserializes JSON input string"
        staticPropertyType.AddMember parseMethod

        staticPropertyType

    let staticParameters = 
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ] 
    
    let generatedType = 
        let providedType = ProvidedTypeDefinition(asm, ns, providerTypeName, baseType = Some typeof<obj>, isErased = false)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
    do
        this.AddNamespace(ns, [generatedType])


[<TypeProviderAssembly>]
do ()
