module JsonProviderImplementation
#nowarn "0025"

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open FSharp.Core.CompilerServices
open FSharp.Liminiens.JsonProvider
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open System.Diagnostics

// Put any utility helpers here

[<TypeProvider>]
type JsonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("JsonProvider.DesignTime", "JsonProvider.Runtime")], addDefaultProbingLocation=true)

    let providerTypeName = "JsonProvider"
    let ns = "FSharp.Liminiens.JsonProvider"
    let execAsm = Assembly.GetExecutingAssembly()

    do execAsm.Location |> Path.GetDirectoryName |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<Marker>.Assembly.GetName().Name = execAsm.GetName().Name)

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let sample = args.[0] :?> string
        let sampleObject = Json.parse sample

        let asm = ProvidedAssembly()
        // Create root TP type specifing asm and ns
        let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let sampleType = TypeInference.inferType sampleObject.Root tpType
        
        let sampleField = ProvidedField.Literal("SampleJson", sampleType, sample)
        tpType.AddMember(sampleField)
        
        let parseMethod =
            ProvidedMethod(
                methodName = "GetSampleValue",
                parameters = [],
                returnType = sampleType,
                isStatic = true,
                invokeCode =
                    fun args -> <@@ Json.deserialize sample sampleType @@>)
        parseMethod.AddXmlDoc("Gets the sample data value")
        tpType.AddMember(parseMethod)

        let parseMethod =
            ProvidedMethod(
                methodName = "Parse",
                parameters = [ProvidedParameter("input", typeof<string>)],
                returnType = sampleType,
                isStatic = true,
                invokeCode =
                    fun args -> <@@ Json.deserialize (%%args.[0]: string) sampleType @@>)
        parseMethod.AddXmlDoc("Deserializes JSON input string")
        tpType.AddMember(parseMethod)

        asm.AddTypes([tpType])
        tpType

    let staticParameters =
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ]

    let generatedType =
        let providedType = ProvidedTypeDefinition(execAsm, ns, providerTypeName, baseType = Some typeof<obj>, isErased = false)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
    do
        this.AddNamespace(ns, [generatedType])


[<TypeProviderAssembly>]
do ()
