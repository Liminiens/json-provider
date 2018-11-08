module JsonProviderImplementation
#nowarn "0025"

open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Data.JsonProvider
open ProviderImplementation.ProvidedTypes

// Put any utility helpers here

[<TypeProvider>]
type JsonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("JsonProvider.DesignTime", "JsonProvider.Runtime")], addDefaultProbingLocation=true)

    let providerTypeName = "JsonProvider"
    let ns = "FSharp.Data.JsonProvider"
    let execAsm = Assembly.GetExecutingAssembly()

    do execAsm.Location |> Path.GetDirectoryName |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<Marker>.Assembly.GetName().Name = execAsm.GetName().Name)

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let sample = args.[0] :?> string
        let rootTypeName = args.[1] :?> string

        let sampleObject = Json.parse sample

        let asm = ProvidedAssembly()
        // Create root TP type specifing asm and ns
        let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let settings = { RootTypeName = rootTypeName }
        let sampleType = TypeInference.inferType sampleObject.Root tpType settings
        
        let sampleMethod =
            ProvidedMethod(
                methodName = "GetSampleJson",
                parameters = [],
                returnType = typeof<string>,
                isStatic = true,
                invokeCode =
                    fun args -> <@@ sample @@>)
        sampleMethod.AddXmlDoc("Returns json sample")
        tpType.AddMember(sampleMethod)

        let sampleValueMethod =
            ProvidedMethod(
                methodName = "GetSample",
                parameters = [],
                returnType = sampleType,
                isStatic = true,
                invokeCode =
                    fun args -> <@@ Json.deserialize sample sampleType @@>)
        sampleValueMethod.AddXmlDoc("Returns deserialized sample")
        tpType.AddMember(sampleValueMethod)

        let parseMethod =
            ProvidedMethod(
                methodName = "Parse",
                parameters = [ProvidedParameter("input", typeof<string>)],
                returnType = sampleType,
                isStatic = true,
                invokeCode =
                    fun args -> <@@ Json.deserialize (%%args.[0]: string) sampleType @@>)
        parseMethod.AddXmlDoc("Deserializes json input string to provided type")
        tpType.AddMember(parseMethod)

        asm.AddTypes([tpType])
        tpType

    let staticParameters =
        let sampleParameter = 
            ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "")
        sampleParameter.AddXmlDoc("Json sample string")
        let rootTypeNameParameter = 
            ProvidedStaticParameter("RootTypeName", typeof<string>, parameterDefaultValue = TypeInference.defaultRootTypeName)
        rootTypeNameParameter.AddXmlDoc("Type name for json object root")
        [ sampleParameter; rootTypeNameParameter ]

    let generatedType =
        let providedType = ProvidedTypeDefinition(execAsm, ns, providerTypeName, baseType = Some typeof<obj>, isErased = false)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
    do
        this.AddNamespace(ns, [generatedType])


[<TypeProviderAssembly>]
do ()
