namespace JsonProvider

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<TypeProvider>]
type JsonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    
    let ns = "Liminiens.JsonProvider"    
    let asm = Assembly.GetExecutingAssembly()
    
    let sampleType = ProvidedTypeDefinition(asm, ns, "Data", baseType = Some typeof<obj>, isErased = false)

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let asm = ProvidedAssembly()
        let sample = (args.[0] :?> string) |> JObject.Parse

        let providedType = ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, isErased = false)

        let parseMethod = 
            ProvidedMethod(
                methodName = "Parse", 
                parameters = [ProvidedParameter("input", typeof<string>)], 
                returnType = sampleType.AsType(), 
                isStatic = true,
                invokeCode = fun args -> <@@ JsonConvert.DeserializeObject(%%args.[0], sampleType.AsType()) @@>) 
        parseMethod.AddXmlDoc "Deserializes JSON input string"
        providedType.AddMember parseMethod

        providedType
    
    let staticParameters = 
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ] 
    
    let generatedType = 
        let providedType = ProvidedTypeDefinition(asm, ns, "JsonProvider", baseType = Some typeof<obj>, isErased = false)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
        
    do
       this.AddNamespace(ns, [generatedType; sampleType])

[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()