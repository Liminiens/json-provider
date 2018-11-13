module JsonProviderImplementation
#nowarn "0025"

open System.Linq
open System.IO
open System.Text
open System.Reflection
open Newtonsoft.Json.Linq
open FSharp.Core.CompilerServices
open FSharp.Data.JsonProvider
open ProviderImplementation.ProvidedTypes
open System

// Put any utility helpers here
module internal Sample = 
    open System.Net
    open TypeInference

    let (|RelativeFileResource|FileResource|WebResource|Json|) (sample: string, ctx: Context) =     
        let invalidPathChars = Path.GetInvalidPathChars()
        let isValidPath =
            sample 
            |> Seq.tryFind (fun ch -> Array.contains ch invalidPathChars)
            |> Option.isNone

        if sample.StartsWith("http") then
           WebResource
        elif sample.StartsWith("{") || sample.StartsWith("[") then
           Json
        elif isValidPath && File.Exists(ctx.GetRelativeFilePath sample) then
           RelativeFileResource
        elif isValidPath && File.Exists(sample) then
           FileResource
        else
           Json

    let load (encoding: Encoding) (sample: string) (ctx: Context) = 
        let sample = sample.Trim()
        let readFile file = 
            if File.Exists(file) then
                File.OpenRead(file) |> readStream encoding
            else
                invalidArg "sample" <| sprintf """Couldn't find file \"%s\" """ file
        match (sample, ctx) with
        | WebResource ->
            let response = WebRequest.Create(sample).GetResponse()
            response.GetResponseStream() |> readStream encoding
        | RelativeFileResource ->
            let file = ctx.GetRelativeFilePath sample
            readFile file
        | FileResource ->
            readFile sample
        | Json ->
            sample    
            
    let parse (sample: string): JToken =
        let sample = sample.Trim()
        let parseAsJObject str = JObject.Parse(str) :> JToken
        let parseAsJArray str = JArray.Parse(str) :> JToken
        let parseAsJValue str = JValue.Parse(str)

        match tryFirst sample [parseAsJObject; parseAsJArray; parseAsJValue] with
        | Ok(token) ->
            token
        | Error(message) ->
            failwith message
    
    let getDeserializeQuotation (token: JToken) sample sampleType = 
        match TypeInference.readToken token with
        | Value(Boolean(value)) ->
            <@@ value @@>
        | Value(Int(value)) ->
            <@@ value @@>
        | Value(Long(value)) ->
            <@@ value @@>
        | Value(Float(value)) ->
            <@@ value @@>
        | Value(String(value)) ->
            <@@ value @@>
        | _ ->
            <@@ Json.deserialize sample sampleType @@>
            
            
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
        let context = Context(this, config.ResolutionFolder)
        
        let encoding = Encoding.GetEncoding(args.[2] :?> string)
        let rootTypeName = args.[1] :?> string
        let sample = Sample.load encoding (args.[0] :?> string) context
        let tokenizedSample = Sample.parse sample

        let asm = ProvidedAssembly()
        // Create root TP type specifing asm and ns
        let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false)

        let settings = { RootTypeName = rootTypeName }
        let sampleType = TypeInference.inferType tokenizedSample.Root tpType settings
        
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
                    fun args -> Sample.getDeserializeQuotation tokenizedSample sample sampleType)
        sampleValueMethod.AddXmlDoc("Returns deserialized sample")
        tpType.AddMember(sampleValueMethod)

        let canBeParsed = 
            (not sampleType.IsValueType) && (sampleType.FullName <> "System.String")
        
        if canBeParsed then
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
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = ""); 
          ProvidedStaticParameter("RootTypeName", typeof<string>, parameterDefaultValue = TypeInference.defaultRootTypeName); 
          ProvidedStaticParameter("Encoding", typeof<string>, parameterDefaultValue = "UTF-8");]

    let summaryText = 
        """<summary>A json typed representation</summary>
           <param name='Sample'>Json sample, http url to json resource, relative or absolute path to a file</param>       
           <param name='RootTypeName'>The name to be used for the root type. Defaults to 'Root'.</param>
           <param name='Encoding'>Sample encoding, default is 'UTF-8'</param>"""

    let generatedType =
        let providedType = ProvidedTypeDefinition(execAsm, ns, providerTypeName, baseType = Some typeof<obj>, isErased = false)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType.AddXmlDoc summaryText
        providedType
    do
        this.AddNamespace(ns, [generatedType])


[<TypeProviderAssembly>]
do ()
