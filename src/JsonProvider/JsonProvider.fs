namespace JsonProvider

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open Newtonsoft.Json.Linq

[<TypeProvider>]
type ComboGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "JsonProvider"
    let asm = Assembly.GetExecutingAssembly()

    let buildTypes (typeName: string) (args: obj[]) =
        let sample = (args.[0] :?> string) |> JObject.Parse
        for property in (sample :> JToken) do
            ()
        ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>)
    
    let parameters = 
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ] 
    
    let generatedType = 
        let providedType = ProvidedTypeDefinition(asm, ns, "JsonProvider", baseType = Some typeof<obj>)
        providedType.DefineStaticParameters(parameters, buildTypes)
        providedType
    do
       this.AddNamespace(ns, [generatedType])
    (*
    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)

        let meth = ProvidedMethod("StaticMethod", [], typeof<SomeRuntimeHelper>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<SomeRuntimeHelper>)))
        myType.AddMember(meth)
        asm.AddTypes [ myType ]

        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))
        t*)


[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()
