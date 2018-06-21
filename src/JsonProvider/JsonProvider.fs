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

    let buildStaticParameters (typeName: string) (args: obj[]) =
        let asm = ProvidedAssembly()
        let providedType = ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>)
        let sample = (args.[0] :?> string) |> JObject.Parse
        let parseMethod = 
            ProvidedMethod(
                methodName = "Parse", 
                parameters = [ProvidedParameter("input", typeof<string>)], 
                returnType = providedType.AsType(), 
                isStatic = true,
                invokeCode = fun args -> <@@ JsonConvert.DeserializeObject(%%args.[0], providedType.AsType()) @@>) 

        parseMethod.AddXmlDoc "Deserializes JSON input string"

        providedType.AddMember parseMethod
        providedType
    
    let staticParameters = 
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ] 
    
    let generatedType = 
        let providedType = ProvidedTypeDefinition(asm, ns, "JsonProvider", baseType = Some typeof<obj>)
        providedType.DefineStaticParameters(staticParameters, buildStaticParameters)
        providedType
        
    do
       this.AddNamespace(ns, [generatedType])

(*
type SomeRuntimeHelper() = 
    static member Help() = "help"

[<AllowNullLiteral>]
type SomeRuntimeHelper2() = 
    static member Help() = "help"

[<TypeProvider>]
type public ComboGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "ComboProvider"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

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
        t
    do
        this.AddNamespace(ns, [myParamType])
*)

[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()