namespace FSharp.Data.JsonProvider

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

// Put any runtime constructs here
type ``Asm marker``() =
  let dummy = ()

// Put any utility helpers here
module Json = 
    let inline deserialize (json: string) (typ: Type) = 
        let settings = new JsonSerializerSettings()
        //https://github.com/JamesNK/Newtonsoft.Json/issues/862
        settings.DateParseHandling <- DateParseHandling.None
        JsonConvert.DeserializeObject(json, typ, settings)

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("JsonProvider.DesignTime.dll")>]
do ()
