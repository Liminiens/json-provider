namespace FSharp.Liminiens.JsonProvider

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Diagnostics
open System

// Put any runtime constructs here
type Marker() =
  let dummy = ()

// Put any utility helpers here

module Json = 
    let deserialize (json: string) (typ: Type) = 
        JsonConvert.DeserializeObject(json, typ)
    let parse (sample: string) = 
        JObject.Parse sample

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("JsonProvider.DesignTime.dll")>]
do ()
