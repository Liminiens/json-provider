namespace FSharp.Liminiens.JsonProvider

open System

// Put any runtime constructs here
type Marker() =
  let dummy = ()


// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("JsonProvider.DesignTime.dll")>]
do ()
