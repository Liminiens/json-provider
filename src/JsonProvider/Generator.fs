namespace JsonProvider

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Newtonsoft.Json.Linq

module Generator =
    
    type JsonTokenType = 
        | Object
        | Property
        | Array
        | Float
        | String
    
    let createSampleType (jobj: JObject) = 
       seq {
            for property in jobj do
                ()
       }
    
