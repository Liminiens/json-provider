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
                yield ProvidedProperty("1", typeof<string>, getterCode = (fun _ -> <@@ "Hi" @@>))
       }
    
