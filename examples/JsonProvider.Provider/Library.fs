namespace JsonProvider.Provider

open FSharp.Data.JsonProvider

type ProviderExampleType = 
    JsonProvider<"""{ "Data": [{ "Test": 1, "Array": [1.3, 1] }] }""", "SuperRoot">
    