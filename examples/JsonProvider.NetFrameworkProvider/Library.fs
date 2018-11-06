namespace JsonProvider.NetFrameworkProvider

open FSharp.Liminiens.JsonProvider

type ProviderExampleType = 
    JsonProvider<"""{ "Data": [{ "Test": 1, "Array": [1.3, 1] }] }""", "SuperRoot">
