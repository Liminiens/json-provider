namespace JsonProvider.Provider

open FSharp.Data.JsonProvider

type JsonType = JsonProvider<"""{ "Data": [{ "Test": 1, "Array": [1.3, 1] }] }""", "SuperRoot">

type JsonNullableType = JsonProvider<"""{ "Data": [null, 1, 2] }""", NullableValueTypes = true>

type JsonFromFile = JsonProvider<"files/example.json">    

type JsonFromWeb = JsonProvider<"https://jsonplaceholder.typicode.com/comments">