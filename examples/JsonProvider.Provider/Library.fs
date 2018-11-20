namespace JsonProvider.Provider

open FSharp.Data.JsonProvider

type JsonStringType = 
    JsonProvider<"""{ "Data": [{ "Test": 1, "Array": [1.3, 1] }] }""", "SuperRoot">

type JsonFromFile = JsonProvider<"files/example.json">    

type JsonFromWeb = JsonProvider<"https://jsonplaceholder.typicode.com/comments">