#r @".\bin\Debug\net45\JsonProvider.DesignTime.dll"
open FSharp.Liminiens.JsonProvider
printf "123"
type Test = JsonProvider<"""{"data": [1,2,4] }""">