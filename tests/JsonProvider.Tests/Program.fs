// Learn more about F# at http://fsharp.org

open System

open Liminiens.JsonProvider

type TestType = JsonProvider<"""{"id": 1 }""">

[<EntryPoint>]
let main argv =
    let a = TestType.Parse("")
    printfn "Hello World from F#!"
    0 // return an integer exit code
