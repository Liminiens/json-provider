module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework

type TestTyp = JsonProvider<"""{ "Data": [1, 3, 4, 5] }""">

[<Test>]
let ``BasicTest`` () =
    printfn "%A" TestTyp.Value
    ()

