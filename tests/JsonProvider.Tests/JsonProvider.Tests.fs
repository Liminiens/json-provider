module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework

type TestTyp = JsonProvider<"""{"dat": 1 }""">

[<Test>]
let ``BasicTest`` () =
    printfn "%A" TestTyp.Value
    printfn "%A" (TestTyp.Parse """{"dat": 2 }""")
    ()

