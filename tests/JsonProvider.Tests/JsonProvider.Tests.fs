module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework

type TestTyp = JsonProvider<"""{"dat": "6" }""">

[<Test>]
let ``BasicTest`` () =
    //TestTyp.Value1
    ()

