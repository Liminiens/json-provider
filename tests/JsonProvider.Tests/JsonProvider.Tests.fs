module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework

type TestTyp = JsonProvider<"""{"dat": 1 }""">

[<Test>]
let ``BasicTest`` () =
    //TestTyp.Value1
    ()

