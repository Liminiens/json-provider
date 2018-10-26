module JsonProviderTests


open FSharp.Liminiens.JsonProvider
open NUnit.Framework


type JsonGenerativeProviderTest = JsonGenerativeProvider<"""{ "data": [1,2] }""">

[<Test>]
let ``BasicTest`` () =
    let obj = Generative2()
    Assert.AreEqual(obj.Property1, 1)
    Assert.AreEqual(obj.Property2, 2)  

