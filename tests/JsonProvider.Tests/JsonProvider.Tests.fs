module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework
open Newtonsoft.Json.Linq

module Tests = 
    type ArrayIntType = JsonProvider<"""{ "Data": [1, 2, 3] }""">

    [<Test>]
    let ``Array int test`` () =
        let expected = [1; 2; 3]
        let data = ArrayIntType.GetSampleValue().Data

        Assert.AreEqual(typeof<int32[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)
        
    type ArrayDecimalType = JsonProvider<"""{ "Data": [1.2, 2, 3] }""">

    [<Test>]
    let ``Array decimal test`` () =
        let expected = [1.2M; 2M; 3M]
        let data = ArrayDecimalType.GetSampleValue().Data

        Assert.AreEqual(typeof<decimal[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)

    type ArrayLongType = JsonProvider<"""{ "Data": [100000000000, 2, 3] }""">

    [<Test>]
    let ``Array long test`` () =
        let expected = [100000000000L; 2L; 3L]
        let data = ArrayLongType.GetSampleValue().Data

        Assert.AreEqual(typeof<int64[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)
        
    type ArrayObjectType = JsonProvider<"""{ "Data": [{"Test": 1}, {"Test": 2}] }""">

    [<Test>]
    let ``Array simple object test`` () =
        let data = ArrayObjectType.GetSampleValue().Data

        Assert.AreEqual(1, data.[0].Test)
        Assert.AreEqual(2, data.[1].Test)

    
    type ArrayMixedType = JsonProvider<"""{ "Data": [{"Test": 1}, 2] }""">    
    
    [<Test>]
    let ``Array mixed test`` () =
        let data = ArrayMixedType.GetSampleValue().Data

        Assert.AreEqual(1, data.SelectToken("[0].Test").Value<int32>())
        Assert.AreEqual(2, data.[1].Value<int>())