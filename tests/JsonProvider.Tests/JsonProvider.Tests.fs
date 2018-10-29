module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework


module Tests = 

    type ArrayIntType = JsonProvider<"""{ "Data": [1, 2, 3] }""">

    [<Test>]
    let ``Array int test`` () =
        let expected = [1; 2; 3]
        CollectionAssert.AreEquivalent(expected, ArrayIntType.Value.Data)
        
    type ArrayDecimalType = JsonProvider<"""{ "Data": [1.2, 2, 3] }""">

    [<Test>]
    let ``Array decimal test`` () =
        let expected = [1.2M; 2M; 3M]
        CollectionAssert.AreEquivalent(expected, ArrayDecimalType.Value.Data)

    type ArrayLongType = JsonProvider<"""{ "Data": [100000000000, 2, 3] }""">

    [<Test>]
    let ``Array long test`` () =
        let expected = [100000000000L; 2L; 3L]
        CollectionAssert.AreEquivalent(expected, ArrayLongType.Value.Data)
        
    type ArrayObjectType = JsonProvider<"""{ "Data": [{"Test": 1}, {"Test": 2}] }""">

    [<Test>]
    let ``Array simple object test`` () =
        let data = ArrayObjectType.Value.Data
        Assert.AreEqual(1, data.[0].Test)
        Assert.AreEqual(2, data.[1].Test)

    
    type ArrayMixedType = JsonProvider<"""{ "Data": [{"Test": 1}, 2] }""">    
    
    [<Test>]
    let ``Array mixed test`` () =
        let data = ArrayMixedType.Value.Data
        Assert.AreEqual(1, data.SelectToken("[*].Test").Value<int32>())
        Assert.AreEqual(2, data.[1].Value<int>())