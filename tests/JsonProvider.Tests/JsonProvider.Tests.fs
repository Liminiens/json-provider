module JsonProviderTests

open FSharp.Liminiens.JsonProvider
open NUnit.Framework
open Newtonsoft.Json.Linq

module ArrayTests = 
    type ArrayIntType = JsonProvider<"""{ "Data": [1, 2, 3] }""">

    [<Test>]
    let ``Array int test`` () =
        let expected = [1; 2; 3]
        let data = ArrayIntType.GetSample().Data

        Assert.AreEqual(typeof<int32[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)
        
    type ArrayDecimalType = JsonProvider<"""{ "Data": [1.2, 2, 3] }""">

    [<Test>]
    let ``Array decimal test`` () =
        let expected = [1.2M; 2M; 3M]
        let data = ArrayDecimalType.GetSample().Data

        Assert.AreEqual(typeof<decimal[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)

    type ArrayLongType = JsonProvider<"""{ "Data": [100000000000, 2, 3] }""">

    [<Test>]
    let ``Array long test`` () =
        let expected = [100000000000L; 2L; 3L]
        let data = ArrayLongType.GetSample().Data

        Assert.AreEqual(typeof<int64[]>, data.GetType())
        CollectionAssert.AreEquivalent(expected, data)
        
    type ArrayObjectType = JsonProvider<"""{ "Data": [{"Test": 1}, {"Test": 2}] }""">

    [<Test>]
    let ``Array simple object test`` () =
        let data = ArrayObjectType.GetSample().Data

        Assert.AreEqual(1, data.[0].Test)
        Assert.AreEqual(2, data.[1].Test)

    
    type ArrayMixedType = JsonProvider<"""{ "Data": [{"Test": 1}, 2] }""">    
    
    [<Test>]
    let ``Array mixed test`` () =
        let data = ArrayMixedType.GetSample().Data

        Assert.AreEqual(1, data.SelectToken("[0].Test").Value<int32>())
        Assert.AreEqual(2, data.[1].Value<int>())
        
    type ArrayBoolType = JsonProvider<"""{ "Data": ["true", false] }""">  

    [<Test>]
    let ``Array bool test`` () =
        let data = ArrayBoolType.GetSample().Data
        
        Assert.True(data.[0])
        Assert.False(data.[1])

module ObjectTests = 
    
    type NullType = JsonProvider<"""{"Data": null}""">

    [<Test>]
    let ``Property null test`` () =
        let data = NullType.GetSample().Data
        
        Assert.IsNull(data)
    
    type SameNameDifferentCaseType = JsonProvider<"""{"data": 1, "Data ": 2, "dAtA": 3, "DaTa": 4}""">
   
    [<Test>]
    let ``Same name different case test`` () =
        let sample = SameNameDifferentCaseType.Parse("""{"data": 10, "Data ": 11, "dAtA": 12, "DaTa": 13}""")

        Assert.AreEqual(10, sample.Data)
        Assert.AreEqual(11, sample.Data1)
        Assert.AreEqual(12, sample.DAtA)
        Assert.AreEqual(13, sample.DaTa)