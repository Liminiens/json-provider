module JsonProviderTests

open FSharp.Data.JsonProvider
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
    
    type ArraySampleObjectType = JsonProvider<"""[{"Test": 1}, {"Test": 2}]""", "Array">  

    [<Test>]
    let ``Array object sample test`` () =
        let sample = ArraySampleObjectType.GetSample()
        
        Assert.AreEqual(1, sample.[0].Test)
        Assert.AreEqual(2, sample.[1].Test)

    type ArraySampleIntType = JsonProvider<"""[1, 2]"""> 

    [<Test>]
    let ``Array int sample test`` () =
        let sample = ArraySampleIntType.GetSample()
        
        Assert.AreEqual(1, sample.[0])
        Assert.AreEqual(2, sample.[1])
    
    type PlainObjectArrayType = JsonProvider<"""[{"Data": 1}]"""> 

    [<Test>]
    let ``Plain object array test`` () =
        let sample = PlainObjectArrayType.GetSample()
        
        Assert.AreEqual(1, sample.[0].Data)
        
    type ArrayInPropertyTestType = JsonProvider<"""[{"Data": 1}]"""> 

    [<Test>]
    let ``Type name of array in proprty test`` () =
        let sample = ArrayInPropertyTestType.GetSample()
        
        Assert.AreEqual(1, sample.[0].Data)

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

module MiscTests = 
    type RootNameCheckType = JsonProvider<"{}", "Something">

    [<Test>]
    let ``Root name setting test`` () = 
        RootNameCheckType.Something() |> ignore
        Assert.True(true)

    type RootNameEmptyType = JsonProvider<"{}", "">

    [<Test>]
    let ``Root name empty test`` () = 
        RootNameEmptyType.Root() |> ignore
        Assert.True(true)

    type RootNameWhitespaceType = JsonProvider<"{}", " ">

    [<Test>]
    let ``Root name whitespace test`` () = 
        RootNameWhitespaceType.Root() |> ignore
        Assert.True(true)
           
    type StrangeBoolType = JsonProvider<"""["fAlSe", false, "TrUE"]""">

    [<Test>]
    let ``Strange bool parsed correctly`` () = 
        let data = StrangeBoolType.GetSample()

        Assert.False(data.[0])
        Assert.False(data.[1])
        Assert.True(data.[2])

        let parsed = StrangeBoolType.Parse("""["TruE", false, "TRUE"]""")

        Assert.True(parsed.[0])
        Assert.False(parsed.[1])
        Assert.True(parsed.[2])
    
    type CaseTestType = JsonProvider<"""{"11__)123 data-test_value space": 10}""">

    [<Test>]
    let ``Symbols and property case test`` () = 
        let data = CaseTestType.GetSample()

        Assert.AreEqual(10, data.DataTestValueSpace)

module JsonValueTests =
    type BoolType = JsonProvider<"false">
    type IntType = JsonProvider<"123">
    type StringType = JsonProvider< """ "Test" """>

    [<Test>]
    let ``Test value parser`` () = 
        let boolValue = BoolType.GetSample()
        let intValue = IntType.GetSample()
        let stringValue = StringType.GetSample()

        Assert.AreEqual(123, intValue)
        Assert.AreEqual("Test", stringValue)
        Assert.IsFalse(boolValue)

module JsonNetTests = 
    type DateCheckType = JsonProvider<"""{ "x": "2016-03-31T07:02:00+07:00" }""">

    [<Test>]
    let ``Date parsed correctly by Json.NET`` () = 
        let value = DateCheckType.GetSample().X
        
        Assert.AreEqual("2016-03-31T07:02:00+07:00", value)

module HttpTests = 
    type HttpType = JsonProvider<"""https://jsonplaceholder.typicode.com/posts/1""">

    [<Test>]
    let ``Http resource loads`` () = 
        let data = HttpType.GetSample()
        
        Assert.AreEqual(1, data.UserId)
        Assert.AreEqual(1, data.Id)
        Assert.IsNotNull(data.Body)
        Assert.IsNotNull(data.Title)

module FileTests = 
    type RelativeFileTestType = JsonProvider<"""files\example.json""">

    [<Test>]
    let ``File resource loads`` () = 
        let data = RelativeFileTestType.GetSample()

        Assert.AreEqual(1, data.UserId)
        Assert.AreEqual(1, data.Id)
        Assert.AreEqual("Body", data.Body)
        Assert.AreEqual("Title", data.Title)
        
    type FileAltCharTestType = JsonProvider<"""files/example.json""">

    [<Test>]
    let ``File with alt char path loads`` () = 
        let data = FileAltCharTestType.GetSample()

        Assert.AreEqual(1, data.UserId)
        Assert.AreEqual(1, data.Id)
        Assert.AreEqual("Body", data.Body)
        Assert.AreEqual("Title", data.Title)
        
    type RelativeFileTestBelowType = JsonProvider<"""../JsonProvider.Tests.Templates/example_embed.json""">

    [<Test>]
    let ``Can use folders below resolution root`` () = 
        let data = RelativeFileTestBelowType.GetSample()

        Assert.AreEqual(2, data.UserId)
        Assert.AreEqual(2, data.Id)
        Assert.AreEqual("Body", data.Body)
        Assert.AreEqual("Title", data.Title)

module EmbeddedResources = 
   type EmbedTestType = JsonProvider<EmbeddedResource = "JsonProvider.Tests.Templates, example_embed.json">

   [<Test>]
    let ``Can read from embeded resource`` () = 
        let data = EmbedTestType.GetSample()

        Assert.AreEqual(2, data.UserId)
        Assert.AreEqual(2, data.Id)
        Assert.AreEqual("Body", data.Body)
        Assert.AreEqual("Title", data.Title)