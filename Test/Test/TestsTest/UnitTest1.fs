module Test.TestsTest

open NUnit.Framework
open task1

[<TestFixture>]
type tests () =
    [<Test>]
    member this.``HashTable_AddMultipleKeys_ContainsReturnsCorrectly`` () =
        let hashFunc (s: string) = s.GetHashCode()
        let table = HashTable<string, int>(hashFunc, 10)
        
        table.Add("один", 1)
        table.Add("два", 2)
        table.Add("три", 3)
        
        Assert.That(table.Contains("один"), Is.True)
        Assert.That(table.Contains("два"), Is.True)
        Assert.That(table.Contains("три"), Is.True)
        Assert.That(table.Contains("четыре"), Is.False)
        Assert.That(table.Count, Is.EqualTo(3))
    
    [<Test>]
    member this.``HashTable_AddDuplicateKey_UpdatesValue`` () =
        let hashFunc (s: string) = s.GetHashCode()
        let table = HashTable<string, int>(hashFunc, 10)
        
        table.Add("ключ", 1)
        Assert.That(table.Count, Is.EqualTo(1))
        
        table.Add("ключ", 100)
        Assert.That(table.Count, Is.EqualTo(1))
        
    [<Test>]
    member this.``buildKvadrat_WithNEqual4_ReturnsCorrectPattern`` () =
        let expected = ["****"; "*  *"; "*  *"; "****"]
        let result = buildKvadrat 4 |> Seq.toList
        Assert.That(result, Is.EqualTo(expected))
        Assert.That(result.Length, Is.EqualTo(4))

    [<Test>]
    member this.``buildKvadrat_WithNEqual1_ReturnsSingleStar`` () =
        let expected = ["*"]
        let result = buildKvadrat 1 |> Seq.toList
        Assert.That(result, Is.EqualTo(expected))
        Assert.That(result.Length, Is.EqualTo(1))
    
    [<Test>]
    member this.``buildKvadrat_WithNEqual2_ReturnsFullSquare`` () =
        let expected = ["**"; "**"]
        let result = buildKvadrat 2 |> Seq.toList
        Assert.That(result, Is.EqualTo(expected))
        Assert.That(result.Length, Is.EqualTo(2))
    
    [<Test>]
    member this.``FindMin_WithPositiveNumbers_ReturnsMinimum`` () =
        let result = findMin [5; 2; 8; 1; 9; 4]
        Assert.That(result, Is.EqualTo(Some 1))
    
    [<Test>]
    member this.``FindMin_WithNegativeNumbers_ReturnsMinimum`` () =
        let result = findMin [-5; -2; -8; -1; -10; -3]
        Assert.That(result, Is.EqualTo(Some -10))
    
    [<Test>]
    member this.``FindMin_WithEmptyList_ReturnsNone`` () =
        let result = findMin []
        Assert.That(result, Is.EqualTo(None))
    
    [<Test>]
    member this.``FindMin_WithSingleElement_ReturnsThatElement`` () =
        let result = findMin [42]
        Assert.That(result, Is.EqualTo(Some 42))