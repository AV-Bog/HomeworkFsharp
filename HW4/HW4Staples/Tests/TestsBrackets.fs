module TestsBrackets

open HW4Staples.Brackets
open NUnit.Framework

[<TestFixture>]
type StaplesTests() =

    [<Test>]
    member this.``Valid simple brackets`` () =
        Assert.That(okStaples "()", Is.True)

    [<Test>]
    member this.``Valid nested mixed brackets`` () =
        Assert.That(okStaples "([{}])", Is.True)

    [<Test>]
    member this.``Invalid wrong order`` () =
        Assert.That(okStaples "([)]", Is.False)

    [<Test>]
    member this.``Invalid unclosed bracket`` () =
        Assert.That(okStaples "(", Is.False)

    [<Test>]
    member this.``Brackets with text inside`` () =
        Assert.That(okStaples "(sl[ekek{}wpffri]kd)", Is.True)

    [<Test>]
    member this.``Empty string is valid`` () =
        Assert.That(okStaples "", Is.True)
    
    [<Test>]
    member this.``Valid multiple sequential brackets ([][]){}`` () =
        Assert.That(okStaples "([][]){}", Is.True)
        
    [<Test>]
    member this.``Valid complex pattern with text ([][]kjsdflkjsdflkj){}`` () =
        Assert.That(okStaples "([][]kjsdflkjsdflkj){}", Is.True)
        
    [<Test>]
    member this.``Valid multiple nested sequences`` () =
        Assert.That(okStaples "{[()()][]}", Is.True)
        
    [<Test>]
    member this.``Invalid extra closing bracket`` () =
        Assert.That(okStaples "())", Is.False)
        
    [<Test>]
    member this.``Invalid extra opening bracket`` () =
        Assert.That(okStaples "(()", Is.False)
