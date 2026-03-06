module Tests

open HW4Staples.Staples
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