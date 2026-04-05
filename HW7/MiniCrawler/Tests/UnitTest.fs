// <copyright file="UnitTest.fs" author="bogdanovaarina">
// under MIT License
// </copyright>

module Tests

open NUnit.Framework
open MiniCrawler

[<TestFixture>]
type tests() =
    [<Test>]
    member this.``MiniCrawler_ReturnsArray``() =
        let results = miniCrawler "http://example.com"
        Assert.That(results, Is.Not.Null)
        Assert.That(results, Is.InstanceOf<(string * int)[]>())

    [<Test>]
    member this.``MiniCrawler_EachPageHasPositiveSize``() =
        let results = miniCrawler "http://example.com"

        for (url, size) in results do
            Assert.That(size, Is.GreaterThan(0))

    [<Test>]
    member this.``MiniCrawler_NoDuplicateUrls``() =
        let results = miniCrawler "http://example.com"
        let urls = results |> Array.map fst
        Assert.That(urls.Length, Is.EqualTo(urls |> Array.distinct |> Array.length))

    [<Test>]
    member this.``MiniCrawler_InvalidUrl_ThrowsException``() =
        Assert.Throws<System.Net.WebException>(fun () ->
            miniCrawler "http://this-site-does-not-exist-12345.com" |> ignore)
        |> ignore
