module MiniCrawler

open System
open System.Net
open System.Text.RegularExpressions

let downloadPageAsync (url: string) = async {
    use client = new WebClient()
    let! html = client.AsyncDownloadString(Uri(url))
    return (url, html.Length)
}

let extractLinks (html: string) =
    let pattern = @"<a\s+href=[""'](http://[^""']*)[""']"
    Regex.Matches(html, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups[1].Value)
    |> Seq.distinct
    |> Seq.toList

let miniCrawler (url: string) =
    use client = new WebClient()
    let mainHtml = client.DownloadString(Uri(url))
    let links = extractLinks mainHtml
    
    links
    |> List.map downloadPageAsync
    |> Async.Parallel
    |> Async.RunSynchronously