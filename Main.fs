namespace JsonToolboxWebApp

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open FSharp.Formatting.Markdown
open WebSharper.UI.Server
open System.IO
type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /about">] About

module Templating =
    open WebSharper.UI.Html

    // Compute a menubar where the menu item for the given endpoint is active
    let MenuBar (ctx: Context<EndPoint>) endpoint : Doc list =
        let ( => ) txt act =
             li [if endpoint = act then yield attr.``class`` "active"] [
                a [attr.href (ctx.Link act)] [text txt]
             ]
        [
            "Home" => EndPoint.Home
            "About" => EndPoint.About
        ]

    let Main ctx action (title: string) (body: Doc list) =
        Content.Page(
            Templates.MainTemplate()
                .Title(title)
                .MenuBar(MenuBar ctx action)
                .Body(body)
                .Doc()
        )


module Site =
    open WebSharper.UI.Html

    open type WebSharper.UI.ClientServer
    let readAndParseMarkdown () =
        let filePath = Path.Combine(__SOURCE_DIRECTORY__, "README.md")
        if File.Exists(filePath) then
            let markdownContent = File.ReadAllText(filePath)
            let parsedMarkdown = Markdown.Parse(markdownContent)
            Markdown.ToHtml(markdownContent) // A markdown szöveg HTML-re alakítása
        else
            "<p>README.md file not found.</p>"
    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Home" [
            // h1 [] [text "Say Hi to JavaScript!"]
            div [] [client (Client.Main())]
        ]

    let AboutPage ctx =
        let contentHtml = readAndParseMarkdown ()
        Templating.Main ctx EndPoint.About "About" [
            //h1 [] [text "About"]
            Doc.Verbatim contentHtml // Nyers HTML beillesztése biztonságosan
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx action ->
            match action with
            | Home -> HomePage ctx
            | About -> AboutPage ctx
        )

[<Sealed>]
type Website() =
    interface IWebsite<EndPoint> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home; About]

[<assembly: Website(typeof<Website>)>]
do ()
