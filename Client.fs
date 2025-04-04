namespace JsonToolboxWebApp

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Notation
open WebSharper.UI.Templating

[<JavaScript>]
module Templates =
    type MainTemplate = Templating.Template<"Main.html", ClientLoad.FromDocument, ServerLoad.PerRequest>

[<JavaScript>]
module Client =

    let inputId = "fileInput"

    let DoSomething (input: string) =
        System.String(Array.rev (input.ToCharArray()))

    let ReadJsonFromInput1 (file: File) : Async<string> =
        async {
            Console.Log(sprintf "ReadJsonFromInput called %O" file.Name)

            let! result = Async.FromContinuations(fun (_) -> JS.Window?readFile(file))

            return result
        }

    let ReadJsonFromInput (file: File) : Async<string> =
        async {
            Console.Log(sprintf "ReadJsonFromInput called %O" file.Name)

            let jsPromise: Promise<string> = JS.Window?readFile(file)

            let! result =
                Async.FromContinuations(fun (resolve, reject, _) ->
                    jsPromise
                        .Then(fun res -> resolve (res)) // Eredmény visszaadása
                        .Catch(fun err -> reject (System.Exception("Error:")))
                    |> ignore)

            Console.Log(sprintf "JSON Content: %s" result)
            return result
        }

    let Main () =
        Console.Log("Main function called")
        let rvReversed = Var.Create ""
        // Eseménykezelés inicializálása a fájlválasztóhoz

        JS.Window.OnLoad <-
            fun _ ->
                let fileInput = JS.Document.GetElementById(inputId)

                if fileInput <> null then
                    fileInput?onchange <-
                        fun _ ->
                            let files = fileInput?files |> unbox<FileList>

                            if files.Length > 0 then
                                let file = files.[0]

                                async {
                                    try
                                        Console.Log(sprintf "file name: %s" file.Name)
                                        let! jsonContent = ReadJsonFromInput(file)
                                        Console.Log(sprintf "jsonContent: %s" jsonContent)
                                        let outputDiv = JS.Document.GetElementById("jsonOutput")
                                        outputDiv.TextContent <- jsonContent
                                    with ex ->
                                        Console.Log(sprintf "Error in Main: %s" ex.Message)
                                        let outputDiv = JS.Document.GetElementById("jsonOutput")
                                        outputDiv.TextContent <- sprintf "Error: %s" ex.Message
                                }
                                |> Async.Start

        Templates.MainTemplate
            .MainForm()
            .OnSend(fun e ->
                let res = DoSomething e.Vars.TextToReverse.Value
                rvReversed := res)
            .Reversed(rvReversed.View)
            .Doc()
