namespace JsonToolboxWebApp

open System.Text.Json
open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom
open WebSharper.UI
open WebSharper.UI.Notation
open WebSharper.UI.Templating

[<JavaScript>]
module Templates =
    type MainTemplate = Templating.Template<"Main.html", ClientLoad.FromDocument, ServerLoad.PerRequest>

[<JavaScript>]
module Client =

    let inputId = "fileInput"
    let outputDiv1 = JS.Document.GetElementById("jsonOutput1")
    let outputDiv2 = JS.Document.GetElementById("jsonOutput2")
    
    let getOutputDivTextContent (id: int) =
        match id with
        | 1 ->
            if isNull outputDiv1 then null else outputDiv1.TextContent
        | 2 ->
            if isNull outputDiv2 then null else outputDiv2.TextContent
        | _ -> 
            // Ha a bemenet nem 1 vagy 2, akkor null-t ad vissza
            null
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
        let updateReversed newValue =
            // Az rvReversed értékének frissítése
            rvReversed := newValue
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
                                    let dropdown = JS.Document.GetElementById("jsonTarget") :?> HTMLSelectElement
                                    let selectedValue = dropdown?value // A 'value' mezőt dinamikus hozzáféréssel érjük el
                                    try
                                        Console.Log(sprintf "file name: %s" file.Name)
                                        // A legördülő menü (select) aktuális értékének lekérése
                                       
                                        let! jsonContent = ReadJsonFromInput(file)
                               
                                        let jsonDocument = Json.Deserialize<Object>(jsonContent)
                                        Console.Log(sprintf "jsonContent: %s" jsonContent)
                                        match selectedValue with
                                                | "json1" ->
                                                   // let outputDiv1 = JS.Document.GetElementById("jsonOutput1")
                                                    outputDiv1.TextContent <- jsonContent
                                                    updateReversed "Json1 content loaded"
                                                    // JsonDocument létrehozása                                          
                                                | "json2" ->
                                                   // let outputDiv2 = JS.Document.GetElementById("jsonOutput2")
                                                    outputDiv2.TextContent <- jsonContent
                                                    updateReversed "Json2 content loaded"
                                                | _ ->
                                                    updateReversed "Invalid selection in dropdown."
                                                    Console.Log("Invalid selection in dropdown.")
                                                  
                                    with ex ->
                                        updateReversed (sprintf "Error in Json (if it is a real json): %s" ex.Message)
                                        Console.Log(sprintf "Error in Main: %s" ex.Message)
                                        match selectedValue with
                                                | "json1" ->
                                                   // let outputDiv1 = JS.Document.GetElementById("jsonOutput1")
                                                    outputDiv1.TextContent <- sprintf "Error: %s" ex.Message
                                                    // JsonDocument létrehozása                                          
                                                | "json2" ->
                                                   // let outputDiv2 = JS.Document.GetElementById("jsonOutput2")
                                                    outputDiv2.TextContent <- sprintf "Error: %s" ex.Message
                                                 | _ ->
                                                     Console.Log("Invalid selection in dropdown.")
                                }
                                |> Async.Start

        Templates.MainTemplate
            .MainForm()
            .OnSend(fun e ->
                let res = DoSomething e.Vars.TextToReverse.Value
                rvReversed := res)
            .Reversed(rvReversed.View)
            .Doc()
