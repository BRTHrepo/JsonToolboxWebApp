namespace JsonToolboxWebApp

open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
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
    let outputDiv1 = JS.Document.GetElementById("jsonOutput1")
    let outputDiv2 = JS.Document.GetElementById("jsonOutput2")
    let comparisonResultDiv = JS.Document.GetElementById("comparisonResult")
    let filterSelect = JS.Document.GetElementById("filterSame") :?> HTMLSelectElement
    let CompareJsons jsonString1 jsonString2 =
        try
            // JSON stringek feldolgozása JsonValue típusra
            let json1 = traverseJsonDocument jsonString1
            let json2 = traverseJsonDocument jsonString2

            // Összehasonlítás végrehajtása
            let comparisonResult = compareJsonDictionaries json1 json2
            comparisonResult // További feldolgozásra visszaadható
        with ex ->
            Console.Log("Error during JSON comparison: ", ex.Message)
            raise ex
    let getOutputDivTextContent (id: int) =
        // A bemeneti id alapján visszaadja a megfelelő output div szövegét 
        match id with
        | 1 ->
            if isNull outputDiv1 then null else outputDiv1.TextContent
        | 2 ->
            if isNull outputDiv2 then null else outputDiv2.TextContent
        | _ -> 
            // Ha a bemenet nem 1 vagy 2, akkor null-t ad vissza
            null
    
    let filterResultsBySame (results: Map<string, ComparisonResult>) (filter: string) : Map<string, ComparisonResult> =
        match filter with
        | "true" -> results |> Map.filter (fun _ v -> v.same) // Csak azonosak
        | "false" -> results |> Map.filter (fun _ v -> not v.same) // Csak eltérők
        | _ -> results // Mindkettőt visszaadja ("all")

    let formatComparisonResult (dictionary: Map<string, ComparisonResult>) : string =
        dictionary
        |> Map.fold (fun acc key value ->
            let same = sprintf "Same: %b" value.same
            let json1 = sprintf "JSON1 Value: %A" value.json1Value
            let json2 = sprintf "JSON2 Value: %A" value.json2Value
            acc + sprintf "\nKey: %s\n  %s\n  %s\n  %s\n" key same json1 json2
        ) ""
    let updateHtmlWithFormattedResult (formattedResult: string) =
        // Eredmény frissítése a HTML-ben
        if not (isNull comparisonResultDiv) then
            comparisonResultDiv.TextContent <- formattedResult

    let checkAllJsons () =
        try
              // Példa: két JSON betöltése és összehasonlítása
            let json1Content = getOutputDivTextContent 1
            Console.Log("json1Content: ", json1Content)
            let json2Content = getOutputDivTextContent 2
            Console.Log("json1Content: ", json1Content)

            if not (isNull json1Content) && not (isNull json2Content) && not (json1Content.Length = 0) && not (json2Content.Length = 0) then
                let result = CompareJsons json1Content json2Content
                Console.Log("Comparison completed.", result)
                
                let selectedFilter = filterSelect?value
                            // Szűrt eredmények előállítása
                let filteredResult = filterResultsBySame result selectedFilter
                // Formázott eredmény előállítása
                let formattedResult = formatComparisonResult filteredResult
                // Eredmény frissítése a HTML-ben
                updateHtmlWithFormattedResult formattedResult
            else
                updateHtmlWithFormattedResult "One or both JSON contents are missing."
        with ex ->  updateHtmlWithFormattedResult (sprintf "Error during JSON comparison: %s" ex.Message)
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
                if not (isNull filterSelect) then
                     filterSelect.OnChange <- fun _ -> checkAllJsons ()
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
                                        checkAllJsons ()
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
            (*.OnSend(fun e ->
                let res = DoSomething e.Vars.TextToReverse.Value
                rvReversed := res)*)
            .Reversed(rvReversed.View)
            .Doc()
