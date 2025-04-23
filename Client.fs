namespace JsonToolboxWebApp

open JsonToolboxWebApp.ComparisonResultPrinter
open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
open Microsoft.FSharp.Core
open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom
open WebSharper.UI
open WebSharper.UI.Notation
open WebSharper.UI.Templating
open JsonToolboxWebApp.MergeInModal

/// <summary>
/// HTML template binding for the main form.
/// </summary>
[<JavaScript>]
module Templates =
    type MainTemplate = Templating.Template<"Main.html", ClientLoad.FromDocument, ServerLoad.PerRequest>

[<JavaScript>]
module Client =
    
    /// <summary>
    /// Reactive variable for displaying reversed text or status messages.
    /// </summary>
    let rvReversed = Var.Create "Hello"

    /// <summary>
    /// Updates the value of rvReversed.
    /// </summary>
    /// <param name="newValue">The new value to set.</param>
    let updateReversed newValue =
        // Az rvReversed értékének frissítése
        rvReversed := newValue
    // IDs used for DOM element selection
    let modalTitleId = "jsonModalLabel"
    let buttonMergeId = "MergeJsons"
    let buttoncShowCmparisonResultId = "comparisonResultBt"
    let inputId = "fileInput"
    let outputDiv1Id = "jsonOutput1"
    let outputDiv2Id = "jsonOutput2"
    // let comparisonResultDiv = JS.Document.GetElementById("comparisonResult")
   

    /// <summary>
    /// Gets an element by id, if it doesn't exist, returns None.
    /// </summary>
    /// <param name="id">Element id</param>
    /// <returns>Element if found, otherwise None</returns>
    let getElementByIdOpt (id: string) : Option<Element> =
        match JS.Document.GetElementById(id) with
        | null -> None
        | element -> Some element

    /// <summary>
    /// Shows the JSON content in a modal dialog.
    /// </summary>
    /// <param name="jsonContent">JSON content to display</param>
    /// <param name="i">Index of JSON (1 or 2)</param>
    let ShowJsonInModal (jsonContent: string) (i: int) =
        let jsonModalLabel = JS.Document.GetElementById(modalTitleId)
        jsonModalLabel.TextContent <- sprintf "JSON Content %d" i
        let modalContent = JS.Document.GetElementById("formattedJson")
        modalContent.TextContent <- jsonContent // Modal tartalom frissítése

        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?add ("show") // Bootstrap modal megnyitása
        modal?style?display <- "block"
        modal?("show")
        modal?focus ()

    /// <summary>
    ///    Hides the JSON content modal
    /// </summary>
    /// <returns> </returns>
    let HideJsonModal () =
        let modalBody = JS.Document.GetElementById("modalBody")
        modalBody.TextContent <- ""
        let modalContent = JS.Document.GetElementById("formattedJson")

        if isNull modalContent then
            let pre = JS.Document.CreateElement("pre")
            pre.SetAttribute("id", "formattedJson")
            modalBody.AppendChild(pre) |> ignore

        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?remove ("show") // Bootstrap modal bezárása
        modal?style?display <- "none"



    /// <summary>
    /// Compares two JSON strings and returns a ComparisonResult tree.
    /// </summary>
    /// <param name="jsonString1">JSON string 1</param>
    /// <param name="jsonString2">JSON string 2</param>
    /// <returns>ComparisonResult tree</returns>
    let CompareJsons jsonString1 jsonString2 =
        Console.Log("Comparing JSON strings...")

        try
            // JSON stringek feldolgozása JsonValue típusra
            let json1 = traverseJsonDocument jsonString1
            let json2 = traverseJsonDocument jsonString2

            // Összehasonlítás végrehajtása
            let comparisonResult = compareJsonTrees json1 json2
            Console.Log("Comparison result: 2")
            let outputString = ComparisonResultPrinter.printComparisonResult comparisonResult 0
            Console.Log(outputString)


            // A root elem visszaadása lista helyett
            comparisonResult
        with ex ->
            Console.Log("Error during JSON comparison: " + ex.Message)
            raise ex

    /// <summary>
    /// Gets the text content of the output div based on the id.
    /// </summary>
    /// <param name="id">1 or 2</param>
    /// <returns>Text content of the output div</returns>
    let getOutputDivTextContent (id: int) =
        // A bemeneti id alapján visszaadja a megfelelő output div szövegét

        let outputDiv1 = getElementByIdOpt (outputDiv1Id)
        let outputDiv2 = getElementByIdOpt (outputDiv2Id)

        match id with
        | 1 ->
            match outputDiv1 with
            | Some div -> div.TextContent
            | None -> null
        | 2 ->
            match outputDiv2 with
            | Some div -> div.TextContent
            | None -> null
        | _ ->
            // Ha a bemenet nem 1 vagy 2, akkor null-t ad vissza
            null
            
    /// <summary>
    /// Shows the merge modal dialog, sets up modal content for merging JSONs.
    /// </summary>
    let ShowMergeInModal () =
        let jsonModalLabel = JS.Document.GetElementById(modalTitleId)
        jsonModalLabel.TextContent <- "Merge JSONs"
        let modalBody = JS.Document.GetElementById("modalBody")
        let json1Content = getOutputDivTextContent 1
        let json2Content = getOutputDivTextContent 2
        //MergeInModal.populateModal modalBody (compareJsonTrees json1Content json2Content)

        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?add ("show") // Bootstrap modal megnyitása
        modal?style?display <- "block"
        modal?("show")
        modal?focus ()


    /// <summary>
    /// Formats the comparison result into a string for display.
    /// </summary>
    /// <param name="cmpr">ComparisonResult</param>
    /// <param name="indentLevel">Indentation level for pretty print</param>
    /// <returns>Formatted string</returns>
    let rec formatComparisonResult (cmpr: ComparisonResult) (indentLevel: int) =
        let padding = String.replicate (indentLevel * 4) " "

        match cmpr with
        | PrimitiveComparison pc ->
            let same = sprintf "%*sSame: %b" (indentLevel * 4) "" pc.same
            let json1 = sprintf "%*sJSON1: %A" (indentLevel * 4) "" pc.json1Value
            let json2 = sprintf "%*sJSON2: %A" (indentLevel * 4) "" pc.json2Value
            sprintf "%sPrimitiveComparison\n%s\n%s\n%s\n\n" padding same json1 json2

        | ObjectComparison oc ->
            let same = sprintf "%*sSame: %b" (indentLevel * 4) "" oc.same
            let json1 = sprintf "%*sJSON1: %A" (indentLevel * 4) "" oc.json1Value
            let json2 = sprintf "%*sJSON2: %A" (indentLevel * 4) "" oc.json2Value

            let childrenFormatted =
                oc.children
                |> Map.fold (fun acc key value -> acc + formatComparisonResult value (indentLevel + 1)) ""

            sprintf "%sObjectComparison\n%s\n%s\n%s\n%s\n\n" padding same json1 json2 childrenFormatted

        | ArrayComparison ac ->
            let same = sprintf "%*sSame: %b" (indentLevel * 4) "" ac.same
            let json1 = sprintf "%*sJSON1: %A" (indentLevel * 4) "" ac.json1Value
            let json2 = sprintf "%*sJSON2: %A" (indentLevel * 4) "" ac.json2Value

            let itemsFormatted =
                ac.items
                |> Array.fold (fun acc item -> acc + formatComparisonResult item (indentLevel + 1)) ""

            sprintf "%sArrayComparison\n%s\n%s\n%s\n%s\n\n" padding same json1 json2 itemsFormatted




    /// <summary>
    /// Shows the comparison result in the modal, filtered as requested.
    /// </summary>
    /// <param name="json1Content">JSON content 1 as string</param>
    /// <param name="json2Content">JSON content 2 as string</param>
    let showComparisonResult (json1Content: string) (json2Content: string) =
        try
            if
                not (isNull json1Content)
                && not (isNull json2Content)
                && not (json1Content.Length = 0)
                && not (json2Content.Length = 0)
            then
                let jsonModalLabel = JS.Document.GetElementById(modalTitleId)
                jsonModalLabel.TextContent <- "Comparison Result"
                let modalBody = JS.Document.GetElementById("modalBody")
                let result = CompareJsons json1Content json2Content
                let filterSelect = JS.Document.GetElementById("filterSame") :?> HTMLSelectElement
                let selectedFilter = filterSelect?value
                let keySearchInput =        JS.Document.GetElementById("keySearchInput") :?> HTMLInputElement
                let key =
                    if isNull keySearchInput.Value || keySearchInput.Value.Trim() = "" then
                        ""
                    else
                        Some(keySearchInput.Value.Trim()).Value
                formatSingleComparisonResultForModal modalBody result selectedFilter key
        with ex ->
            Console.Log("Error during JSON comparison", ex)
            updateReversed (sprintf "Error during JSON comparison: %s" ex.Message)

    /// <summary>
    ///    Checks if all JSONs are loaded and ready for comparison, then can show the merge in modal if they are
    /// </summary>
    let checkAllJsons () =
        let buttonMerge = getElementByIdOpt buttonMergeId
        Console.Log("Merge button is", buttonMerge)

        let buttoncShowCmparisonResult =
            JS.Document.GetElementById(buttoncShowCmparisonResultId)
            |> unbox<HTMLButtonElement>

        try
            let json1Content = getOutputDivTextContent 1
            Console.Log("json1Content: ", json1Content)
            let json2Content = getOutputDivTextContent 2
            Console.Log("json1Content: ", json1Content)

            if
                not (isNull json1Content)
                && not (isNull json2Content)
                && not (json1Content.Length = 0)
                && not (json2Content.Length = 0)
            then
                match buttonMerge with
                | Some button -> button.AddEventListener("click", fun (ev: Event) -> ShowMergeInModal())
                | _ -> Console.Log("Merge button is not found")

                buttoncShowCmparisonResult.AddEventListener(
                    "click",
                    fun (ev: Event) -> (showComparisonResult json1Content json2Content)
                )
            else
                match buttonMerge with
                | Some button -> button.AddEventListener("click", fun (ev: Event) -> ())
                | _ -> Console.Log("Merge button is not found")

                buttoncShowCmparisonResult.AddEventListener("click", fun (ev: Event) -> ())
                updateReversed "Please load both JSON files before ..."
        with ex ->
            match buttonMerge with
            | Some button -> button.AddEventListener("click", fun (ev: Event) -> ())
            | _ -> Console.Log("Merge button is not found")

            buttoncShowCmparisonResult.AddEventListener("click", fun (ev: Event) -> ())
            updateReversed ex.Message
            Console.Log("Error during JSON comparison", ex)



    /// <summary>
    /// Reads the JSON content from the input file using a JS FileReader.
    /// </summary>
    /// <param name="file">File object</param>
    /// <returns>Async<string> with the file content</returns>
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

    /// <summary>
    ///    Initializes the key search input field , which triggers the comparison when the value changes
    /// </summary>
    let initializeKeySearch () =
        //if not (isNull keySearchInput) then
        // keySearchInput.OnChange <- fun _ -> checkAllJsons ()
        let buttonShowModal1 =
            JS.Document.GetElementById("showJson1Modal") |> unbox<HTMLButtonElement>

        buttonShowModal1.AddEventListener("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 1) 1)
        let buttonShowModal2 = getElementByIdOpt ("showJson2Modal")

        match buttonShowModal2 with
        | Some button ->
            button.AddEventListener("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 2) 2)
        | None -> ()

        let hideModal = JS.Document.GetElementById("closeModal")
        hideModal.AddEventListener("click", fun (ev: Event) -> HideJsonModal())
        let hideModal1 = JS.Document.GetElementById("closeModal1")
        hideModal1.AddEventListener("click", fun (ev: Event) -> HideJsonModal())

    /// <summary>
    ///  Main function
    /// </summary>
    let Main () =
        Console.Log("Main function called")

    
        // Eseménykezelés inicializálása a fájlválasztóhoz
        JS.Window.OnLoad <-
            fun _ ->
               // initializeKeySearch ()
                let buttonShowModal1 =
                    JS.Document.GetElementById("showJson1Modal") |> unbox<HTMLButtonElement>

                buttonShowModal1.AddEventListener("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 1) 1)
                let buttonShowModal2 = getElementByIdOpt ("showJson2Modal")

                match buttonShowModal2 with
                | Some button ->
                    button.AddEventListener("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 2) 2)
                | None -> ()

                let hideModal = JS.Document.GetElementById("closeModal")
                hideModal.AddEventListener("click", fun (ev: Event) -> HideJsonModal())
                let hideModal1 = JS.Document.GetElementById("closeModal1")
                hideModal1.AddEventListener("click", fun (ev: Event) -> HideJsonModal())
                let fileInput = JS.Document.GetElementById(inputId)
                Console.Log("File input is", fileInput)
                // if not (isNull filterSelect) then
                //     filterSelect.OnChange <- fun _ -> checkAllJsons ()

                if fileInput <> null then
                    fileInput?onchange <-
                        fun _ ->
                            Console.Log("File input changed")

                            let files = fileInput?files |> unbox<FileList>

                            if files.Length > 0 then
                                let file = files.[0]

                                async {
                                    let dropdown = JS.Document.GetElementById("jsonTarget") :?> HTMLSelectElement
                                    let selectedValue = dropdown?value // A 'value' mezőt dinamikus hozzáféréssel érjük el
                                    let outputDiv1 = getElementByIdOpt (outputDiv1Id)
                                    let outputDiv2 = getElementByIdOpt (outputDiv2Id)

                                    try
                                        Console.Log(sprintf "file name: %s" file.Name)
                                        // A legördülő menü (select) aktuális értékének lekérése

                                        let! jsonContent = ReadJsonFromInput(file)
                                        // try to parse the JSON content for validation ,
                                        // if it's not valid, it will throw an exception
                                        let jsonDocument = Json.Deserialize<Object>(jsonContent)

                                        match selectedValue with
                                        | "json1" ->
                                            match outputDiv1 with
                                            | Some div ->
                                                div.TextContent <- jsonContent
                                                updateReversed "Json1 content loaded"
                                                buttonShowModal1.TextContent <- file.Name
                                            | None -> updateReversed ("No output div found.")
                                        | "json2" ->
                                            match outputDiv2 with
                                            | Some div ->
                                                div.TextContent <- jsonContent
                                                updateReversed "Json2 content loaded"
                                                match buttonShowModal2 with
                                                | Some button -> button.TextContent <- file.Name
                                                | None -> ()

                                            | None -> updateReversed ("No output div found.")
                                        | _ ->
                                            updateReversed "Invalid selection in dropdown."
                                            Console.Log("Invalid selection in dropdown.")

                                    with ex ->
                                        match selectedValue with
                                        | "json1" ->
                                            match outputDiv1 with
                                            | Some div -> div.TextContent <- sprintf "Error: %s" ex.Message
                                            | None ->
                                                updateReversed (
                                                    sprintf
                                                        "No output div found. Error in Json (if it is a real json): %s"
                                                        ex.Message
                                                )
                                        // JsonDocument létrehozása
                                        | "json2" ->
                                            match outputDiv2 with
                                            | Some div -> div.TextContent <- sprintf "Error: %s" ex.Message
                                            | None ->
                                                updateReversed (
                                                    sprintf
                                                        "No output div found. Error in Json (if it is a real json): %s"
                                                        ex.Message
                                                )
                                        | _ -> updateReversed ("Invalid selection in dropdown.")

                                    checkAllJsons ()
                                }
                                |> Async.Start

        Templates.MainTemplate
            .MainForm()
            (*.OnSend(fun e ->
                let res = DoSomething e.Vars.TextToReverse.Value
                rvReversed := res)*)
            .Reversed(rvReversed.View)
            .Doc()
