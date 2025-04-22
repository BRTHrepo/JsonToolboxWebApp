namespace JsonToolboxWebApp

open JsonToolboxWebApp.ComparisonResultPrinter
open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom
open WebSharper.UI
open WebSharper.UI.Notation
open WebSharper.UI.Templating
open JsonToolboxWebApp.MergeInModal
[<JavaScript>]
module Templates =
    type MainTemplate = Templating.Template<"Main.html", ClientLoad.FromDocument, ServerLoad.PerRequest>

[<JavaScript>]
module Client =

    let buttonMergeId = "MergeJsons"
    let buttoncShowCmparisonResultId = "comparisonResultBt"
    let inputId = "fileInput"
    let outputDiv1Id = "jsonOutput1"
    let outputDiv2Id = "jsonOutput2"
   // let comparisonResultDiv = JS.Document.GetElementById("comparisonResult")
    let filterSelect = JS.Document.GetElementById("filterSame") :?> HTMLSelectElement
    
        /// <summary>
    ///    Gets an element by id, if it doesn't exist, returns None
    /// </summary>
    /// <param name="id"> Element id </param>
    /// <returns> Element if found, otherwise None </returns> 
    let getElementByIdOpt (id: string) : Option<Element> =
        match JS.Document.GetElementById(id) with
        | null -> None
        | element -> Some element
        

    let ShowJsonInModal (jsonContent: string) =
        let modalContent = JS.Document.GetElementById("formattedJson")
        modalContent.TextContent <- jsonContent // Modal tartalom frissítése

        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?add("show") // Bootstrap modal megnyitása
        modal?style?display <- "block"
        modal?("show")
        modal?focus()
    // Modal bezárása
    let HideJsonModal () =
        let modalBody = JS.Document.GetElementById("modalBody")
        modalBody.TextContent <- ""
        let modalContent = JS.Document.GetElementById("formattedJson")
        if isNull modalContent then
            let pre = JS.Document.CreateElement("pre")
            pre.SetAttribute("id", "formattedJson")
            modalBody.AppendChild(pre) |> ignore
        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?remove("show") // Bootstrap modal bezárása
        modal?style?display <- "none"
        
    let keySearchInput =
        JS.Document.GetElementById("keySearchInput") :?> HTMLInputElement
   

        
    let outputDiv1 = getElementByIdOpt (outputDiv1Id)
    let outputDiv2 = getElementByIdOpt (outputDiv2Id)
    
    /// <summary>
    ///    Compares two JSON strings and returns a map of comparison results
    /// </summary>
    /// <param name="jsonString1"> JSON string 1 </param>
    /// <param name="jsonString2"> JSON string 2 </param>
    /// <returns> Map of comparison results </returns>
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
    ///    Get the text content of the output div based on the id
    /// </summary>
    /// <param name="id"> 1 or 2 </param>
    /// <returns> Text content of the output div </returns>
    let getOutputDivTextContent (id: int) =
        // A bemeneti id alapján visszaadja a megfelelő output div szövegét
        match id with
        | 1 ->  match  outputDiv1 with
                | Some div -> div.TextContent
                | None -> null
        | 2 ->  match outputDiv2 with
                | Some div -> div.TextContent
                | None -> null
        | _ ->
            // Ha a bemenet nem 1 vagy 2, akkor null-t ad vissza
            null
             // Modal tartalom frissítése és megnyitása
    /// <summary>
    ///    Shows the merge in modal
    /// </summary>
    /// <returns> </returns>
    let ShowMergeInModal()  =
        let modalBody = JS.Document.GetElementById("modalBody")
        let json1Content = getOutputDivTextContent 1
        let json2Content = getOutputDivTextContent 2
        //MergeInModal.populateModal modalBody (compareJsonTrees json1Content json2Content)

        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?add("show") // Bootstrap modal megnyitása
        modal?style?display <- "block"
        modal?("show")
        modal?focus()
    // Not used
    /// <summary>
    ///   English : Filtering by key, exact match 
    /// </summary>
    /// <param name="results"></param>
    /// <returns> Filtered results </returns>
    let filterResultsByKey2 (results: Map<string, ComparisonResult>) =
        let key =
            if isNull keySearchInput.Value || keySearchInput.Value.Trim() = "" then
                None
            else
                Some(keySearchInput.Value.Trim())

        match key with
        | Some k when not (isNull k) && k.Trim() <> "" -> //  String.IsNullOrWhiteSpace
            results |> Map.filter (fun mapKey _ -> mapKey = k)
        | _ -> results

    /// <summary>
    ///    Filtering by key, contains
    /// </summary>
    /// <param name="map">  UnionDictionary </param>
    /// <returns> Filtered results</returns>
    let rec filterResultsByKey (map: Map<string, ComparisonResult>) =
        let key = if isNull keySearchInput.Value then keySearchInput.Value.Trim() else ""
        match key with
        | "" -> map
        | filterString ->
            map |> Map.filter (fun mapKey _ -> mapKey.Contains(filterString))


    /// <summary>
    ///    Filtering by filter string (All; no filtering, Same; same, Different; differences)
    ///  </summary>
    /// <param name="map"> UnionDictionary </param>
    /// <param name="filter"> All, Same , Different </param>
    /// <returns> Filtered results </returns>
    let rec filterResultsBySame (cmpr: ComparisonResult) (filter: string) =
        match filter with
        | "true" -> cmpr
        | "false" -> cmpr
        | _ -> cmpr

    /// <summary>
    ///    Formats the comparison result into a string
    ///    </summary>
    /// <param name="dictionary"> UnionDictionary </param>
    /// <returns> Formatted comparison result </returns>
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
                oc.children |> Map.fold (fun acc key value ->
                    acc + formatComparisonResult value (indentLevel + 1)
                ) ""
            sprintf "%sObjectComparison\n%s\n%s\n%s\n%s\n\n" padding same json1 json2 childrenFormatted
        
        | ArrayComparison ac ->
            let same = sprintf "%*sSame: %b" (indentLevel * 4) "" ac.same
            let json1 = sprintf "%*sJSON1: %A" (indentLevel * 4) "" ac.json1Value
            let json2 = sprintf "%*sJSON2: %A" (indentLevel * 4) "" ac.json2Value
            let itemsFormatted =
                ac.items |> Array.fold (fun acc item ->
                    acc + formatComparisonResult item (indentLevel + 1)
                ) ""
            sprintf "%sArrayComparison\n%s\n%s\n%s\n%s\n\n" padding same json1 json2 itemsFormatted



   
    /// <summary>
    ///    Updates the HTML with the formatted comparison result
    /// </summary>
    /// <param name="formattedResult"> Formatted comparison result </param>
 //   let updateHtmlWithFormattedResult (formattedResult: string) =
        // Eredmény frissítése a HTML-ben
     //   if not (isNull comparisonResultDiv) then
      //      comparisonResultDiv.TextContent <- formattedResult
            
    /// <summary>
    ///    Checks if all JSONs are loaded and ready for comparison, then performs the comparison
    /// </summary>
    let checkAllJsons () =
        try
            // Példa: két JSON betöltése és összehasonlítása
            let json1Content = getOutputDivTextContent 1
            Console.Log("json1Content: ", json1Content)
            let json2Content = getOutputDivTextContent 2
            Console.Log("json1Content: ", json1Content)
            let buttonMerge = getElementByIdOpt buttonMergeId
            Console.Log("Merge button is" , buttonMerge)
          
            if
                not (isNull json1Content)
                && not (isNull json2Content)
                && not (json1Content.Length = 0)
                && not (json2Content.Length = 0)
            then
                let modalBody = JS.Document.GetElementById("modalBody")
                let result = CompareJsons json1Content json2Content
               // Console.Log("Comparison completed", result)
              
                let selectedFilter = filterSelect?value
                // Szűrt eredmények előállítása
               // let filteredResult = filterResultsBySame result selectedFilter
                // key szűrés
               // let filteredResultByKey = filterResultsByKey filteredResult
                // Formázott eredmény előállítása
              //  let formattedResult = formatComparisonResult filteredResult
                // Eredmény frissítése a HTML-ben
              //  updateHtmlWithFormattedResult formattedResult
                formatSingleComparisonResultForModal modalBody result selectedFilter keySearchInput.Value
                match  buttonMerge with
                   | Some button -> button.AddEventListener ("click", fun (ev: Event) -> ShowMergeInModal()   )
                   | _ -> Console.Log("Merge button is not found")
            else
                match  buttonMerge with
                   | Some button -> button.AddEventListener ("click", fun (ev: Event) -> ()   )
                   | _ -> Console.Log("Merge button is not found")
                //updateHtmlWithFormattedResult "One or both JSON contents are missing."
        with ex ->
             Console.Log("Error during JSON comparison", ex)
          //  updateHtmlWithFormattedResult (sprintf "Error during JSON comparison: %s" ex.Message)
            


    /// <summary>
    ///    Reads the JSON content from the input file . Invokes the JavaScript script embedded in the HTML, which is responsible for reading the JSON content from the input file.
    /// </summary>
    /// <param name="file"> File </param>
    /// <returns> JSON content </returns>
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
        let buttonShowModal1 =  JS.Document.GetElementById("showJson1Modal") |> unbox<HTMLButtonElement>
        buttonShowModal1.AddEventListener ("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 1)  )
        let buttonShowModal2 = getElementByIdOpt ("showJson2Modal")
        match buttonShowModal2 with
              | Some button ->
                  button.AddEventListener ("click", fun (ev: Event) -> ShowJsonInModal (getOutputDivTextContent 2)  )
              | None ->  ()                            
                        
        let hideModal = JS.Document.GetElementById("closeModal")
        hideModal.AddEventListener ("click", fun (ev: Event) -> HideJsonModal () )
        let hideModal1 = JS.Document.GetElementById("closeModal1")
        hideModal1.AddEventListener ("click", fun (ev: Event) -> HideJsonModal () )
        let buttoncShowCmparisonResult =  JS.Document.GetElementById(buttoncShowCmparisonResultId) |> unbox<HTMLButtonElement>
        buttoncShowCmparisonResult.AddEventListener ("click", fun (ev: Event) ->  checkAllJsons ()  )
    /// <summary>
    ///  Main function
    /// </summary>
    let Main () =
        Console.Log("Main function called")
        let rvReversed = Var.Create ""

        let updateReversed newValue =
            // Az rvReversed értékének frissítése
            rvReversed := newValue

        // Eseménykezelés inicializálása a fájlválasztóhoz
        JS.Window.OnLoad <-
            fun _ ->
                initializeKeySearch ()
           
                let fileInput = JS.Document.GetElementById(inputId)
                Console.Log("File input is" , fileInput)
                if not (isNull filterSelect) then
                    filterSelect.OnChange <- fun _ -> checkAllJsons ()

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

                                        try
                                            Console.Log(sprintf "file name: %s" file.Name)
                                            // A legördülő menü (select) aktuális értékének lekérése

                                            let! jsonContent = ReadJsonFromInput(file)
                                            // try to parse the JSON content for validation ,
                                            // if it's not valid, it will throw an exception
                                            let jsonDocument = Json.Deserialize<Object>(jsonContent)
                                         
                                            match selectedValue with
                                            | "json1" ->
                                                match  outputDiv1 with
                                                | Some div -> div.TextContent <- jsonContent
                                                              updateReversed "Json1 content loaded"   
                                                | None ->  updateReversed("No output div found.")                                                        
                                            | "json2" ->
                                                match  outputDiv2 with
                                                | Some div -> div.TextContent <- jsonContent
                                                              updateReversed "Json2 content loaded"   
                                                | None ->  updateReversed("No output div found.")                      
                                            | _ ->
                                                updateReversed "Invalid selection in dropdown."
                                                Console.Log("Invalid selection in dropdown.")
                                            checkAllJsons ()
                                        with ex ->
                                            match selectedValue with
                                            | "json1" ->
                                                match outputDiv1 with
                                                | Some div -> div.TextContent <- sprintf "Error: %s" ex.Message
                                                | None -> updateReversed (sprintf "No output div found. Error in Json (if it is a real json): %s" ex.Message)
                                            // JsonDocument létrehozása
                                            | "json2" ->
                                                match outputDiv2 with
                                                | Some div -> div.TextContent <- sprintf "Error: %s" ex.Message
                                                | None -> updateReversed (sprintf "No output div found. Error in Json (if it is a real json): %s" ex.Message)
                                            | _ -> updateReversed ("Invalid selection in dropdown.")
                                        }
                                |> Async.Start

        Templates.MainTemplate
            .MainForm()
            (*.OnSend(fun e ->
                let res = DoSomething e.Vars.TextToReverse.Value
                rvReversed := res)*)
            .Reversed(rvReversed.View)
            .Doc()
