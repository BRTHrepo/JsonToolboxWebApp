namespace JsonToolboxWebApp.MergeInModal

open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module MergeInModal =

    /// Recursively generates HTML for hierarchical JSON structure, including arrays and objects.
    let isNullOrEmpty (s: string) = obj.ReferenceEquals(s, null) || s = ""

    let isArrayOrObject (result: ComparisonResult) =
        match result.json1Value, result.json2Value with
        | JsonValue.Array _, _
        | _, JsonValue.Array _
        | JsonValue.Object _, _
        | _, JsonValue.Object _ -> true
        | _ -> false

    let isVariableNode (segment: string) (fullPath: string) (comparisonResults: Map<string, ComparisonResult>) =
        // Csak akkor true, ha van ilyen kulcs, és az érték tömb vagy objektum
        match comparisonResults.TryFind(fullPath) with
        | Some result -> isArrayOrObject result
        | None -> false

    let rec generateHtmlForHierarchy (path: string) (comparisonResults: Map<string, ComparisonResult>) =
        let children =
            comparisonResults
            |> Seq.groupBy (fun kvp ->
                let key = kvp.Key
                let remainingPath = key.Substring(path.Length).TrimStart('.')

                if System.String.IsNullOrEmpty(remainingPath) then
                    ""
                else
                    remainingPath.Split([| '.'; '[' |])
                    |> Array.filter (fun s -> not (System.String.IsNullOrEmpty(s)))
                    |> fun arr -> if arr.Length > 0 then arr.[0] else "")
            |> Seq.filter (fun (segment, _) -> not (System.String.IsNullOrEmpty(segment)))

        children
        |> Seq.map (fun (segment, groupedResults) ->
            let fullPath = if path = "" then segment else sprintf "%s.%s" path segment

            // Itt döntjük el, hogy változó-e (tömb vagy objektum), vagy konkrét elem
            let isVar = isVariableNode segment fullPath comparisonResults

            // Label: értékek megjelenítése
            let label =
                match comparisonResults.TryFind(fullPath) with
                | Some result -> sprintf "%s: JSON1(%A) | JSON2(%A)" segment result.json1Value result.json2Value
                | None -> segment

            // Opciók: változónál include/exclude, elemeknél json1/json2/exclude
            let optionsHtml =
                if isVar then
                    "<option value='null'>Exclude</option><option value='include'>Include</option>"
                else
                    let json1Value =
                        match comparisonResults.TryFind(fullPath) with
                        | Some r -> sprintf "(%A)" r.json1Value
                        | None -> "N/A"

                    let json2Value =
                        match comparisonResults.TryFind(fullPath) with
                        | Some r -> sprintf "(%A)" r.json2Value
                        | None -> "N/A"
                    //sprintf "<option value='null'>Exclude</option><option value='json1'>%s</option><option value='json2'>%s</option>" json1Value json2Value
                    sprintf
                        """
                                <option value='null'>Exclude</option>
                                <option value='json1' title='%A'>JSON 1</option>
                                <option value='json2' title='%A'>JSON 2</option>
                            """
                        json1Value
                        json2Value

            let childHtml =
                generateHtmlForHierarchy
                    fullPath
                    (groupedResults |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq)

            sprintf "<li>%s<select data-key='%s'>%s</select><ul>%s</ul></li>" label fullPath optionsHtml childHtml)
        |> String.concat ""



    /// Populates the modal with hierarchical JSON comparison results.
    let populateModal (modalBodyElement: Dom.Element) (comparisonResults: Map<string, ComparisonResult>) =
        modalBodyElement.TextContent <- "" // Clear previous content
        let htmlContent = generateHtmlForHierarchy "" comparisonResults
        modalBodyElement.InnerHTML <- sprintf "<ul>%s</ul>" htmlContent
    (*comparisonResults |> Seq.iter (fun kvp ->
            let key = kvp.Key
            let result = kvp.Value

            let containerDiv = JS.Document.CreateElement("div")
            
            let labelElement = JS.Document.CreateElement("label")
            labelElement.TextContent <- sprintf "%s: JSON1(%A) | JSON2(%A)" key result.json1Value result.json2Value
            
            let selectElement = JS.Document.CreateElement("select")
            selectElement.SetAttribute("data-key", key)
            
            let optionNull = JS.Document.CreateElement("option") |> unbox<HTMLOptionElement>
            optionNull.Value <- "null"
            optionNull.TextContent <- "Exclude"
            
            let optionJson1 = JS.Document.CreateElement("option") |> unbox<HTMLOptionElement>
            optionJson1.Value <- "json1"
            optionJson1.TextContent <- sprintf "JSON 1 (%A)" result.json1Value
            
            let optionJson2 = JS.Document.CreateElement("option") |> unbox<HTMLOptionElement>
            optionJson2.Value <- "json2"
            optionJson2.TextContent <- sprintf "JSON 2 (%A)" result.json2Value

            selectElement.AppendChild(optionNull) |> ignore
            selectElement.AppendChild(optionJson1) |> ignore
            selectElement.AppendChild(optionJson2) |> ignore

            containerDiv.AppendChild(labelElement) |> ignore
            containerDiv.AppendChild(selectElement) |> ignore
            modalBodyElement.AppendChild(containerDiv) |> ignore
        ) *)

    /// Updates the `forMerge` field in `ComparisonResult` based on user selections in the modal.
    let updateForMerge (comparisonResults: Map<string, ComparisonResult>) =
        let selects = JS.Document.QuerySelectorAll("select[data-key]")
        // NodeList -> seq
        let mutable updatedResults = comparisonResults

        for i = 0 to selects.Length - 1 do
            let select = selects.[i] :?> HTMLSelectElement
            let key = select.GetAttribute("data-key")
            let selectedValue = select.GetAttribute("value")

            match comparisonResults.TryFind(key) with
            | Some result ->
                let newResult =
                    match selectedValue with
                    | "json1" ->
                        { result with
                            forMerge = result.json1Value }
                    | "json2" ->
                        { result with
                            forMerge = result.json2Value }
                    | _ -> { result with forMerge = Null }

                updatedResults <- updatedResults.Add(key, newResult)
            | None -> ()


    /// Generates a merged JSON object based on user selections.
    let generateMergedJson (comparisonResults: Map<string, ComparisonResult>) : Map<string, JsonValue> =
        comparisonResults
        |> Map.fold
            (fun (acc: Map<string, JsonValue>) key result ->
                match result.forMerge with
                | Null -> acc // Skip unselected keys
                | _ -> acc.Add(key, result.forMerge))
            Map.empty
