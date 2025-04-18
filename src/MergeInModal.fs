namespace JsonToolboxWebApp.MergeInModal

open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module MergeInModal =

    let private isNullOrEmpty (s: string) = obj.ReferenceEquals(s, null) || s = ""

    type ComparisonResult with
        member this.IsArrayOrObjectOrTypeConflict =
            match this.json1Value, this.json2Value with
            | JsonValue.Array _, JsonValue.Array _
            | JsonValue.Object _, JsonValue.Object _ -> true
            | JsonValue.Array _, JsonValue.Object _
            | JsonValue.Object _, JsonValue.Array _ -> true
            | JsonValue.Array _, _
            | _, JsonValue.Array _
            | JsonValue.Object _, _
            | _, JsonValue.Object _ -> true
            | _ -> false

    let private isVariableNode (segment: string) (fullPath: string) (comparisonResults: Map<string, ComparisonResult>) =
        comparisonResults.TryFind fullPath
        |> Option.map (fun r -> r.IsArrayOrObjectOrTypeConflict)
        |> Option.defaultValue false

    let rec generateHtmlForHierarchy (path: string) (comparisonResults: Map<string, ComparisonResult>) =
        let children = 
            comparisonResults
            |> Seq.groupBy (fun kvp ->
                let key = kvp.Key
                let remainingPath = key.Substring(path.Length).TrimStart('.')
                
                if isNullOrEmpty remainingPath then "" 
                else
                    // JavaScript-kompatibilis indexkezelés
                    let pattern = RegExp(@"^([^\[\.]*)(\[\d+\])?")
                    let m = pattern.Exec(remainingPath)
                    if m <> null then 
                        if not (isNullOrEmpty m.[1]) then m.[1] 
                        else if not (isNullOrEmpty m.[2]) then m.[2]
                        else ""
                    else
                        let parts = remainingPath.Split('.')
                        if parts.Length > 0 then parts.[0] else ""
            )
            |> Seq.filter (fun (segment, _) -> not (isNullOrEmpty segment))

        children
        |> Seq.map (fun (segment, groupedResults) ->
            let fullPath = 
                if path = "" then segment 
                else sprintf "%s.%s" path segment

            Console.Log("Current path:", fullPath)

            let isVar = isVariableNode segment fullPath comparisonResults
            
            // Típus információk meghatározása
            let getTypeString = function
                | JsonValue.Array _ -> "Array"
                | JsonValue.Object _ -> "Object"
                | _ -> "Value"
            
            let type1, type2 = 
                match comparisonResults.TryFind fullPath with
                | Some r -> getTypeString r.json1Value, getTypeString r.json2Value
                | None -> "", ""

            let optionsHtml = 
                match comparisonResults.TryFind fullPath with
                | Some r ->
                    let isNull v = match v with JsonValue.Null -> true | _ -> false
                    let getTypeString = function
                        | JsonValue.Array _ -> "Array"
                        | JsonValue.Object _ -> "Object"
                        | _ -> "Value"
                    
                    match r.json1Value, r.json2Value with
                    | v1, v2 when isNull v1 && isNull v2 ->
                        "<option value='null'>Exclude</option><option value='include'>Include</option>"
                    
                    | JsonValue.Array _, JsonValue.Array _ ->
                        "<option value='null'>Exclude</option><option value='include'>Include Array</option><option value='json1'>Use JSON 1</option><option value='json2'>Use JSON 2</option>"
                    
                    | JsonValue.Object _, JsonValue.Object _ ->
                        "<option value='null'>Exclude</option><option value='include'>Include Object</option><option value='json1'>Use JSON 1</option><option value='json2'>Use JSON 2</option>"
                    
                    | JsonValue.Object _, JsonValue.Array _ ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>Use Object (JSON 1)</option><option value='json2'>Use Array (JSON 2)</option>"
                    
                    | JsonValue.Array _, JsonValue.Object _ ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>Use Array (JSON 1)</option><option value='json2'>Use Object (JSON 2)</option>"
                    
                    | JsonValue.Array _, v2 ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>Use Array (JSON 1)</option><option value='json2'>%A (JSON 2)</option>" v2
                    
                    | v1, JsonValue.Array _ ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>%A (JSON 1)</option><option value='json2'>Use Array (JSON 2)</option>" v1
                    
                    | JsonValue.Object _, v2 ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>Use Object (JSON 1)</option><option value='json2'>%A (JSON 2)</option>" v2
                    
                    | v1, JsonValue.Object _ ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>%A (JSON 1)</option><option value='json2'>Use Object (JSON 2)</option>" v1
                    
                    | _ ->
                        sprintf "<option value='null'>Exclude</option><option value='json1'>%A (JSON 1)</option><option value='json2'>%A (JSON 2)</option>" r.json1Value r.json2Value
                | None ->
                    "<option value='null'>Exclude</option><option value='include'>Include</option>"


            let childHtml = 
                generateHtmlForHierarchy fullPath (
                    groupedResults
                    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                    |> Map.ofSeq
                )

            sprintf """
                <li class='node'>
                    <span class='segment'>%s</span>
                    <span class='types'>[%s/%s]</span>
                    <select data-key='%s'>%s</select>
                    <ul class='children'>%s</ul>
                </li>
            """ segment type1 type2 fullPath optionsHtml childHtml
        )
        |> String.concat ""

    let populateModal (modalBodyElement: Dom.Element) (comparisonResults: Map<string, ComparisonResult>) =
        modalBodyElement.TextContent <- ""
        let htmlContent = generateHtmlForHierarchy "" comparisonResults
        modalBodyElement.InnerHTML <- sprintf "<ul>%s</ul>" htmlContent

    let updateForMerge (comparisonResults: Map<string, ComparisonResult>) =
        let selects = JS.Document.QuerySelectorAll("select[data-key]")
        let mutable updatedResults = comparisonResults

        for i = 0 to selects.Length - 1 do
            let select = selects.[i] :?> HTMLSelectElement
            let key = select.GetAttribute("data-key")
            let selectedValue = select?value

            match comparisonResults.TryFind key with
            | Some result ->
                let newResult =
                    match selectedValue with
                    | "json1" ->
                        { result with
                            forMerge = result.json1Value }
                    | "json2" ->
                        { result with
                            forMerge = result.json2Value }
                    | "array" ->
                        match result.json1Value, result.json2Value with
                        | JsonValue.Array a, _ ->
                            { result with
                                forMerge = JsonValue.Array a }
                        | _, JsonValue.Array b ->
                            { result with
                                forMerge = JsonValue.Array b }
                        | _ ->
                            { result with
                                forMerge = JsonValue.Null }
                    | "primitive" ->
                        match result.json1Value, result.json2Value with
                        | JsonValue.Array _, b -> { result with forMerge = b }
                        | a, JsonValue.Array _ -> { result with forMerge = a }
                        | JsonValue.Object _, b -> { result with forMerge = b }
                        | a, JsonValue.Object _ -> { result with forMerge = a }
                        | a, _ -> { result with forMerge = a }
                    | "include" -> result
                    | _ ->
                        { result with
                            forMerge = JsonValue.Null }

                updatedResults <- updatedResults.Add(key, newResult)
            | None -> ()

        updatedResults

    let generateMergedJson (comparisonResults: Map<string, ComparisonResult>) : Map<string, JsonValue> =
        comparisonResults
        |> Map.fold
            (fun acc key result ->
                match result.forMerge with
                | JsonValue.Null -> acc
                | _ -> acc.Add(key, result.forMerge))
            Map.empty
