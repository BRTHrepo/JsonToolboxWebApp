namespace JsonToolboxWebApp

open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript
open JsonToolboxWebApp.JsonComparator.JsonComparator

[<JavaScript>]
module ComparisonResultPrinter =

    let printJsonValue (indent : int )( json: JsonValue)  =
        let indentStr = String.replicate indent "  "
        match json with
        | Primitive(key, value) ->
             Console.Log($"{indentStr}Primitive - Key: {key}, Value: {value}")
        | Object(key, _) ->
            Console.Log($"{indentStr}Object - Key: {key}")
        | Array(key, _) ->
            Console.Log($"{indentStr}Array - Key: {key}")
        | Null(key) ->
            Console.Log($"{indentStr}Null - Key: {key}")


    let rec printComparisonResult (result: ComparisonResult) (indent: int) =
     
        let indentStr = String.replicate indent "  "

        match result with
        | PrimitiveComparison pc ->
            Console.Log($"{indentStr}PrimitiveComparison [same: {pc.same}]")
            Console.Log($"{indentStr}json1Value:")
            printJsonValue (indent+1)  pc.json1Value
            Console.Log($"{indentStr}json2Value:")
            printJsonValue (indent+1)  pc.json2Value
          

        | ObjectComparison oc ->
            Console.Log(sprintf "%sObjectComparison", indentStr)
            printJsonValue (indent+1) oc.json1Value
            printJsonValue (indent+1) oc.json2Value
            Console.Log(sprintf "%ssame: %b" indentStr oc.actualSame)
            for KeyValue(key, child) in oc.children do
                Console.Log(sprintf "%sField key: %s", indentStr, key)
                printComparisonResult child (indent + 1)

        | ArrayComparison ac ->
            Console.Log(sprintf "%sArrayComparison", indentStr)
            printJsonValue (indent+1) ac.json1Value
            printJsonValue (indent+1) ac.json2Value
            Console.Log(sprintf "%ssame: %b" indentStr ac.actualSame)
            for i = 0 to ac.items.Length - 1 do
                let level = indent + 1
                printComparisonResult ac.items.[i] level

        | TypeMismatchComparison tmc ->
            Console.Log(sprintf "%sTypeMismatchComparison", indentStr)
            printJsonValue (indent+1) tmc.json1Value
            printJsonValue (indent+1) tmc.json2Value
            Console.Log(sprintf "%ssame: %b" indentStr tmc.same)
            match tmc.children1 with
            | Some child1 -> 
                Console.Log(sprintf "%sChildren1:", indentStr)
                printComparisonResult child1 (indent + 1)
            | None -> ()
            match tmc.children2 with
            | Some child2 -> 
                Console.Log(sprintf "%sChildren2:", indentStr)
                printComparisonResult child2 (indent + 1)
            | None -> ()

    let htmlEncode (s: string) =
        s.Replace("&", "&amp;")
         .Replace("<", "&lt;")
         .Replace(">", "&gt;")
         .Replace("\"", "&quot;")
         .Replace("'", "&#39;")

    let jsonValueToDom (indent: int) (json: JsonValue) : Dom.Element =
        let doc = JS.Document
        let indentStr = String.replicate indent "  "
        let div = doc.CreateElement "div"
        div.AppendChild(doc.CreateTextNode(indentStr)) |> ignore
        match json with
        | Primitive(key, value) ->
            let valueStr =
                match value with
                | :? string as s -> sprintf "\"%s\"" (htmlEncode s)
                | :? float as f -> string f
                | :? int as i -> string i
                | :? bool as b -> string b
                | null -> "null"
                | _ -> sprintf "%O" value
            div.InnerHTML <- sprintf "<b>Primitive</b> - Key: %s, Value: %s" (htmlEncode key) valueStr

        | Object(key, _) ->
            div.InnerHTML <- sprintf "<b>Object</b> - Key: %s" (htmlEncode key)

        | Array(key, _) ->
            div.InnerHTML <- sprintf "<b>Array</b> - Key: %s" (htmlEncode key)

        | Null(key) ->
            div.InnerHTML <- sprintf "<b>Null</b> - Key: %s" (htmlEncode key)

        div
    // Segédfüggvény a String.IsNullOrWhiteSpace helyett
    let isNullOrWhiteSpace (s: string) =
        if isNull s then true
        else s.Trim() = ""

    // Frissített shouldDisplayElement függvény
    let shouldDisplayElement (json1: JsonValue) (json2: JsonValue) (filterKey: string) (filterSame: string) (same: bool) =
        let key1 = getKey json1
        let key2 = getKey json2
        let value1 = getPrimitiveValueString json1
        let value2 = getPrimitiveValueString json2
        let keyFilterPassed =
            Console.Log($"filterKey: {filterKey}")
            Console.Log($"key1: {key1.Value.ToString()}")
            Console.Log($"key2: {key2.Value.ToString()}")
            if not (isNullOrWhiteSpace filterKey) && not (isNullOrWhiteSpace key1.Value) && not (isNullOrWhiteSpace key2.Value)then
                (key1.IsSome && key1.Value.Contains(filterKey)) || value1.Contains(filterKey) ||
                (key2.IsSome && key2.Value.Contains(filterKey)) || value2.Contains(filterKey)
            else true
        Console.Log($"keyFilterPassed: {keyFilterPassed}  " )
        let sameFilterPassed =
            match filterSame with
            | "true" -> same
            | "false" -> not same
            | _ -> true
        Console.Log($"sameFilterPassed: {sameFilterPassed}  " )    
        Console.Log($"--------------------------------------------------------------------")
        keyFilterPassed && sameFilterPassed
        
        
    /// <summary>
    /// 
    /// </summary>
    /// <param name="modalBody"></param>
    /// <param name="result"></param>
    /// <param name="filterSame"></param>
    /// <param name="filterKeyString"></param>
    let formatSingleComparisonResultForModal (modalBody: Dom.Element) (result: ComparisonResult) (filterSame: string) (filterKeyString: string): unit =
        let doc = JS.Document
     
        let rec renderComparison (result: ComparisonResult) (indent: int) : Dom.Element =
            let container = doc.CreateElement("div")
            container.ClassName <- "cmp-entry"
            container.SetAttribute("style", sprintf "margin-left: %dpx;" (indent * 20))
            
            let label = doc.CreateElement("div")
            label.ClassName <- "cmp-label"
            
            let content = doc.CreateElement("div")
            content.ClassName <- "cmp-content"
            
            let addField (labelStr: string) (valueDom: Dom.Element) =
                let field = doc.CreateElement("div")
                let labelEl = doc.CreateElement("span")
                labelEl.ClassName <- "cmp-field-label"
                labelEl.TextContent <- labelStr
                field.AppendChild(labelEl) |> ignore
                field.AppendChild(valueDom) |> ignore
                content.AppendChild(field) |> ignore
            
            let appendWithToggle childrenRenderer =
                // Cast to HTMLElement to access Style and OnClick
                let toggleBtn = doc.CreateElement("button") :?> HTMLElement
                let collapsibleDiv = doc.CreateElement("div") :?> HTMLElement

                // Use indexer syntax to access CSS properties
                collapsibleDiv.Style?``display`` <- "none"
                // Set initial button text
                toggleBtn.TextContent <- "➕"
                toggleBtn.OnClick <- fun _ ->
                    // Use indexer syntax to get and set 'display'
                    if collapsibleDiv.Style?``display`` = "none" then
                        collapsibleDiv.Style?``display`` <- "block"
                        toggleBtn.TextContent <- "➖"
                    else
                        collapsibleDiv.Style?``display`` <- "none"
                        toggleBtn.TextContent <- "➕"

                // Ensure label and content are also cast if necessary before AppendChild
                (label :?> HTMLElement).AppendChild(toggleBtn) |> ignore

                childrenRenderer collapsibleDiv // Pass the already casted element

                (content :?> HTMLElement).AppendChild(collapsibleDiv) |> ignore
            
            match result with
            | PrimitiveComparison pc ->
                let show = shouldDisplayElement result.json1Value result.json2Value filterKeyString filterSame pc.same
                if not show then
                    // Halvány szín, például szürke
                    container.SetAttribute("style", "color: lightgray;")
                else
                    // Erősebb szín, például sötétzöld
                    container.SetAttribute("style", "color: darkgreen;")
                label.InnerHTML <- "<b>PrimitiveComparison</b>"
                let sameField = doc.CreateElement("span")
                sameField.TextContent <- pc.same.ToString()
                addField "Same:" sameField
                addField "JSON1:" (jsonValueToDom indent pc.json1Value)
                addField "JSON2:" (jsonValueToDom indent pc.json2Value)
            
            | ObjectComparison oc ->
                let show = shouldDisplayElement result.json1Value result.json2Value filterKeyString filterSame oc.same
                if not show then
                    // Halvány szín, például szürke
                    container.SetAttribute("style", "color: lightgray;")
                else
                    // Erősebb szín, például sötétzöld
                    container.SetAttribute("style", "color: darkgreen;")
                    
                label.InnerHTML <- "<b>ObjectComparison</b>"
                let sameField = doc.CreateElement("span")
                sameField.TextContent <- oc.actualSame.ToString()
                addField "Same:" sameField
                addField "JSON1:" (jsonValueToDom indent oc.json1Value)
                addField "JSON2:" (jsonValueToDom indent oc.json2Value)
                
                appendWithToggle (fun childContainer ->
                    for KeyValue(key, child) in oc.children do
                        let childDiv = renderComparison child (indent + 2)
                        let field = doc.CreateElement("div")
                        field.ClassName <- "cmp-field"
                        field.SetAttribute("style", sprintf "margin-left: %dpx;" ((indent + 1) * 20))
                        field.InnerHTML <- sprintf "<span class=\"cmp-key\">%s</span>" (htmlEncode key)
                        field.AppendChild(childDiv) |> ignore
                        childContainer.AppendChild(field) |> ignore
                )
            
            | ArrayComparison ac ->
                let show = shouldDisplayElement result.json1Value result.json2Value filterKeyString filterSame ac.same
                if not show then
                    // Halvány szín, például szürke
                    container.SetAttribute("style", "color: lightgray;")
                else
                    // Erősebb szín, például sötétzöld
                    container.SetAttribute("style", "color: darkgreen;")
                label.InnerHTML <- "<b>ArrayComparison</b>"
                let sameField = doc.CreateElement("span")
                sameField.TextContent <- ac.actualSame.ToString()
                addField "Same:" sameField
                addField "JSON1:" (jsonValueToDom indent ac.json1Value)
                addField "JSON2:" (jsonValueToDom indent ac.json2Value)
                
                appendWithToggle (fun childContainer ->
                    for i = 0 to ac.items.Length - 1 do
                        let itemDiv = renderComparison ac.items.[i] (indent + 2)
                        let arrayItem = doc.CreateElement("div")
                        arrayItem.ClassName <- "cmp-array-item"
                        arrayItem.SetAttribute("style", sprintf "margin-left: %dpx;" ((indent + 1) * 20))
                        arrayItem.InnerHTML <- sprintf "<span class=\"cmp-index\">%d</span>" i
                        arrayItem.AppendChild(itemDiv) |> ignore
                        childContainer.AppendChild(arrayItem) |> ignore
                )
            
            | TypeMismatchComparison tmc ->
                
                let show = shouldDisplayElement result.json1Value result.json2Value filterKeyString filterSame tmc.same
                if not show then
                    // Halvány szín, például szürke
                    container.SetAttribute("style", "color: lightgray;")
                else
                    // Erősebb szín, például sötétzöld
                    container.SetAttribute("style", "color: darkgreen;")
                
                
                label.InnerHTML <- "<b>TypeMismatchComparison</b>"
                let sameField = doc.CreateElement("span")
                sameField.TextContent <- tmc.same.ToString()
                addField "Same:" sameField
                addField "JSON1:" (jsonValueToDom indent tmc.json1Value)
                addField "JSON2:" (jsonValueToDom indent tmc.json2Value)
                
                // Külön gombokat tartalmazó konténer
                let buttonsContainer = doc.CreateElement("div") :?> HTMLElement
                buttonsContainer.SetAttribute("style", "display: inline-block; margin-left: 5px;")
                (label :?> HTMLElement).AppendChild(buttonsContainer) |> ignore
                
                // Children1 kezelése
                match tmc.children1 with
                | Some c1 ->
                    let childId1 = Math.Random().ToString().Replace(".", "")
                    let toggleBtn1 = doc.CreateElement("button") :?> HTMLElement
                    let collapsibleDiv1 = doc.CreateElement("div") :?> HTMLElement
                    
                    toggleBtn1.SetAttribute("id", sprintf "btn1_%s" childId1)
                    collapsibleDiv1.SetAttribute("id", sprintf "div1_%s" childId1)
                    
                    collapsibleDiv1.Style?``display`` <- "none"
                    toggleBtn1.TextContent <- "➕ Children1"
                    toggleBtn1.SetAttribute("style", "margin-right: 5px; background-color: #e6f7ff;")
                    
                    toggleBtn1.OnClick <- fun _ ->
                        if collapsibleDiv1.Style?``display`` = "none" then
                            collapsibleDiv1.Style?``display`` <- "block"
                            toggleBtn1.TextContent <- "➖ Children1"
                        else
                            collapsibleDiv1.Style?``display`` <- "none"
                            toggleBtn1.TextContent <- "➕ Children1"
                    
                    buttonsContainer.AppendChild(toggleBtn1) |> ignore
                    
                    let childHeader = doc.CreateElement("div")
                    childHeader.SetAttribute("style", "margin-top: 5px; font-weight: bold; color: #0066cc;")
                    childHeader.TextContent <- "Children1 Content:"
                    collapsibleDiv1.AppendChild(childHeader) |> ignore
                    
                    let childWrapper = doc.CreateElement("div") :?> HTMLElement
                    childWrapper.SetAttribute("style", "border-left: 2px solid #e6f7ff; padding-left: 10px; margin-top: 5px;")
                    childWrapper.AppendChild(renderComparison c1 (indent + 1)) |> ignore
                    
                    collapsibleDiv1.AppendChild(childWrapper) |> ignore
                    (content :?> HTMLElement).AppendChild(collapsibleDiv1) |> ignore
                | None -> Console.Log("No children1")
                
                // Children2 kezelése - teljesen külön kezelve a children1-től
                match tmc.children2 with
                | Some c2 ->
                    let childId2 = Math.Random().ToString().Replace(".", "")
                    let toggleBtn2 = doc.CreateElement("button") :?> HTMLElement
                    let collapsibleDiv2 = doc.CreateElement("div") :?> HTMLElement
                    
                    toggleBtn2.SetAttribute("id", sprintf "btn2_%s" childId2)
                    collapsibleDiv2.SetAttribute("id", sprintf "div2_%s" childId2)
                    
                    collapsibleDiv2.Style?``display`` <- "none"
                    toggleBtn2.TextContent <- "➕ Children2"
                    toggleBtn2.SetAttribute("style", "margin-right: 5px; background-color: #fff0f0;")
                    
                    toggleBtn2.OnClick <- fun _ ->
                        if collapsibleDiv2.Style?``display`` = "none" then
                            collapsibleDiv2.Style?``display`` <- "block"
                            toggleBtn2.TextContent <- "➖ Children2"
                        else
                            collapsibleDiv2.Style?``display`` <- "none"
                            toggleBtn2.TextContent <- "➕ Children2"
                    
                    buttonsContainer.AppendChild(toggleBtn2) |> ignore
                    
                    let childHeader = doc.CreateElement("div")
                    childHeader.SetAttribute("style", "margin-top: 5px; font-weight: bold; color: #cc0000;")
                    childHeader.TextContent <- "Children2 Content:"
                    collapsibleDiv2.AppendChild(childHeader) |> ignore
                    
                    let childWrapper = doc.CreateElement("div") :?> HTMLElement
                    childWrapper.SetAttribute("style", "border-left: 2px solid #fff0f0; padding-left: 10px; margin-top: 5px;")
                    childWrapper.AppendChild(renderComparison c2 (indent + 1)) |> ignore
                    
                    collapsibleDiv2.AppendChild(childWrapper) |> ignore
                    (content :?> HTMLElement).AppendChild(collapsibleDiv2) |> ignore
                | None -> Console.Log("No children2")
            
            container.AppendChild(label) |> ignore
            container.AppendChild(content) |> ignore
            container
        
        // Clear previous content
        while modalBody.HasChildNodes() do
            modalBody.RemoveChild(modalBody.FirstChild) |> ignore
        
        let root = renderComparison result 0
        modalBody.AppendChild(root) |> ignore
        
        let modal = JS.Document.QuerySelector("#jsonModal")
        modal?classList?add("show")
        modal?style?display <- "block"
        modal?show()
        modal?focus()
