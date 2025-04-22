namespace JsonToolboxWebApp.MergeInModal

open JsonToolboxWebApp.JsonComparator.JsonComparator
open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom

[<JavaScript>]
module MergeInModal =
    type TreeNode =
        {
            Path: string
            Name: string
            Type1: string
            Type2: string
            ValueHint1: string
            ValueHint2: string
            IsConflict: bool
            Children: TreeNode list
        }

    let private isNullOrEmpty (s: string) =
      //  JS.Inline "typeof $0 === 'string' ? $0.trim() === '' : true" s
        JS.Inline("typeof $0 === 'string' ? $0.trim() === '' : true")

 


    let buildTree (comparisonResults: Map<string, ComparisonResult>) =
        Console.Log("comparisonResults.Count " + comparisonResults.Count.ToString())
         // Segédfüggvény annak ellenőrzésére, hogy egy útvonal egy másik gyermeke-e
        let isChildPath (possibleParent: string) (possibleChild: string) =
            possibleChild.StartsWith(possibleParent + ".") || 
            possibleChild.StartsWith(possibleParent + "[")
        
        // Ellenőrzi, hogy egy útvonal gyermeke-e bármely másik útvonalnak
        let isChildOfAny (path: string) =
            comparisonResults
            |> Map.exists (fun parentPath _ -> 
                path <> parentPath && isChildPath parentPath path)
        
        // Csak a gyökér elemekre építünk fát (amelyek nem gyermekei másnak)
        comparisonResults 
        |> Map.toList 
        |> List.filter (fun (path, _) -> not (isChildOfAny path))
   //     |> List.map (fun (k, v) ->// buildTreeRecursive k k v)


    let rec private renderNodesToHtml (nodes: TreeNode list) =
        Console.Log("Rendering nodes to HTML...")
        nodes
        |> List.map (fun node ->
            let childrenHtml = renderNodesToHtml node.Children
            Console.Log($"Rendering children HTML: {childrenHtml}")
            let hasChildren = not (List.isEmpty node.Children)
            Console.Log($"Has children: {hasChildren}")
            let conflictClass = if node.IsConflict then "conflict" else ""
            Console.Log($"Rendering node: {node.Name}")
            Console.Log($"Rendering children: {childrenHtml}")
            Console.Log($"Rendering conflict class: {conflictClass}")
            let typeClass =
                match node.Type1, node.Type2 with
                | "Array", "Array" -> "node-array"
                | "Object", "Object" -> "node-object"
                | _ -> "node-primitive"
            
            let tooltip = $"Path: {node.Path}\nJSON1: {node.ValueHint1}\nJSON2: {node.ValueHint2}"
            
            let headerContent =
                if node.IsConflict then
                    $"""<span class="node-name">{node.Name}</span>
                        <span class="types">{node.Type1} | {node.Type2}</span>
                        <select class="conflict-select" data-path="{node.Path}">
                            <option value="json1">{node.ValueHint1}</option>
                            <option value="json2">{node.ValueHint2}</option>
                        </select>"""
                else
                    $"""<span class="node-name">{node.Name}</span>
                        <span class="types">{node.Type1} | {node.Type2}</span>"""
            
            $"""<li class="tree-node {typeClass} {conflictClass}" data-path="{node.Path}" title="{tooltip}">
                {headerContent}
                {if hasChildren then $"<ul class=\"nested\">{childrenHtml}</ul>" else ""}
            </li>"""
        )
        |> String.concat "\n"

    let renderTreeToHtml (tree: TreeNode list) =
        $"<ul class=\"tree-container\">{renderNodesToHtml tree}</ul>"

    let populateModal (modalBody: Dom.Element) (comparisonResults: Map<string, ComparisonResult>) =
        try
            Console.Log("Populating modal with comparison results...")
            let treeNodes = buildTree comparisonResults
       //     let generatedHtml = renderTreeToHtml treeNodes
            
            // Helyes tartalom törlése
            while modalBody.HasChildNodes() do // Zárójelek hozzáadva
                modalBody.RemoveChild(modalBody.FirstChild) |> ignore
            
            // Új tartalom hozzáadása
            let doc = JS.Document
            let fragment = doc.CreateDocumentFragment()
            let tempDiv = doc.CreateElement("div")
          //  tempDiv?innerHTML <- generatedHtml
            
            while tempDiv.FirstChild <> null do
                fragment.AppendChild(tempDiv.FirstChild) |> ignore
            
            modalBody.AppendChild(fragment) |> ignore
            
        with ex ->
            Console.Error("Error populating modal:", ex)



