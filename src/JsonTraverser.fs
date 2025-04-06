namespace JsonToolboxWebApp

open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module JsonTraverser =

    type JsonValue =
        | String of string
        | Integer of int
        | Float of float
        | Boolean of bool
        | Array of JsonValue list
        | Object of Map<string, JsonValue>
        | Null

    let traverseJsonDocument (jsonString: string) : Map<string, JsonValue> =
        // JSON string átalakítása natív JavaScript objektummá
        let jsObj = JS.Inline("JSON.parse($0)", jsonString)

        let rec traverseElement (element: obj) (path: string) : seq<string * JsonValue> =
            match element with
            | :? string as str -> seq { yield (path, String str) }
            | :? int as intVal -> seq { yield (path, Integer intVal) }
            | :? float as floatVal -> seq { yield (path, Float floatVal) }
            | :? bool as boolVal -> seq { yield (path, Boolean boolVal) }
            | :? JavaScript.Array as array ->
                // JSON tömb feldolgozása natívan Inline segítségével
                let length = JS.Inline("$0.length", array)

                Seq.init length (fun index ->
                    let item = JS.Inline("$0[$1]", array, index)
                    let newPath = path + "[" + string index + "]"
                    traverseElement item newPath)
                |> Seq.concat
            | :? JavaScript.Object as jsObj when not (isNull jsObj) ->
                // JSON objektum feldolgozása natívan Inline segítségével
                let keys = JS.Inline("Object.keys($0)", jsObj): string[]
 
                keys
                |> Seq.collect (fun key ->
                    let value = JS.Inline("$0[$1]", jsObj, key)
                    traverseElement value (if path = "" then key else path + "." + key))
            | :? JavaScript.Object as jsObj when  (isNull jsObj) ->  seq { yield (path, Null) }
            | _ -> failwith (sprintf "Unsupported JSON value type %s" path)

        traverseElement jsObj "" |> Map.ofSeq
