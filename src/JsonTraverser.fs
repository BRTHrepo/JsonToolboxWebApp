

module JsonToolboxWebApp.JsonTraverser 

    open System
    open System.Text.Json

     type JsonValue = 
        | String of string
        | Integer of int
        | Float of float
        | Boolean of bool
        | Array of JsonValue list
        | Object of Map<string, JsonValue>
        | Null
        
    let JsonObject = Map<string, JsonValue>

    // Function to convert a JsonElement to a JsonValue
    let rec jsonElementToJsonValue (element: JsonElement) : JsonValue =
        match element.ValueKind with
        | JsonValueKind.String -> String (element.GetString())
        | JsonValueKind.True -> Boolean true
        | JsonValueKind.False -> Boolean false
        | JsonValueKind.Number when element.TryGetInt32() |> fst ->
            Integer (element.GetInt32())
        | JsonValueKind.Number ->
            Float (element.GetDouble())
        | JsonValueKind.Array ->
            Array (element.EnumerateArray()
                   |> Seq.map jsonElementToJsonValue
                   |> List.ofSeq)
        | JsonValueKind.Object ->
            Object (element.EnumerateObject()
                    |> Seq.map (fun prop -> prop.Name, jsonElementToJsonValue prop.Value)
                    |> Map.ofSeq)
        | JsonValueKind.Null -> Null
        | _ -> failwith "Unsupported JSON value kind"


    // Function to traverse a JsonDocument and return a dictionary of key-value pairs
    let traverseJsonDocument (jsonDoc: JsonDocument) : Map<string, JsonValue> =
        let root = jsonDoc.RootElement
        let rec traverseElement (element: JsonElement) (path: string) =
            match element.ValueKind with
            | JsonValueKind.Object ->
                element.EnumerateObject()
                |> Seq.map (fun prop ->
                    let newPath = if String.IsNullOrEmpty path then prop.Name else path + "." + prop.Name
                    (newPath, jsonElementToJsonValue prop.Value))
                |> Map.ofSeq
            | _ -> Map.ofList [(path, jsonElementToJsonValue element)]
        
        traverseElement root ""


