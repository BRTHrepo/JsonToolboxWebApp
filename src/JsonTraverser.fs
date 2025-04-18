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
        | Array of JsonValue array
        | Object of Map<string, JsonValue>
        | Null
    let isArray = function 
    | JsonValue.Array _ -> true 
    | _ -> false

    let isObject = function 
        | JsonValue.Object _ -> true 
        | _ -> false
    
    /// <summary>
    /// Parses a JSON string into a native JavaScript object and recursively traverses its structure.
    /// The function converts the JSON document into a `Map[string, JsonValue]`, where each key represents
    /// the path to a specific value in the JSON hierarchy, and the value is its corresponding `JsonValue`.
    /// </summary>
    /// <param name="jsonString">
    /// A string containing the JSON document to be parsed and traversed.
    /// </param>
    /// <returns>
    /// A `Map[string, JsonValue]` that represents the entire JSON structure:
    /// - Keys: Strings indicating the hierarchical path to each value (e.g., "root.key1.subkey2" or "array[3]").
    /// - Values: Corresponding `JsonValue` instances, representing primitive values, arrays, objects, or nulls.
    /// </returns>
    /// <remarks>
    /// The function performs the following steps:
    /// 1. **JSON Parsing**:
    ///    - Converts the input JSON string into a native JavaScript object using `JSON.parse` via inline JavaScript.
    ///
    /// 2. **Recursive Traversal**:
    ///    - Recursively processes each element in the JSON hierarchy, handling different types:
    ///      - **Primitive values** (`string`, `int`, `float`, `bool`): Directly mapped to `JsonValue` types.
    ///      - **Arrays**: Iterates through each element in the array and appends its index to the path.
    ///      - **Objects**: Iterates through all keys in the object and appends them to the current path.
    ///      - **Null values**: Represented as `Null` in the resulting map.
    ///
    /// 3. **Path Construction**:
    ///    - Dynamically constructs paths for each value in the hierarchy using dot notation for objects
    ///      (e.g., `"root.key1.subkey2"`) and bracket notation for arrays (e.g., `"array[3]"`).
    ///
    /// 4. **Error Handling**:
    ///    - Throws an error if an unsupported JSON value type is encountered.
    ///
    /// This function is particularly useful for converting deeply nested JSON documents into a flat map representation,
    /// enabling easier traversal and manipulation of JSON data.
    /// </remarks>        
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
