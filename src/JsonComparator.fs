namespace JsonToolboxWebApp.JsonComparator


open JsonToolboxWebApp.JsonTraverser
open WebSharper // A WebSharper attribútumok használatához
[<JavaScript>] 

module JsonComparator =
    // Define the ComparisonResult type
    open JsonToolboxWebApp.JsonTraverser
    type ComparisonResult = 
        { 
            same: bool
            json1Value: JsonValue
            json2Value: JsonValue 
        }
    type UnionDictionary = Map<string, ComparisonResult>
    
    /// <summary>
    /// Recursively compares two JSON structures (`JsonValue` instances), traversing their hierarchy.
    /// The function handles various JSON types (null, objects, arrays, primitive values) and builds a list
    /// of differences and similarities between the two JSON structures.
    /// Each comparison result is represented as a tuple containing the JSON path and the comparison result.
    /// </summary>
    /// <param name="path">
    /// The current JSON path indicating the location of the key or index being compared.
    /// For example: "root.key1.subkey2" or "array[3]". The path is dynamically updated during recursion.
    /// </param>
    /// <param name="value1">
    /// The value or structure from the first JSON object to be compared.
    /// </param>
    /// <param name="value2">
    /// The value or structure from the second JSON object to be compared against the first.
    /// </param>
    /// <returns>
    /// A list of tuples, where each tuple contains:
    /// - A string representing the JSON path (e.g., "root.key1.subkey2").
    /// - A `ComparisonResult` object that includes:
    ///   - `same`: A boolean indicating whether the values are identical (`true`) or different (`false`).
    ///   - `json1Value`: The value from the first JSON object.
    ///   - `json2Value`: The value from the second JSON object.
    /// This allows detailed analysis of differences and similarities between the two JSON structures.
    /// </returns>
    /// <remarks>
    /// The function handles the following cases:
    /// 1. **Null values**:
    ///    - If both values are null, they are considered identical.
    ///    - If one value is null and the other is not, they are considered different.
    ///
    /// 2. **Objects**:
    ///    - Compares all keys in both objects by merging their key sets (`Set.union`).
    ///    - Recursively compares values associated with each key. Missing keys are treated as `Null`.
    ///
    /// 3. **Arrays**:
    ///    - Compares elements at each index up to the length of the longer array.
    ///    - Missing elements in shorter arrays are treated as `Null`.
    ///
    /// 4. **Primitive values**:
    ///    - Compares directly. If values differ, they are marked as different; otherwise, they are identical.
    ///
    /// The function uses recursion to traverse nested structures (objects and arrays) and dynamically updates
    /// the path to reflect the current location in the hierarchy.
    /// </remarks>   
    let rec compareJsonValues (path: string) (value1: JsonValue) (value2: JsonValue) : (string * ComparisonResult) list =
        match value1, value2 with
        | Null, Null ->
            [(path, { same = true; json1Value = Null; json2Value = Null })]

        | Null, _ ->
            [(path, { same = false; json1Value = Null; json2Value = value2 })]

        | _, Null ->
            [(path, { same = false; json1Value = value1; json2Value = Null })]

        | Object obj1, Object obj2 ->
            let allKeys = 
                Set.union (obj1 |> Map.toSeq |> Seq.map fst |> Set.ofSeq) 
                          (obj2 |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

            allKeys
            |> Seq.collect (fun key ->
                let newPath = path + "." + key
                let value1 = obj1.TryFind key |> Option.defaultValue Null
                let value2 = obj2.TryFind key |> Option.defaultValue Null
                compareJsonValues newPath value1 value2)
            |> Seq.toList

        | Array arr1, Array arr2 ->
            let maxLength = max arr1.Length arr2.Length
            [0 .. maxLength - 1]
            |> List.collect (fun i ->
                let value1 = List.tryItem i arr1 |> Option.defaultValue Null
                let value2 = List.tryItem i arr2 |> Option.defaultValue Null
                compareJsonValues $"{path}[{i}]" value1 value2)

        | _ ->
            if value1 <> value2 then
                [(path, { same = false; json1Value = value1; json2Value = value2 })]
            else
                [(path, { same = true; json1Value = value1; json2Value = value2 })]

    
    /// <summary>
    /// Compares two JSON dictionaries (Map[string, JsonValue]), analyzing their keys and values.
    /// The function identifies similarities and differences between the two dictionaries, handling cases
    /// where keys or values may be missing in one of the dictionaries.
    /// </summary>
    /// <param name="dict1">
    /// The first JSON dictionary to be compared. Each key represents a JSON property, and its associated value
    /// can be a primitive value, object, array, or null.
    /// </param>
    /// <param name="dict2">
    /// The second JSON dictionary to be compared. Similar to `dict1`, it contains keys and their associated values.
    /// </param>
    /// <returns>
    /// A `UnionDictionary` (represented as a `Map[string, JsonValue]`) that contains:
    /// - Keys from both dictionaries (merged using a union operation).
    /// - For each key:
    ///   - A `ComparisonResult` object that includes:
    ///     - `same`: A boolean indicating whether the values are identical (`true`) or different (`false`).
    ///     - `json1Value`: The value from the first dictionary (or `Null` if the key is missing).
    ///     - `json2Value`: The value from the second dictionary (or `Null` if the key is missing).
    ///
    /// This output allows detailed analysis of differences and similarities between the two JSON dictionaries.
    /// </returns>
    /// <remarks>
    /// The function performs the following steps:
    /// 1. **Key Union**:
    ///    - Merges all keys from both dictionaries into a single list using a union operation.
    ///    - Ensures that all keys are considered, even if they exist in only one dictionary.
    ///
    /// 2. **Value Comparison**:
    ///    - For each key:
    ///      - Retrieves the corresponding value from both dictionaries using `TryFind`.
    ///      - Handles cases where a key is missing in one of the dictionaries by treating the missing value as `Null`.
    ///
    /// 3. **Recursive Comparison**:
    ///    - Uses the `compareJsonValues` function to recursively compare values associated with each key.
    ///    - Supports nested structures like objects and arrays.
    ///
    /// 4. **Result Construction**:
    ///    - Collects all comparison results into a list of tuples (`string * ComparisonResult`) and converts it into a map (`Map[string, ComparisonResult]`).
    ///
    /// This function is particularly useful for comparing JSON objects represented as dictionaries, enabling
    /// detailed analysis of their content.
    /// </remarks>
    let compareJsonDictionaries (dict1: Map<string, JsonValue>) (dict2: Map<string, JsonValue>) : UnionDictionary =
        let allKeys = List.ofSeq dict1.Keys @ List.ofSeq dict2.Keys |> Set.ofSeq |> Set.toList
        let baseResults =
            allKeys
            |> List.collect (fun key ->
                let value1 = dict1.TryFind key
                let value2 = dict2.TryFind key
                match (value1, value2) with
                | (Some v1, Some v2) -> compareJsonValues key v1 v2
                | (Some v1, None) -> compareJsonValues key v1 Null
                | (None, Some v2) -> compareJsonValues key Null v2
                | (None, None) -> compareJsonValues key Null Null
                | _ -> [])
        baseResults |> Map.ofSeq

