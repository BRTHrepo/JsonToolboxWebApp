module JsonToolboxWebApp.JsonComparator

open System.Text.Json
open JsonComparerStaticWebApp.JsonTraverser
open System.Collections.Generic

// Define the ComparisonResult type
type ComparisonResult = 
    { 
        same: bool
        json1Value: JsonValue
        json2Value: JsonValue 
    }

// Define the UnionDictionary type
type UnionDictionary = Map<string, ComparisonResult>

// Function to compare two JsonValue instances
let rec compareJsonValues (path: string) (value1: JsonValue) (value2: JsonValue) : (string * ComparisonResult) list =
    match (value1, value2) with
    | (Object obj1, Object obj2) ->
        let allKeys = 
            Set.union (obj1 |> Map.toSeq |> Seq.map fst |> Set.ofSeq) 
                      (obj2 |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

        allKeys
        |> Seq.collect (fun key ->
            let newPath = path + "." + key
            let value1 = obj1.TryFind key
            let value2 = obj2.TryFind key
            match (value1, value2) with
            | (Some v1, Some v2) -> compareJsonValues newPath v1 v2
            | (Some v1, None) -> compareJsonValues newPath v1 Null
            | (None, Some v2) -> compareJsonValues newPath Null v2
            | _ -> [])
        |> Seq.toList
        |> (fun results ->
            if List.forall (fun (_, result) -> result.same) results then
                [(path, { same = true; json1Value = value1; json2Value = value2 })]
            else
                results)

    | (Array arr1, Array arr2) ->
        let maxLength = max arr1.Length arr2.Length
        [0 .. maxLength - 1]
        |> List.collect (fun i ->
            let value1 = List.tryItem i arr1
            let value2 = List.tryItem i arr2
            match (value1, value2) with
            | (Some v1, Some v2) -> compareJsonValues $"{path}[{i}]" v1 v2
            | (Some v1, None) -> compareJsonValues $"{path}[{i}]" v1 Null
            | (None, Some v2) -> compareJsonValues $"{path}[{i}]" Null v2
            | _ -> [])
        |> (fun results ->
            if List.forall (fun (_, result) -> result.same) results then
                [(path, { same = true; json1Value = value1; json2Value = value2 })]
            else
                results)

    | _ ->
        if value1 <> value2 then
            [(path, { same = false; json1Value = value1; json2Value = value2 })]
        else
            [(path, { same = true; json1Value = value1; json2Value = value2 })]

// Function to compare two JSON dictionaries
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

