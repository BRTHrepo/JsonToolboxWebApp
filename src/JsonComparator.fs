namespace JsonToolboxWebApp.JsonComparator

open System
open JsonToolboxWebApp.JsonTraverser
open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module JsonComparator =

    type ComparisonResult =
        | ObjectComparison of ObjectComparisonResult
        | ArrayComparison of ArrayComparisonResult
        | PrimitiveComparison of PrimitiveComparisonResult
        | TypeMismatchComparison of TypeMismatchComparisonResult
        
        member this.same =
            match this with
            | ObjectComparison oc -> oc.actualSame
            | ArrayComparison ac -> ac.actualSame
            | PrimitiveComparison pc -> pc.same
            | TypeMismatchComparison tcr -> tcr.same
        
        member this.forMerge =
            match this with
            | ObjectComparison oc -> oc.forMerge
            | ArrayComparison ac -> ac.forMerge
            | PrimitiveComparison pc -> pc.forMerge
            | TypeMismatchComparison tcr -> tcr.forMerge
            
        member this.json1Value =
            match this with
            | ObjectComparison oc -> oc.json1Value
            | ArrayComparison ac -> ac.json1Value
            | PrimitiveComparison pc -> pc.json1Value
            | TypeMismatchComparison tcr -> tcr.json1Value
            
        member this.json2Value =
            match this with
            | ObjectComparison oc -> oc.json2Value
            | ArrayComparison ac -> ac.json2Value
            | PrimitiveComparison pc -> pc.json2Value
            | TypeMismatchComparison tcr -> tcr.json2Value
        member this.children1 =
            match this with
            | TypeMismatchComparison tcr -> tcr.children1
            | _ -> None
        member this.children2 =
            match this with
            | TypeMismatchComparison tcr -> tcr.children2
            | _ -> None   
            
    and PrimitiveComparisonResult = {
        same: bool
        json1Value: JsonValue
        json2Value: JsonValue
        forMerge: JsonValue
    }

    and ObjectComparisonResult = {
        same: bool
        json1Value: JsonValue
        json2Value: JsonValue
        forMerge: JsonValue
        children: Map<string, ComparisonResult>
    } with
          member this.actualSame =
            Console.WriteLine("Comparing objects: {0} = {1}", this.json1Value, this.json2Value)
            if not this.same then false
            else
                let mutable allChildrenSame = true
                for KeyValue(_, value) in this.children do
                    if not value.same then
                        allChildrenSame <- false
                allChildrenSame

    and ArrayComparisonResult = {
        same: bool
        json1Value: JsonValue
        json2Value: JsonValue
        forMerge: JsonValue
        items: ComparisonResult array
    } with
         member this.actualSame =
            Console.WriteLine("Comparing arrays: {0} = {1}", this.json1Value, this.json2Value)
            if not this.same then false
            else
                let mutable allItemsSame = true
                for item in this.items do
                    if not item.same then
                        allItemsSame <- false
                allItemsSame

    and TypeMismatchComparisonResult = {
        same: bool
        json1Value: JsonValue
        json2Value: JsonValue
        forMerge: JsonValue
        children1: ComparisonResult option
        children2: ComparisonResult option
    }

    let private toArrayItems (v: JsonValue) : JsonValue list =
        Console.WriteLine("Converting to array items: {0}", v)
        match v with
        | Array(_, items) -> items
        | Object(_, fields) -> fields
        | Null(_) -> []
        | Primitive(_, _) -> [v]

    let private toObjectFields (v: JsonValue) : JsonValue list =
        Console.WriteLine("Converting to object fields: {0}", v)
        match v with
        | Object(_, fields) -> fields
        | Array(_, items) -> items |> List.mapi (fun i item -> Primitive(sprintf "[%d]" i, item))
        | Null(_) -> []
        | Primitive(k, v) -> [Primitive(k, v)]

       // Kulcs kinyerő segédfüggvény
  
    let rec compareJsonTrees (json1: JsonValue) (json2: JsonValue) : ComparisonResult =
        match json1, json2 with
        | Primitive(k1, v1), Primitive(k2, v2) when k1 = k2 ->
            Console.WriteLine("Comparing primitives: {0} = {1}", k1, k2)
            let same = v1 = v2
            PrimitiveComparison {
                same = same
                json1Value = Primitive(k1, v1)
                json2Value = Primitive(k2, v2)
                forMerge = if same then Primitive(k1, v1) else Null(k1)
            }
            
        | Primitive(k, v), Null(k2) ->
            Console.WriteLine("Comparing nulls: {0} = {1}", k, k2)
            PrimitiveComparison {
                same = false
                json1Value = Primitive(k, v)
                json2Value = Null(k2)
                forMerge = Primitive(k, v)
            }
            
        | Null(k), Primitive(k2, v) ->
            Console.WriteLine("Comparing nulls: {0} = {1}", k, k2)
            PrimitiveComparison {
                same = false
                json1Value = Null(k)
                json2Value = Primitive(k2, v)
                forMerge = Primitive(k2, v)
            }
            
        | Null(k1), Array(k2, items2) ->
            let children2 = Some(compareJsonTrees json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = None
                children2 = children2  // Some értéket kap
            }
        | Array(k1, items1), Null(k2) ->
            let children1 = Some(compareJsonTrees json1 (json1))
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1  // Some értéket kap
                children2 = None
            }
        | Null(k1), Object(k2, fields2) ->
            let children2 = Some(compareJsonTrees json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = None
                children2 = children2  // Some értéket kap
            }
        | Object(k1, fields1), Null(k2) ->
            let children1 = Some(compareJsonTrees json1 json1)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1  // Some értéket kap
                children2 = None
            }

        | Object(k1, fields1), Object(k2, fields2) when k1 = k2 ->
            Console.WriteLine("Comparing objects: {0} = {1}", k1, k2)

         

            // Minden kulcs kinyerése
            let allKeys1 =
                fields1
                |> List.choose getKey
                |> List.distinct

            let allKeys2 =
                fields2
                |> List.choose getKey
                |> List.distinct

            // Mezők összehasonlítása
            let children =
                (allKeys1 @ allKeys2)
                |> List.fold (fun acc key ->
                    // Mezők keresése kulcs alapján
                    let f1 = fields1 |> List.tryFind (function 
                        | Primitive(k', _) when k' = key -> true
                        | Object(k', _) when k' = key -> true
                        | Array(k', _) when k' = key -> true
                        | Null(k') when k' = key -> true
                        | _ -> false)

                    let f2 = fields2 |> List.tryFind (function 
                        | Primitive(k', _) when k' = key -> true
                        | Object(k', _) when k' = key -> true
                        | Array(k', _) when k' = key -> true
                        | Null(k') when k' = key -> true
                        | _ -> false)

                    let comparison =
                        match f1, f2 with
                        | Some v1, Some v2 -> compareJsonTrees v1 v2
                        | Some v1, None -> 
                             match v1 with
                                | Object(_, _) -> compareJsonTrees v1 (Null(key))  // Rekurzív objektum-összehasonlítás
                                | Array(_, _) -> compareJsonTrees v1 (Null(key))   // Rekurzív tömb-összehasonlítás
                                | _ -> PrimitiveComparison { same = false; json1Value = v1; json2Value = Null(key); forMerge = v1 }
                        | None, Some v2 -> PrimitiveComparison { same = false; json1Value = Null(key); json2Value = v2; forMerge = v2 }
                        | None, None -> failwith "Unexpected empty key"

                    Map.add key comparison acc
                ) Map.empty

            // Check if key sets are identical
            let keysMatch = allKeys1 = allKeys2

            ObjectComparison {
                same = keysMatch  // Keys must match exactly for base same to be true
                json1Value = JsonValue.Object(k1, fields1)
                json2Value = JsonValue.Object(k2, fields2)
                forMerge = JsonValue.Object(k1, fields1)
                children = children
            }

            
        | Array(k1, items1), Array(k2, items2) when k1 = k2 ->
            Console.WriteLine("Comparing arrays: {0} = {1}", k1, k2)
            let lengthMatch = List.length items1 = List.length items2
            
            let maxLength = max (List.length items1) (List.length items2)
            let padded1 = [ for i in 0 .. maxLength - 1 -> if i < List.length items1 then items1.[i] else Null("padded") ]
            let padded2 = [ for i in 0 .. maxLength - 1 -> if i < List.length items2 then items2.[i] else Null("padded") ]
            
            let itemsComparison = List.map2 compareJsonTrees padded1 padded2
            
            ArrayComparison {
                same = lengthMatch  // Lengths must match for base same to be true
                json1Value = JsonValue.Array(k1, items1)
                json2Value = JsonValue.Array(k2, items2)
                forMerge = JsonValue.Array(k1, items1)
                items = itemsComparison |> List.toArray
            }

        | Object(k1, fields1), Array(k2, items2) ->
            Console.WriteLine("Comparing objects: {0} = {1}", k1, k2)
            // Object vs Array típus eltérés
            let children1 = Some(compareJsonTrees json1 json1)
            let children2 = Some(compareJsonTrees json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1
                children2 = children2
            }
            
        | Array(k1, items1), Object(k2, fields2) ->
            Console.WriteLine("Comparing arrays: {0} = {1}", k1, k2)
            // Array vs Object típus eltérés
            let children1 = Some(compareJsonTrees json1 json1)
            let children2 = Some(compareJsonTrees json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1
                children2 = children2
            }
            
        | Primitive(k1, v1), Object(k2, fields2) ->
            Console.WriteLine("Comparing primitives: {0} = {1}", k1, k2)
            // Primitive vs Object típus eltérés
            let children2 = Some(compareJsonTrees json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = None
                children2 = children2
            }
            
        | Object(k1, fields1), Primitive(k2, v2) ->
            Console.WriteLine("Comparing primitives: {0} = {1}", k1, k2)
            // Object vs Primitive típus eltérés
            let children1 = Some(compareJsonTrees json1  json1)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1
                children2 = None
            }
            
        | Primitive(k1, v1), Array(k2, items2) ->
            Console.WriteLine("Comparing primitives: {0} = {1}", k1, k2)
            // Primitive vs Array típus eltérés
            let children2 = Some(compareJsonTrees  json2 json2)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = None
                children2 = children2
            }
            
        | Array(k1, items1), Primitive(k2, v2) ->
            Console.WriteLine("Comparing arrays: {0} = {1}", k1, k2)
            // Array vs Primitive típus eltérés
            let children1 = Some(compareJsonTrees json1  json1)
            
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = children1
                children2 = None
            }
            
        | Null(k1), Null(k2) ->
            Console.WriteLine("Comparing nulls: {0} = {1}", k1, k2)
            // Mindkettő null
            PrimitiveComparison {
                same = true
                json1Value = json1
                json2Value = json2
                forMerge = json1
            }
            
        | _ ->
            Console.WriteLine("Comparing different types: {0} = {1}", json1, json2)
            // Egyéb típus eltérések
            TypeMismatchComparison {
                same = false
                json1Value = json1
                json2Value = json2
                forMerge = Null("typeMismatch")
                children1 = None
                children2 = None
            }
