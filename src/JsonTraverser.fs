namespace JsonToolboxWebApp

open WebSharper
open WebSharper.JavaScript

[<JavaScript>]
module JsonTraverser =

    type JsonValue =
        | Primitive of key: string * value: obj
        | Object of key: string * fields: JsonValue list
        | Array of key: string * items: JsonValue list
        | Null of key: string 
    let getKey (field: JsonValue) =
                match field with
                | Primitive(k, _) -> Some k
                | Object(k, _) -> Some k
                | Array(k, _) -> Some k
                | Null(k) -> Some k
    let getValue (field: JsonValue) =
                match field with
                | Primitive(_, v) -> Some v
                | Object(_, v) -> Some v
                | Array(_, v) -> Some v
                | Null(_) -> None
    let getPrimitiveValueString (field: JsonValue) =
                match field with
                | Primitive(_, v) ->  v.ToString()
                | Object(_, v) -> ""
                | Array(_, v) -> ""
                | Null(_) -> ""            
                
    let rec traverseJsonDocument (jsonString: string) : JsonValue =
        let jsObj = JS.Inline("JSON.parse($0)", jsonString)

        let rec traverse (key: string) (value: obj) : JsonValue =
            if isNull value then 
                Null(key)
            else
                match JS.Inline("typeof $0", value) with
                | "string"  -> Primitive(key, unbox<string> value)
                | "number"  -> 
                    if JS.Inline("Number.isInteger($0)", value) then
                        Primitive(key, unbox<int> value)
                    else
                        Primitive(key, unbox<float> value)
                | "boolean" -> Primitive(key, unbox<bool> value)
                | "object" ->
                    if JS.Inline("Array.isArray($0)", value) then
                        let arr = unbox<obj[]> value
                        let items = 
                            [ for i in 0 .. arr.Length - 1 -> traverse $"[{i}]" arr.[i] ]
                        Array(key, items)
                    else
                        let keys = JS.Inline<string[]>("Object.keys($0)", value)
                        let fields = 
                            [ for k in keys -> traverse k (JS.Inline("$0[$1]", value, k)) ]
                        Object(key, fields)
                | _ -> Null(key)

        traverse "root" jsObj

