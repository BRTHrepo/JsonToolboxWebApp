module JsonToolboxWebApp.JsonValidator

open WebSharper
open WebSharper.Json

// ValidationStatus típus definiálása
type ValidationStatus =
    | Valid
    | Invalid

// ValidationResult típus definiálása
type ValidationResult = {
    status1: ValidationStatus
    status2: ValidationStatus
    json1Document: obj option
    json2Document: obj option
}

// Egy JSON szöveg validálása
let validateJson (jsonString: string) : ValidationStatus * obj option =
    try 
        let jsonDoc = Json.Deserialize<obj>(jsonString)
        Valid, Some jsonDoc
    with _ -> Invalid, None

// Két JSON szöveg validálása és eredmény visszaadása
let validateJsonFiles (json1: string) (json2: string) : ValidationResult =
    let status1, doc1 = validateJson json1
    let status2, doc2 = validateJson json2
    
    { status1 = status1; status2 = status2; json1Document = doc1; json2Document = doc2 }


