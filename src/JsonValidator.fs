
module JsonToolboxWebApp.JsonValidator


open System
open System.Text.Json

// Define the ValidationStatus type
type ValidationStatus = 
    | Valid 
    | Invalid

// Define the ValidationResult type
type ValidationResult = 
    { 
        status1: ValidationStatus 
        status2: ValidationStatus 
        json1Document: JsonDocument option 
        json2Document: JsonDocument option 
    }

// Function to validate a JSON string
let validateJson (jsonString: string) : ValidationStatus * JsonDocument option =
    try
        let jsonDoc = JsonDocument.Parse(jsonString)
        (Valid, Some jsonDoc)
    with
    | :? JsonException -> (Invalid, None)

// Function to validate two JSON strings and return a ValidationResult
let validateJsonFiles (json1: string) (json2: string) : ValidationResult =
    let status1, doc1 = validateJson json1
    let status2, doc2 = validateJson json2
    {
        status1 = status1
        status2 = status2
        json1Document = doc1
        json2Document = doc2
    }

// Example usage
let exampleJson1 = "{ \"key\": \"value\" }"
let exampleJson2 = "{ \"key\": \"value2\" }"

let result = validateJsonFiles exampleJson1 exampleJson2

