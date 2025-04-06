# JsonToolboxWebApp

## Project Overview

JsonToolboxWebApp is a web application designed to process, validate,
and compare JSON files.
Built with F# and WebSharper, the application provides a user-friendly interface for JSON analysis.

---

## Features

### 1. JSON Traverser
- Recursively navigates through JSON structures.
- Handles nested objects and arrays efficiently.

### 2. JSON Validator
- Validates the syntax and structure of JSON files.
- Provides meaningful error messages for invalid JSON inputs.

### 3. JSON Comparator
- Compares two JSON files recursively.
- Identifies differences in keys, values, and array sizes.
- Outputs discrepancies in a structured format (`UnionDictionary`).

### 4. Customizable Output
- Allows filtering of comparison results based on the `same` property:
    - Show all results.
    - Show only identical entries (`same = true`).
    - Show only differing entries (`same = false`).
- Dynamically updates the output when the filter changes.

### 5. JavaScript Object Null Handling
- Ensures that `null` values are not misclassified as objects during traversal.
- Added explicit checks to correctly identify and process `null` values at all recursive levels.

---

## File Structure

### Key Files
1. **JsonTraverser.fs**:
    - Contains functions for parsing and traversing JSON files.
    - Handles recursive navigation of objects and arrays.

2. **JsonComparator.fs**:
    - Implements recursive comparison logic for JSON files.
    - Defines types like `ComparisonResult` and `UnionDictionary`.

3. **Client.fs**:
    - Manages client-side functionality, including file input handling and dynamic UI updates.
    - Integrates filtering logic for comparison results.

4. **Main.fs**:
    - Server-side entry point for the application.
    - Handles routing, templates, and rendering.

5. **Main.html**:
    - Defines the web interface with sections for file input, target selection, and comparison result display.

6. **wsconfig.json**:
    - Configuration file specifying project settings (e.g., output directory).

---

## How It Works

### 1. Validation
The `traverseJsonDocument` function reads and parses JSON files using JavaScript's `JSON.parse`. Invalid files result in descriptive error messages.

### 2. Traversing
The `traverseElement` function recursively processes JSON structures:
- Supports strings, numbers, booleans, arrays, objects, and `null`.
- Ensures robust handling of edge cases like `null`.

### 3. Comparison
The `compareJsonValues` function compares two JSON structures recursively:
- Identifies differences in keys, values, and array sizes.
- Outputs results as a `UnionDictionary`.

### 4. Filtering
The filtering logic dynamically adjusts the displayed results based on user selection:
- Filters by `same = true`, `same = false`, or shows all entries.
- Updates output in real-time when the filter changes.

---

## Dependencies

1. **WebSharper**:
    - Provides tools for building reactive web applications in F#.

2. **JavaScript Native JSON Handling**:
    - Uses JavaScript's native `JSON.parse` for parsing and processing JSON data directly in the browser environment.

3. **FSharp.Formatting** (optional):
    - Enables Markdown parsing for rendering documentation dynamically (if used).

---

## Usage

### Installation
1. Clone the repository:

