# Retrieve Metadata for a File from Gemini API

Retrieves metadata for a specific file uploaded to the Gemini API.

## Usage

``` r
gemini_file_metadata(.file_name)
```

## Arguments

- .file_name:

  The file ID (e.g., "files/abc-123") to retrieve metadata for.

## Value

A tibble containing metadata fields such as name, display name, MIME
type, size, and URI.
