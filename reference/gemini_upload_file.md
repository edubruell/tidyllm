# Upload a File to Gemini API

Uploads a file to the Gemini API and returns its metadata as a tibble.

## Usage

``` r
gemini_upload_file(.file_path)
```

## Arguments

- .file_path:

  The local file path of the file to upload.

## Value

A tibble containing metadata about the uploaded file, including its
name, URI, and MIME type.
