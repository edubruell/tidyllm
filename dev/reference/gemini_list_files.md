# List Files in Gemini API

Lists metadata for files uploaded to the Gemini API, supporting
pagination.

## Usage

``` r
gemini_list_files(.page_size = 10, .page_token = NULL)
```

## Arguments

- .page_size:

  The maximum number of files to return per page (default: 10, maximum:
  100).

- .page_token:

  A token for fetching the next page of results (default: NULL).

## Value

A tibble containing metadata for each file, including fields such as
name, display name, MIME type, and URI.
