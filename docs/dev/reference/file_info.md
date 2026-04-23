# Get Metadata for a File Stored on a Provider

Get Metadata for a File Stored on a Provider

## Usage

``` r
file_info(.provider, .file_id, ...)
```

## Arguments

- .provider:

  A provider function call.

- .file_id:

  The file ID string to look up, or a `tidyllm_file` object returned by
  [`upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/upload_file.md).

- ...:

  Additional provider-specific arguments.

## Value

A single-row tibble of file metadata.
