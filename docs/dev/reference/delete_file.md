# Delete a File from a Provider's File Store

Delete a File from a Provider's File Store

## Usage

``` r
delete_file(.provider, .file_id, ...)
```

## Arguments

- .provider:

  A provider function call.

- .file_id:

  The file ID string to delete, or a `tidyllm_file` object returned by
  [`upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/upload_file.md).

- ...:

  Additional provider-specific arguments.

## Value

Invisibly NULL; prints a confirmation message.
