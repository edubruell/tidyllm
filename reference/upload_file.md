# Upload a File to a Provider's File Store

Upload a File to a Provider's File Store

## Usage

``` r
upload_file(.provider, .path, ...)
```

## Arguments

- .provider:

  A provider function call (e.g.
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md),
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)).

- .path:

  Path to the local file to upload.

- ...:

  Additional provider-specific arguments.

## Value

A `tidyllm_file` object.
