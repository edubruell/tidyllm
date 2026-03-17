# List Local GGUF Model Files

Scans a directory for `.gguf` files and returns a tibble with their
names, sizes, and modification times. No server needed.

## Usage

``` r
llamacpp_list_local_models(
  .path = Sys.getenv("LLAMACPP_MODEL_DIR", "~/models")
)
```

## Arguments

- .path:

  Directory to scan (default: `LLAMACPP_MODEL_DIR` env var, then
  `"~/models"`). The path is expanded with
  [`path.expand()`](https://rdrr.io/r/base/path.expand.html).

## Value

A tibble with columns `filename`, `size_gb`, `modified`, and `path`.
