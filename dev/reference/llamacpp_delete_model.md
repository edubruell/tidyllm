# Delete a Local GGUF Model File

Deletes a GGUF file from disk. Issues a warning confirming the deletion.
Mirrors the
[`ollama_delete_model()`](https://edubruell.github.io/tidyllm/dev/reference/ollama_delete_model.md)
pattern.

## Usage

``` r
llamacpp_delete_model(.path)
```

## Arguments

- .path:

  Full path to the `.gguf` file to delete.

## Value

Invisibly returns `TRUE` on success.
