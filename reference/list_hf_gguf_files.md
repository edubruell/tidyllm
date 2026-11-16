# List GGUF Files Available in a Hugging Face Repository

Calls the Hugging Face Hub API to list all `.gguf` files in a
repository, along with their sizes. Useful for choosing a quantization
level before calling
[`llamacpp_download_model()`](https://edubruell.github.io/tidyllm/reference/llamacpp_download_model.md).

## Usage

``` r
list_hf_gguf_files(.repo, .timeout = 30)
```

## Arguments

- .repo:

  Hugging Face repository ID in `"owner/model"` format (e.g.
  `"Qwen/Qwen3-8B-GGUF"`).

- .timeout:

  Request timeout in seconds (default: 30).

## Value

A tibble with columns `filename`, `size_gb`, and `url`.
