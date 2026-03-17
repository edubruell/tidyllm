# Download a GGUF Model from Hugging Face

Downloads a single GGUF file from a Hugging Face repository with a
streaming progress bar. Use
[`list_hf_gguf_files()`](https://edubruell.github.io/tidyllm/reference/list_hf_gguf_files.md)
first to browse available quantizations.

## Usage

``` r
llamacpp_download_model(
  .repo,
  .filename,
  .dir = Sys.getenv("LLAMACPP_MODEL_DIR", "~/models"),
  .timeout = 3600
)
```

## Arguments

- .repo:

  Hugging Face repository ID (e.g. `"Qwen/Qwen3-8B-GGUF"`).

- .filename:

  The exact filename to download (e.g. `"Qwen3-8B-Q4_K_M.gguf"`). Use
  `list_hf_gguf_files(.repo)` to see what is available.

- .dir:

  Destination directory. Defaults to `LLAMACPP_MODEL_DIR` env var, then
  `"~/models"`. Created if it does not exist.

- .timeout:

  Download timeout in seconds (default: 3600).

## Value

Invisibly returns the full path of the downloaded file.
