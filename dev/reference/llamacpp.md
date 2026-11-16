# llama.cpp Provider Function

The `llamacpp()` provider connects tidyllm to a locally running
[llama.cpp](https://github.com/ggml-org/llama.cpp) server. It exposes
the same verb/provider pattern as every other tidyllm provider while
also offering llama.cpp-specific features: BNF grammar constraints
(`.grammar`), token logprobs (`.logprobs`), and model management
helpers.

The server must be started separately before calling `llamacpp()`. See
[`llamacpp_health()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp_health.md)
to verify the server is running, and
[`llamacpp_download_model()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp_download_model.md)
/
[`list_hf_gguf_files()`](https://edubruell.github.io/tidyllm/dev/reference/list_hf_gguf_files.md)
to obtain models.

## Usage

``` r
llamacpp(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters passed to the appropriate llama.cpp-specific function.

- .called_from:

  An internal argument specifying which verb invoked this function.
  Managed automatically by tidyllm verbs; do not set manually.

## Value

The result of the requested action (e.g., an updated `LLMMessage` for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md), a
tibble for
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md)
or
[`list_models()`](https://edubruell.github.io/tidyllm/dev/reference/list_models.md)).
