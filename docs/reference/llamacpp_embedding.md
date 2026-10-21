# Generate Embeddings Using a llama.cpp Server

Sends text, images, audio or video to the `/v1/embeddings` endpoint of a
running llama.cpp server and returns embedding vectors. Media need a
multimodal embedding model, such as EmbeddingGemma 2, started with its
`--mmproj` file (for example
`llama-server -m embeddinggemma-2-Q8_0.gguf --mmproj mmproj-embeddinggemma-2-Q8_0.gguf --embeddings -c 16384 -b 16384 -ub 16384`).
A video of about 9 seconds needs a context and batch size near 16384.
The server marks audio input as experimental.

## Usage

``` r
llamacpp_embedding(
  .input,
  .model = "local-model",
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .task = NULL
)
```

## Arguments

- .input:

  A character vector of texts to embed, a list mixing texts with
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
  and
  [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md)
  objects (a nested list is one fused item), or an `LLMMessage` object.

- .model:

  The model name (default: `"local-model"`). llama.cpp ignores this and
  serves the loaded embedding model.

- .server:

  Base URL of the llama.cpp server. Defaults to the `LLAMACPP_SERVER`
  environment variable, falling back to `"http://localhost:8080"`.

- .api_key:

  API key for the server (default: `LLAMACPP_API_KEY` env var).

- .timeout:

  Request timeout in seconds (default: 120).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

- .max_tries:

  Maximum retries (default: 3).

- .task:

  `NULL` (default), `"query"` or `"document"`. Adds the EmbeddingGemma 2
  task prefix to text; media get none.

## Value

A tibble with columns `input` (text, or a label such as `[IMG] cat.jpg`)
and `embeddings` (list of numeric vectors).
