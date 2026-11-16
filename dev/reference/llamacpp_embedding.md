# Generate Embeddings Using a llama.cpp Server

Sends text to the `/v1/embeddings` endpoint of a running llama.cpp
server and returns embedding vectors.

## Usage

``` r
llamacpp_embedding(
  .input,
  .model = "local-model",
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3
)
```

## Arguments

- .input:

  A character vector of texts to embed, or an `LLMMessage` object.

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

## Value

A tibble with columns `input` (text) and `embeddings` (list of numeric
vectors).
