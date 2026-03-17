# Rerank Documents Using a llama.cpp Server

Calls the `/v1/reranking` endpoint of a running llama.cpp server to
score documents by relevance to a query. Useful for building fully-local
RAG pipelines (embed → cosine search → rerank → chat, all with
[`llamacpp()`](https://edubruell.github.io/tidyllm/reference/llamacpp.md)).

## Usage

``` r
llamacpp_rerank(
  .query,
  .documents,
  .model = "local-model",
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .query:

  A single query string.

- .documents:

  A character vector of documents to rerank.

- .model:

  The model name (default: `"local-model"`).

- .server:

  Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER` env
  var, falling back to `"http://localhost:8080"`.

- .api_key:

  API key for the server (default: `LLAMACPP_API_KEY` env var).

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum retries (default: 3).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

## Value

A tibble with columns `index` (original position), `document` (text),
and `relevance_score`, sorted by descending score.
