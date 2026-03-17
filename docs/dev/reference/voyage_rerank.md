# Rerank Documents Using Voyage AI API

Rerank Documents Using Voyage AI API

## Usage

``` r
voyage_rerank(
  .query,
  .documents,
  .model = "rerank-2",
  .top_k = NULL,
  .api_key = Sys.getenv("VOYAGE_API_KEY"),
  .timeout = 60,
  .max_tries = 3
)
```

## Arguments

- .query:

  A single character string representing the search query.

- .documents:

  A character vector of documents to rerank.

- .model:

  The reranking model identifier (default: "rerank-2").

- .top_k:

  Integer; return only the top-k results (default: NULL, returns all).

- .api_key:

  Character; Voyage API key (default: from environment).

- .timeout:

  Integer; request timeout in seconds (default: 60).

- .max_tries:

  Integer; maximum retries (default: 3).

## Value

A tibble with columns `index`, `document`, and `relevance_score`, sorted
by score descending.
