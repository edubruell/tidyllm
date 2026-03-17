# Generate Embeddings Using the OpenRouter API

Sends text to an embedding model accessible via OpenRouter and returns
embedding vectors. Note that embedding models are not listed in
`list_models(openrouter())` — specify the model ID directly. Known
supported models include `"openai/text-embedding-3-small"`,
`"openai/text-embedding-3-large"`, and `"mistralai/mistral-embed"`.

## Usage

``` r
openrouter_embedding(
  .input,
  .model = "openai/text-embedding-3-small",
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3
)
```

## Arguments

- .input:

  An `LLMMessage` object or a character vector of texts to embed.

- .model:

  The embedding model ID (default: `"openai/text-embedding-3-small"`).

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
