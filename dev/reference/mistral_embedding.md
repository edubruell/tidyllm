# Generate Embeddings Using Mistral API

Generate Embeddings Using Mistral API

## Usage

``` r
mistral_embedding(
  .input,
  .model = "mistral-embed",
  .timeout = 120,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .input:

  A character vector of texts to embed or an `LLMMessage` object

- .model:

  The embedding model identifier (default: "mistral-embed").

- .timeout:

  Timeout for the API request in seconds (default: 120).

- .max_tries:

  Maximum retries to peform request

- .dry_run:

  If TRUE, perform a dry run and return the request object.

## Value

A matrix where each column corresponds to the embedding of a message in
the message history.
