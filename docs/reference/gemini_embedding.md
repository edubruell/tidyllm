# Generate Embeddings Using the Google Gemini API

Generate Embeddings Using the Google Gemini API

## Usage

``` r
gemini_embedding(
  .input,
  .model = "gemini-embedding-2-preview",
  .truncate = TRUE,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3
)
```

## Arguments

- .input:

  A character vector of texts to embed or an `LLMMessage` object

- .model:

  The embedding model identifier (default: "text-embedding-3-small").

- .truncate:

  Whether to truncate inputs to fit the model's context length (default:
  TRUE).

- .timeout:

  Timeout for the API request in seconds (default: 120).

- .dry_run:

  If TRUE, perform a dry run and return the request object.

- .max_tries:

  Maximum retry attempts for requests (default: 3).

## Value

A matrix where each column corresponds to the embedding of a message in
the message history.
