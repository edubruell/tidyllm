# Generate Embeddings Using OpenAI API

Generate Embeddings Using OpenAI API

## Usage

``` r
openai_embedding(
  .input,
  .model = "text-embedding-3-small",
  .truncate = TRUE,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .verbose = FALSE
)
```

## Arguments

- .input:

  An existing LLMMessage object (or a character vector of texts to
  embed)

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

- .verbose:

  Should information about current ratelimits be printed? (default:
  FALSE)

## Value

A tibble with two columns: `input` and `embeddings`. The `input` column
contains the texts sent to embed, and the `embeddings` column is a list
column where each row contains an embedding vector of the sent input.
