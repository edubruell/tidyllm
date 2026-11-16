# Fetch Results for a Groq Batch

This function retrieves the results of a completed Groq batch and
updates the provided list of `LLMMessage` objects with the responses.

## Usage

``` r
fetch_groq_batch(
  .llms,
  .batch_id = NULL,
  .api_url = "https://api.groq.com/",
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects that were part of the batch.

- .batch_id:

  Character; the unique identifier for the batch.

- .api_url:

  Character; the base URL for the Groq API (default:
  "https://api.groq.com/").

- .dry_run:

  Logical; if `TRUE`, returns the constructed request without executing
  it (default: `FALSE`).

- .max_tries:

  Integer; maximum number of retries if the request fails (default:
  `3`).

- .timeout:

  Integer; request timeout in seconds (default: `60`).

## Value

A list of updated `LLMMessage` objects, each with the assistant's
response added if successful.
