# Check Batch Processing Status for OpenAI Batch API

This function retrieves the processing status and other details of a
specified OpenAI batch ID from the OpenAI Batch API.

## Usage

``` r
check_openai_batch(
  .llms = NULL,
  .batch_id = NULL,
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of LLMMessage objects.

- .batch_id:

  A manually set batch ID.

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .max_tries:

  Maximum retries to perform the request (default: 3).

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

## Value

A tibble with information about the status of batch processing.
