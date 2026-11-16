# Check Batch Processing Status for Groq API

This function retrieves the processing status and other details of a
specified Groq batch.

## Usage

``` r
check_groq_batch(
  .llms = NULL,
  .batch_id = NULL,
  .api_url = "https://api.groq.com/",
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of LLMMessage objects with a batch_id attribute.

- .batch_id:

  A character string with the batch ID to check.

- .api_url:

  Character; base URL of the Groq API (default:
  "https://api.groq.com/").

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing (default: FALSE).

- .max_tries:

  Maximum retries to perform request.

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

## Value

A tibble with information about the status of batch processing.
