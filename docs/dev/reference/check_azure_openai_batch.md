# Check Batch Processing Status for Azure OpenAI Batch API

This function retrieves the processing status and other details of a
specified Azure OpenAI batch ID from the Azure OpenAI Batch API.

## Usage

``` r
check_azure_openai_batch(
  .llms = NULL,
  .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
  .batch_id = NULL,
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of LLMMessage objects.

- .endpoint_url:

  Base URL for the API (default: Sys.getenv("AZURE_ENDPOINT_URL")).

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
