# List Azure OpenAI Batch Requests

Retrieves batch request details from the Azure OpenAI Batch API.

## Usage

``` r
list_azure_openai_batches(
  .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
  .limit = 20,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .endpoint_url:

  Base URL for the API (default: Sys.getenv("AZURE_ENDPOINT_URL")).

- .limit:

  Maximum number of batches to retrieve (default: 20).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

## Value

A tibble with batch details: batch ID, status, creation time, expiration
time, and request counts (total, completed, failed).
