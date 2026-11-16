# List OpenAI Batch Requests

Retrieves batch request details from the OpenAI Batch API.

## Usage

``` r
list_openai_batches(.limit = 20, .max_tries = 3, .timeout = 60)
```

## Arguments

- .limit:

  Maximum number of batches to retrieve (default: 20).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

## Value

A tibble with batch details: batch ID, status, creation time, expiration
time, and request counts (total, completed, failed).
