# List Mistral Batch Requests

Retrieves batch request details from the OpenAI Batch API.

## Usage

``` r
list_mistral_batches(
  .limit = 100,
  .max_tries = 3,
  .timeout = 60,
  .status = NULL,
  .created_after = NULL
)
```

## Arguments

- .limit:

  Maximum number of batches to retrieve (default: 20).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

- .status:

  Filter by status. (default: NULL)

- .created_after:

  created after a string specifiying a date-time (default: NULL)

## Value

A tibble with batch details for all batches fitting the request
