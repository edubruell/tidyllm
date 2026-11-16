# List Groq Batch Requests

Retrieves batch request details from the Groq API.

## Usage

``` r
list_groq_batches(
  .api_url = "https://api.groq.com/",
  .limit = 20,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .api_url:

  Base URL for the Groq API (default: "https://api.groq.com/").

- .limit:

  Maximum number of batches to retrieve (default: 20).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

## Value

A tibble with batch details including batch ID, status, creation time,
and request counts.
