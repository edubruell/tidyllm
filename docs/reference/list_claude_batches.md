# List Claude Batch Requests

Retrieves batch request details from the Claude API.

## Usage

``` r
list_claude_batches(
  .api_url = "https://api.anthropic.com/",
  .limit = 20,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .api_url:

  Base URL for the Claude API (default: "https://api.anthropic.com/").

- .limit:

  Maximum number of batches to retrieve (default: 20).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

## Value

A tibble with batch details: batch ID, status, creation time, expiration
time, and request counts (succeeded, errored, expired, canceled).
