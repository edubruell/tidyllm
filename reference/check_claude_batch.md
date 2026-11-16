# Check Batch Processing Status for Claude API

This function retrieves the processing status and other details of a
specified Claude batch ID from the Claude API.

## Usage

``` r
check_claude_batch(
  .llms = NULL,
  .batch_id = NULL,
  .api_url = "https://api.anthropic.com/",
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of LLMMessage objects

- .batch_id:

  A manually set batchid

- .api_url:

  Character; base URL of the Claude API (default:
  "https://api.anthropic.com/").

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .max_tries:

  Maximum retries to peform request

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

## Value

A tibble with information about the status of batch processing
