# Fetch Results for a Claude Batch

This function retrieves the results of a completed Claude batch and
updates the provided list of `LLMMessage` objects with the responses. It
aligns each response with the original request using the `custom_id`s
generated in
[`send_claude_batch()`](https://edubruell.github.io/tidyllm/reference/send_claude_batch.md).

## Usage

``` r
fetch_claude_batch(
  .llms,
  .batch_id = NULL,
  .api_url = "https://api.anthropic.com/",
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects that were part of the batch. The list
  should have names (custom IDs) set by
  [`send_claude_batch()`](https://edubruell.github.io/tidyllm/reference/send_claude_batch.md)
  to ensure correct alignment.

- .batch_id:

  Character; the unique identifier for the batch. By default this is
  NULL and the function will attempt to use the `batch_id` attribute
  from `.llms`.

- .api_url:

  Character; the base URL for the Claude API (default:
  "https://api.anthropic.com/").

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
