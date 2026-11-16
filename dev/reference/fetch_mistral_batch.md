# Fetch Results for an Mistral Batch

This function retrieves the results of a completed Mistral batch and
updates the provided list of `LLMMessage` objects with the responses. It
aligns each response with the original request using the `custom_id`s
generated in
[`send_mistral_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_mistral_batch.md).

## Usage

``` r
fetch_mistral_batch(
  .llms,
  .batch_id = NULL,
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects that were part of the batch.

- .batch_id:

  Character; the unique identifier for the batch. By default this is
  NULL and the function will attempt to use the `batch_id` attribute
  from `.llms`.

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
