# Fetch Results for an Azure OpenAI Batch

This function retrieves the results of a completed Azure OpenAI batch
and updates the provided list of `LLMMessage` objects with the
responses. It aligns each response with the original request using the
`custom_id`s generated in
[`send_azure_openai_batch()`](https://edubruell.github.io/tidyllm/reference/send_azure_openai_batch.md).

## Usage

``` r
fetch_azure_openai_batch(
  .llms,
  .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
  .batch_id = NULL,
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects that were part of the batch.

- .endpoint_url:

  Base URL for the API (default: Sys.getenv("AZURE_ENDPOINT_URL")).

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
