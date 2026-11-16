# Fetch Results from a Batch API

This function retrieves the results of a completed batch and updates the
provided list of `LLMMessage` objects with the responses. It aligns each
response with the original request using the `custom_id`s generated in
[`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md).

## Usage

``` r
fetch_batch(
  .llms,
  .provider = getOption("tidyllm_fbatch_default"),
  .dry_run = NULL,
  .max_tries = NULL,
  .timeout = NULL
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects containing conversation histories.

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/dev/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/dev/reference/claude.md),
  etc. You can also set a default provider function via the
  `tidyllm_fbatch_default` option.

- .dry_run:

  Logical; if `TRUE`, returns the constructed request without executing
  it

- .max_tries:

  Integer; maximum number of retries if the request fails

- .timeout:

  Integer; request timeout in seconds

## Value

A list of updated `LLMMessage` objects, each with the assistant's
response added if successful.

## Details

The function routes the input to the appropriate provider-specific batch
API function.
