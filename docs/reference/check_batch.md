# Check Batch Processing Status

This function retrieves the processing status and other details of a
specified batchid or a list of `LLMMessage` objects with batch
attribute. It routes the input to the appropriate provider-specific
batch API function.

## Usage

``` r
check_batch(
  .llms,
  .provider = getOption("tidyllm_cbatch_default"),
  .dry_run = NULL,
  .max_tries = NULL,
  .timeout = NULL
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects or a character vector with a batch ID.

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md),
  etc. You can also set a default provider function via the
  `tidyllm_cbatch_default` option.

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it

- .max_tries:

  Maximum retries to perform the request

- .timeout:

  Integer specifying the request timeout in seconds

## Value

A tibble with information about the status of batch processing.
