# List all Batch Requests on a Batch API

List all Batch Requests on a Batch API

## Usage

``` r
list_batches(.provider = getOption("tidyllm_lbatch_default"))
```

## Arguments

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/dev/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/dev/reference/claude.md),
  etc. You can also set a default provider function via the
  `tidyllm_lbatch_default` option.

## Value

A tibble with information about the status of batch processing.
