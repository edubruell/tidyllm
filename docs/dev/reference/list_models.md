# List Available Models for a Provider

The `list_models()` function retrieves available models from the
specified provider.

## Usage

``` r
list_models(.provider = getOption("tidyllm_lmodels_default"), ...)
```

## Arguments

- .provider:

  A function or function call specifying the provider and any additional
  parameters. You can also set a default provider via the
  `tidyllm_lmodels_default` option.

- ...:

  Additional arguments to be passed to the provider-specific list_models
  function.

## Value

A tibble containing model information.
