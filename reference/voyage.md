# Voyage Provider Function

The `voyage()` function acts as a provider interface for interacting
with the Voyage.ai API through `tidyllm`'s verbs. It dynamically routes
requests to voyage-specific functions. At the moment this is only
`voyage_embed()`

## Usage

``` r
voyage(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Voyage-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument specifying which action (e.g., `embed`) the
  function is invoked from. This argument is automatically managed by
  the `tidyllm` verbs and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked
