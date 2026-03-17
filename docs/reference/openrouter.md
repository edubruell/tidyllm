# OpenRouter Provider Function

The `openrouter()` function provides access to hundreds of AI models
from different providers through the OpenRouter API, using a single
OpenAI-compatible interface.

## Usage

``` r
openrouter(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters passed to the appropriate OpenRouter-specific function.

- .called_from:

  An internal argument specifying which verb invoked this function.
  Managed automatically by tidyllm verbs; do not set manually.

## Value

The result of the requested action (e.g., an updated `LLMMessage` for
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md)).
