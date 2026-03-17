# Deepseek Provider Function

The `deepseek()` function acts as a provider interface for interacting
with the Deepseek API through `tidyllm`'s
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
verb. It dynamically routes requests to deepseek-specific function. At
the moment this is only
[`deepseek_chat()`](https://edubruell.github.io/tidyllm/dev/reference/deepseek_chat.md)

## Usage

``` r
deepseek(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Deepseek-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument specifying which action (e.g., `chat`, `embed`)
  the function is invoked from. This argument is automatically managed
  by the `tidyllm` verbs and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)).
