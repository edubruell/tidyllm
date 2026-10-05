# Perplexity Provider Function

The `perplexity()` function acts as a provider interface for interacting
with the Perplexity API through `tidyllm`'s
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) verb.
It dynamically routes requests to Perplxeity-specific function. At the
moment this is only
[`perplexity_chat()`](https://edubruell.github.io/tidyllm/reference/perplexity_chat.md)

## Usage

``` r
perplexity(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Perplexity-specific
  function, such as model configuration, input text, or API-specific
  options.

- .called_from:

  An internal argument specifying which action (e.g., `chat`, `embed`)
  the function is invoked from. This argument is automatically managed
  by the `tidyllm` verbs and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md)).

## Deprecated

Perplexity support is deprecated since tidyllm 0.7.0 and ends in 0.8.0.
Use `openrouter(.model = "perplexity/sonar")` for the same models, or
[`websearch_tool()`](https://edubruell.github.io/tidyllm/reference/websearch_tool.md)
to give any provider web search.
