# Mistral Provider Function

The `mistral()` function acts as an interface for interacting with the
Mistral API through main `tidyllm` verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) and
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md). It
dynamically routes requests to Mistral-specific functions like
[`mistral_chat()`](https://edubruell.github.io/tidyllm/reference/mistral_chat.md)
and
[`mistral_embedding()`](https://edubruell.github.io/tidyllm/reference/mistral_embedding.md)
based on the context of the call.

## Usage

``` r
mistral(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Mistral-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument that specifies which action (e.g., `chat`,
  `embed`, `send_batch`) the function is being invoked from. This
  argument is automatically managed and should not be modified by the
  user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md), or a
matrix for
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md)).
