# OpenAI Provider Function

The `openai()` function acts as an interface for interacting with the
OpenAI API through main `tidyllm` verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md),
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md),
and
[`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md).
It dynamically routes requests to OpenAI-specific functions like
[`openai_chat()`](https://edubruell.github.io/tidyllm/dev/reference/openai_chat.md)
and
[`openai_embedding()`](https://edubruell.github.io/tidyllm/dev/reference/openai_embedding.md)
based on the context of the call.

## Usage

``` r
openai(..., .called_from = NULL)

openai(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters passed to the appropriate OpenAI-specific function.

- .called_from:

  Internal routing argument; do not set manually.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md),
or a matrix for
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md)).

Result of the requested action.
