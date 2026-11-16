# Provider Function for Claude models on the Anthropic API

The `claude()` function acts as an interface for interacting with the
Anthropic API through main `tidyllm` verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md), and
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).
It dynamically routes requests to Claude-specific functions like
[`claude_chat()`](https://edubruell.github.io/tidyllm/reference/claude_chat.md)
and
[`send_claude_batch()`](https://edubruell.github.io/tidyllm/reference/send_claude_batch.md)
based on the context of the call.

## Usage

``` r
claude(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate OpenAI-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument that specifies which action (e.g., `chat`,
  `send_batch`) the function is being invoked from. This argument is
  automatically managed and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md), or a
matrix for
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md)).
