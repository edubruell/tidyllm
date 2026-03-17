# Alias for the OpenAI Provider Function

The `chatgpt` function is an alias for the
[`openai()`](https://edubruell.github.io/tidyllm/dev/reference/openai.md)
provider function. It provides a convenient way to interact with the
OpenAI API for tasks such as sending chat messages, generating
embeddings, and handling batch operations using `tidyllm` verbs like
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md),
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md),
and
[`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md).

## Usage

``` r
chatgpt(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate OpenAI-specific function,
  such as model configuration, input text, or other API-specific
  options.

- .called_from:

  An internal argument that specifies the context (e.g., `chat`,
  `embed`, `send_batch`) in which the function is being invoked. This is
  automatically managed and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md),
or a matrix for
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md)).
