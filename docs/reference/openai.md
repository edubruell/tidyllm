# The `openai()` function acts as an interface for interacting with the OpenAI API through main `tidyllm` verbs such as `chat()`, `embed()`, and `send_batch()`. Chat uses the Responses API (`POST /v1/responses`); embeddings and batch operations use the Chat Completions / Embeddings endpoints unchanged.

The `openai()` function acts as an interface for interacting with the
OpenAI API through main `tidyllm` verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
[`embed()`](https://edubruell.github.io/tidyllm/reference/embed.md), and
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).
Chat uses the Responses API (`POST /v1/responses`); embeddings and batch
operations use the Chat Completions / Embeddings endpoints unchanged.

## Usage

``` r
openai(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters passed to the appropriate OpenAI-specific function.

- .called_from:

  Internal routing argument; do not set manually.

## Value

Result of the requested action.
