# Ollama API Provider Function

The `ollama()` function acts as an interface for interacting with local
AI models via the Ollama API. It integrates seamlessly with the main
`tidyllm` verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
and
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md).

## Usage

``` r
ollama(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Ollama-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument specifying the verb (e.g., `chat`, `embed`) the
  function is invoked from. This argument is automatically managed by
  `tidyllm` and should not be set by the user.

## Value

The result of the requested action:

- For
  [`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md):
  An updated `LLMMessage` object containing the model's response.

- For
  [`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md):
  A matrix where each column corresponds to an embedding.

## Details

Some functionalities, like
[`ollama_download_model()`](https://edubruell.github.io/tidyllm/dev/reference/ollama_download_model.md)
or
[`ollama_list_models()`](https://edubruell.github.io/tidyllm/dev/reference/ollama_list_models.md)
are unique to the Ollama API and do not have a general verb counterpart.
These functions can be only accessed directly.

Supported Verbs:

- **[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)**:
  Sends a message to an Ollama model and retrieves the model's response.

- **[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md)**:
  Generates embeddings for input texts using an Ollama model.

- **[`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md)**:
  Behaves different than the other
  [`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md)
  verbs since it immediately processes the answers
