# Chat with any OpenAI-Compatible API Endpoint

Provider function for interacting with any server that implements the
OpenAI Chat Completions wire format (vLLM, LiteLLM, Together, Anyscale,
etc.). Supports the
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) verb
only.

## Usage

``` r
chat_completions_chat(
  .llm,
  .api_url,
  .api_key_env_var = NULL,
  .model = "default",
  ...
)

chat_completions(..., .called_from = NULL)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .api_url:

  Base URL for the API endpoint (required).

- .api_key_env_var:

  Name of the environment variable holding the API key.

- .model:

  The model identifier to use (default: `"default"`).

- ...:

  Additional parameters passed to the underlying chat function.

- .called_from:

  Internal routing argument; do not set manually.

## Value

An updated `LLMMessage` object with the assistant's response appended.
