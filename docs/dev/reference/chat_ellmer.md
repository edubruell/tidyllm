# Send LLM Messages to Ellmer Chat Object

This function converts an `LLMMessage` to the turns ellmer chat object.
The ellmer object is cloned and its turn history is cleared internatlly.
This maintains tidyllms stateless style.

## Usage

``` r
chat_ellmer(
  .llm,
  .ellmer_chat,
  .stream = FALSE,
  .timeout = 60,
  .verbose = FALSE,
  .dry_run = FALSE,
  .tools = NULL,
  .max_tries = 3
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .ellmer_chat:

  An ellmer chat object (e.g., from
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)).

- .stream:

  Logical; if TRUE, streams the response piece by piece (default:
  FALSE).

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  If TRUE, displays additional information about the chat (default:
  FALSE).

- .dry_run:

  If TRUE, returns the constructed request object without executing it
  (default: FALSE).

- .tools:

  Dummy for error message if called from chat with tools (default:
  NULL).

- .max_tries:

  Maximum retries to perform the request (default: 3).

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
