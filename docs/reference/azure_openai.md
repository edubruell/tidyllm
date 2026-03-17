# Azure OpenAI Endpoint Provider Function

The `azure_openai()` function acts as an interface for interacting with
the Azure OpenAI API through main `tidyllm` verbs.

## Usage

``` r
azure_openai(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the Azure OpenAI API specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument that specifies which action (e.g., `chat`) the
  function is being invoked from. This argument is automatically managed
  and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (currently, only an updated `LLMMessage` object for
[`azure_openai_chat()`](https://edubruell.github.io/tidyllm/reference/azure_openai_chat.md)).

## Details

`azure_openai()` currently routes messages only to
[`azure_openai_chat()`](https://edubruell.github.io/tidyllm/reference/azure_openai_chat.md)
when used with
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).

[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).
It dynamically routes requests to OpenAI-specific functions like
[`azure_openai_chat()`](https://edubruell.github.io/tidyllm/reference/azure_openai_chat.md)
and
[`azure_openai_embedding()`](https://edubruell.github.io/tidyllm/reference/azure_openai_embedding.md)
based on the context of the call.
