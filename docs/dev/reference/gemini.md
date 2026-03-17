# Google Gemini Provider Function

The `gemini()` function acts as a provider interface for interacting
with the Google Gemini API through `tidyllm`'s main verbs such as
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
and
[`embed()`](https://edubruell.github.io/tidyllm/dev/reference/embed.md).
It dynamically routes requests to Gemini-specific functions like
[`gemini_chat()`](https://edubruell.github.io/tidyllm/dev/reference/gemini_chat.md)
and
[`gemini_embedding()`](https://edubruell.github.io/tidyllm/dev/reference/gemini_embedding.md)
based on the context of the call.

## Usage

``` r
gemini(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the appropriate Gemini-specific function,
  such as model configuration, input text, or API-specific options.

- .called_from:

  An internal argument specifying which action (e.g., `chat`, `embed`)
  the function is invoked from. This argument is automatically managed
  by the `tidyllm` verbs and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (e.g., an updated `LLMMessage` object for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)).

## Details

Some functions, such as
[`gemini_upload_file()`](https://edubruell.github.io/tidyllm/dev/reference/gemini_upload_file.md)
and
[`gemini_delete_file()`](https://edubruell.github.io/tidyllm/dev/reference/gemini_delete_file.md),
are specific to Gemini and do not have general verb counterparts.
