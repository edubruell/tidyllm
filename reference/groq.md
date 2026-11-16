# Groq API Provider Function

The `groq()` function acts as an interface for interacting with the Groq
API through `tidyllm`'s main verbs. Currently, Groq only supports
[`groq_chat()`](https://edubruell.github.io/tidyllm/reference/groq_chat.md)
for chat-based interactions and
[`groq_transcribe()`](https://edubruell.github.io/tidyllm/reference/groq_transcribe.md)
for transcription tasks.

## Usage

``` r
groq(..., .called_from = NULL)
```

## Arguments

- ...:

  Parameters to be passed to the Groq-specific function, such as model
  configuration, input text, or API-specific options.

- .called_from:

  An internal argument that specifies which action (e.g., `chat`) the
  function is being invoked from. This argument is automatically managed
  and should not be modified by the user.

## Value

The result of the requested action, depending on the specific function
invoked (currently, only an updated `LLMMessage` object for
[`groq_chat()`](https://edubruell.github.io/tidyllm/reference/groq_chat.md)).

## Details

Since
[`groq_transcribe()`](https://edubruell.github.io/tidyllm/reference/groq_transcribe.md)
is unique to Groq and does not have a general verb counterpart, `groq()`
currently routes messages only to
[`groq_chat()`](https://edubruell.github.io/tidyllm/reference/groq_chat.md)
when used with verbs like
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).
