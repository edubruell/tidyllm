# Retrieve Assistant Reply as Text

Extracts the plain text content of the assistant's reply from an
`LLMMessage` object. Use
[`get_reply_data()`](https://edubruell.github.io/tidyllm/dev/reference/get_reply_data.md)
for structured replies in JSON format.

## Usage

``` r
get_reply(.llm, .index = NULL)

last_reply(.llm)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message history.

- .index:

  A positive integer indicating the index of the assistant reply to
  retrieve. Defaults to `NULL`, which retrieves the last reply.

## Value

Returns a character string containing the assistant's reply, or
`NA_character_` if no reply exists.

## Details

This function is the core utility for retrieving assistant replies by
index. For convenience, `last_reply()` is provided as a wrapper to
retrieve the latest assistant reply.

## See also

[`get_reply_data()`](https://edubruell.github.io/tidyllm/dev/reference/get_reply_data.md),
`last_reply()`
