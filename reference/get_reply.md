# Retrieve Assistant Reply as Text

Extracts the plain text content of the assistant's reply from an
`LLMMessage` object. Use
[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md)
for structured replies in JSON format.

## Usage

``` r
get_reply(.llm, .index = NULL)

last_reply(.llm)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message history, or a list of
  them.

- .index:

  A positive integer indicating the index of the assistant reply to
  retrieve. Defaults to `NULL`, which retrieves the last reply.

## Value

A character string with the assistant's reply, or `NA_character_` if no
reply exists. For a list of replies, a character vector with one element
per reply.

## Details

This function is the core utility for retrieving assistant replies by
index. For convenience, `last_reply()` is provided as a wrapper to
retrieve the latest assistant reply.

Given a list of replies, as returned by
[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
or
[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md),
it returns one string per element, in the same order. A request that
failed gives `NA`. An `.index` that is out of range for one element
stops the call.

## See also

[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md),
`last_reply()`
