# Retrieve Assistant Reply as Structured Data

Parses the assistant's reply as JSON and returns the corresponding
structured data. If the reply is not marked as JSON, attempts to extract
and parse JSON content from the text.

## Usage

``` r
get_reply_data(.llm, .index = NULL)

last_reply_data(.llm)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message history.

- .index:

  A positive integer indicating the index of the assistant reply to
  retrieve. Defaults to `NULL`, which retrieves the last reply.

## Value

Returns the parsed data from the assistant's reply, or `NULL` if parsing
fails.

## Details

For convenience, `last_reply_data()` is provided as a wrapper to
retrieve the latest assistant reply's data.

## See also

[`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md),
`last_reply_data()`
