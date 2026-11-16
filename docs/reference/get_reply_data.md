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

  An `LLMMessage` object containing the message history, or a list of
  them.

- .index:

  A positive integer indicating the index of the assistant reply to
  retrieve. Defaults to `NULL`, which retrieves the last reply.

## Value

The parsed data from the assistant's reply, or `NULL` if parsing fails.
For a list of replies, a tibble with one row per reply.

## Details

For convenience, `last_reply_data()` is provided as a wrapper to
retrieve the latest assistant reply's data.

Given a list of replies, as returned by
[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
or
[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md),
it returns a tibble with one row per reply, in the same order. See the
section on lists below.

## A list of replies

The tibble starts with `reply_id` (the position in the list), `reply`
(the names of the list, only when it has names) and `error`. The schema
fields follow as columns; a field that holds several values becomes a
list-column. `error` is `NA` when the reply could be read. A request
that failed keeps its row, with the message of the failure in `error`
and `NA` in the fields, so the rows still match your inputs and
`filter(!is.na(error))` finds what to run again. A schema field called
`reply_id`, `reply` or `error` stops the call, because the table already
uses those names. A reply that holds an array of objects gives one row
per object, all with the same `reply_id`.

## See also

[`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md),
`last_reply_data()`
