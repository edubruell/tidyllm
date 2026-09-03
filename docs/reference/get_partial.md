# The text a running chat has produced so far

Non-blocking: it advances the job as far as it can right now and returns
what has arrived. On a finished job it returns the complete text, so a
polling loop needs no special case at the end. On a non-streaming job it
returns `""` until the job is done, because there is nothing to report
before then.

## Usage

``` r
get_partial(.job)
```

## Arguments

- .job:

  A `tidyllm_chat_job` from
  [`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md).

## Value

A character scalar.
