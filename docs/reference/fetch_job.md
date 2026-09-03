# Fetch the Results of a Job

`fetch_job()` collects what
[`check_job()`](https://edubruell.github.io/tidyllm/reference/check_job.md)
reports on: the messages of a finished batch, the report of a background
research job, or the `LLMMessage` of a chat from
[`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md).
On a chat it waits for the reply if it has not arrived yet.

## Usage

``` r
fetch_job(.job, .provider = NULL, ...)
```

## Arguments

- .job:

  A `tidyllm_chat_job`, a `tidyllm_research_job`, or a batch object from
  [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).

- .provider:

  A provider function; required for batch jobs and ignored by the
  others.

- ...:

  Additional arguments passed to the underlying function.

## Value

Fetched results; type depends on `.job`.
