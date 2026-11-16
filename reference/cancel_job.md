# Stop a running chat

Closes the connection and marks the job cancelled. A cancelled job
cannot be fetched;
[`fetch_job()`](https://edubruell.github.io/tidyllm/reference/fetch_job.md)
on one raises rather than waiting forever.

## Usage

``` r
cancel_job(.job)
```

## Arguments

- .job:

  A `tidyllm_chat_job` from
  [`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md).

## Value

The job, invisibly.
