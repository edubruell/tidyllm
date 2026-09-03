# Check the Status of a Job

`check_job()` reports on anything tidyllm dispatched and did not wait
for: a batch from
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md),
a background research job from `deep_research(.background = TRUE)`, or a
chat from
[`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md).

## Usage

``` r
check_job(.job, ...)
```

## Arguments

- .job:

  A `tidyllm_chat_job`, a `tidyllm_research_job`, or a batch object from
  [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md).

- ...:

  Additional arguments passed to the underlying function.

## Value

Status information; type depends on `.job`.

## Details

What it costs differs by job, and knowingly so: on a batch it is an HTTP
call to the provider, on a chat it is a turn of this session's event
loop.
