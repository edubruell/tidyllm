# Check the Status of an OpenAI Background Research Job

Polls the status of an OpenAI background response created by
`openai_deep_research(.background = TRUE)`.

## Usage

``` r
openai_check_research(.job, .max_tries = 3)
```

## Arguments

- .job:

  A `tidyllm_research_job` object returned by
  `openai_deep_research(.background = TRUE)`.

- .max_tries:

  Maximum retries per HTTP request (default: `3`).

## Value

An updated `tidyllm_research_job` with `$status` set. If completed,
`$response` is also populated and ready for
[`fetch_job()`](https://edubruell.github.io/tidyllm/dev/reference/fetch_job.md).
