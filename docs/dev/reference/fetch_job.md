# Fetch Results from a Batch or Research Job

`fetch_job()` dispatches to
[`fetch_batch()`](https://edubruell.github.io/tidyllm/dev/reference/fetch_batch.md)
for batch objects or
[`perplexity_fetch_research()`](https://edubruell.github.io/tidyllm/dev/reference/perplexity_fetch_research.md)
for `tidyllm_research_job` objects.

## Usage

``` r
fetch_job(.job, .provider = NULL, ...)
```

## Arguments

- .job:

  An object with a `batch_id` attribute (from
  [`send_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_batch.md))
  or a `tidyllm_research_job` (from
  `deep_research(.background = TRUE)`).

- .provider:

  A provider function (required for batch jobs, ignored for research
  jobs).

- ...:

  Additional arguments passed to the underlying function.

## Value

Fetched results; type depends on `.job` class.
