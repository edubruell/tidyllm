# Check the Status of a Batch or Research Job

`check_job()` dispatches to
[`check_batch()`](https://edubruell.github.io/tidyllm/reference/check_batch.md)
for batch objects or
[`perplexity_check_research()`](https://edubruell.github.io/tidyllm/reference/perplexity_check_research.md)
for `tidyllm_research_job` objects.

## Usage

``` r
check_job(.job, ...)
```

## Arguments

- .job:

  An object with a `batch_id` attribute (from
  [`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md))
  or a `tidyllm_research_job` (from
  `deep_research(.background = TRUE)`).

- ...:

  Additional arguments passed to the underlying function.

## Value

Status information; type depends on `.job` class.
