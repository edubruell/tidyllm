# Check the Status of a Perplexity Deep Research Job

Check the Status of a Perplexity Deep Research Job

## Usage

``` r
perplexity_check_research(
  .job,
  .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
  .max_tries = 3
)
```

## Arguments

- .job:

  A `tidyllm_research_job` object returned by
  `perplexity_deep_research(.background = TRUE)`.

- .api_key:

  Character; Perplexity API key (default: from environment).

- .max_tries:

  Integer; maximum retries (default: 3).

## Value

An updated `tidyllm_research_job` with a `$status` field and `$response`
if completed.
