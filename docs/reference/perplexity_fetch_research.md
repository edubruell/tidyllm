# Fetch Results from a Completed Perplexity Deep Research Job

Fetch Results from a Completed Perplexity Deep Research Job

## Usage

``` r
perplexity_fetch_research(
  .job,
  .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
  .max_tries = 3
)
```

## Arguments

- .job:

  A `tidyllm_research_job` object. Must have status "completed" or will
  poll once.

- .api_key:

  Character; Perplexity API key (default: from environment).

- .max_tries:

  Integer; maximum retries (default: 3).

## Value

An updated `LLMMessage` with the research reply appended.
