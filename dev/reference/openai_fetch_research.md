# Fetch Results from a Completed OpenAI Deep Research Job

Extracts the assistant reply from a completed `tidyllm_research_job`
returned by `openai_deep_research(.background = TRUE)`.

## Usage

``` r
openai_fetch_research(.job, .max_tries = 3)
```

## Arguments

- .job:

  A `tidyllm_research_job` object. If not yet completed, polls once via
  [`openai_check_research()`](https://edubruell.github.io/tidyllm/dev/reference/openai_check_research.md)
  and errors if still incomplete.

- .max_tries:

  Maximum retries per HTTP request (default: `3`).

## Value

An updated `LLMMessage` with the research reply appended.
