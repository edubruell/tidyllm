# Run Deep Research via a Provider

The `deep_research()` function sends a message to a provider's deep
research endpoint. Currently supported: OpenAI (deep research models via
the background API).

## Usage

``` r
deep_research(.llm, .provider, .background = FALSE, ...)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the research question.

- .provider:

  A function or function call specifying the provider (e.g.,
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)).

- .background:

  Logical; if TRUE, returns a `tidyllm_research_job` immediately
  (default: FALSE).

- ...:

  Additional arguments passed to the provider's deep research function.

## Value

If `.background = FALSE`, an `LLMMessage` with the research reply. If
`.background = TRUE`, a `tidyllm_research_job` for use with
[`check_job()`](https://edubruell.github.io/tidyllm/reference/check_job.md)/[`fetch_job()`](https://edubruell.github.io/tidyllm/reference/fetch_job.md).
