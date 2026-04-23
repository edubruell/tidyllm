# Submit a Deep Research Request to OpenAI

Sends a research request to OpenAI using the deep research models
(`o3-deep-research` or `o4-mini-deep-research`) via the Responses API
with `background: true`. The model autonomously searches the web and
synthesises a long-form answer, which can take 5-30 minutes.

## Usage

``` r
openai_deep_research(
  .llm,
  .model = "o4-mini-deep-research",
  .background = FALSE,
  .reasoning_effort = "medium",
  .json_schema = NULL,
  .max_output_tokens = NULL,
  .timeout = 1800,
  .max_tries = 3
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the research question.

- .model:

  The deep research model to use (default: `"o4-mini-deep-research"`).

- .background:

  Logical; if `TRUE`, returns a `tidyllm_research_job` immediately
  without waiting for completion (default: `FALSE`).

- .reasoning_effort:

  Reasoning level for the model: `"low"`, `"medium"` (default), or
  `"high"`.

- .json_schema:

  A tidyllm schema for structured JSON output (optional).

- .max_output_tokens:

  Maximum tokens to generate (default: `NULL` for model default).

- .timeout:

  Seconds to wait in blocking mode before giving up (default: `1800`).

- .max_tries:

  Maximum retries per HTTP request (default: `3`).

## Value

If `.background = FALSE`, an updated `LLMMessage` with the research
reply. If `.background = TRUE`, a `tidyllm_research_job` for use with
[`check_job()`](https://edubruell.github.io/tidyllm/dev/reference/check_job.md)/[`fetch_job()`](https://edubruell.github.io/tidyllm/dev/reference/fetch_job.md).
