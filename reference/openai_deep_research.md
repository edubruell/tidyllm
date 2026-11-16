# Submit a Deep Research Request to OpenAI

Sends a research request to OpenAI via the Responses API with
`background: true` and the web search tool. The model autonomously
searches the web and synthesises a long-form answer, which can take 5-30
minutes. OpenAI has retired its dedicated deep research models
(`o3-deep-research`, `o4-mini-deep-research`); a GPT-6 model with web
search does the job now.

## Usage

``` r
openai_deep_research(
  .llm,
  .model = "gpt-6-sol",
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

  The model to use (default: `"gpt-6-sol"`).

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
[`check_job()`](https://edubruell.github.io/tidyllm/reference/check_job.md)/[`fetch_job()`](https://edubruell.github.io/tidyllm/reference/fetch_job.md).
