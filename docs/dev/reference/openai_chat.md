# Send LLM Messages to the OpenAI Responses API

Sends a message history to the OpenAI Responses API
(`POST /v1/responses`) and returns the assistant's reply. Supports
streaming, tool use, structured output, and reasoning models (o-series)
via `.reasoning_effort`.

## Usage

``` r
openai_chat(
  .llm,
  .model = "gpt-5.4",
  .max_output_tokens = NULL,
  .temperature = NULL,
  .seed = NULL,
  .stream = FALSE,
  .timeout = 60,
  .verbose = FALSE,
  .json_schema = NULL,
  .max_tries = 3,
  .dry_run = FALSE,
  .reasoning_effort = NULL,
  .tools = NULL,
  .tool_choice = NULL,
  .max_tool_rounds = 10,
  .stateful = FALSE
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .model:

  The model identifier (default: `"gpt-4o"`).

- .max_output_tokens:

  Maximum tokens to generate (caps reasoning + completion).

- .temperature:

  Sampling temperature (0-2).

- .seed:

  Seed for deterministic sampling.

- .stream:

  If TRUE, stream output to console (default: FALSE).

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  Print rate-limit info after the call (default: FALSE).

- .json_schema:

  A tidyllm schema or ellmer type for structured output.

- .max_tries:

  Maximum retry attempts (default: 3).

- .dry_run:

  If TRUE, return the request object without sending (default: FALSE).

- .reasoning_effort:

  For o-series models: `"low"`, `"medium"`, or `"high"`.

- .tools:

  A TOOL object or list of TOOL objects for function calling.

- .tool_choice:

  Tool selection behavior: `"none"`, `"auto"`, or `"required"`.

- .max_tool_rounds:

  Maximum tool-loop iterations (default: 10).

- .stateful:

  If `TRUE`, use `previous_response_id` to pass conversation context
  server-side instead of re-sending full message history. Falls back to
  full-history mode silently when no previous response ID is available
  (first turn or prior turn used a different provider), and with a
  warning if the server rejects the stored ID (e.g., expired context).
  Default: `FALSE`.

## Value

A new `LLMMessage` object with the assistant's response appended.
