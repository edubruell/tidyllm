# Send LLMMessage to Gemini API

Send LLMMessage to Gemini API

## Usage

``` r
gemini_chat(
  .llm,
  .model = "gemini-2.5-flash",
  .fileid = NULL,
  .temperature = NULL,
  .max_output_tokens = NULL,
  .top_p = NULL,
  .top_k = NULL,
  .grounding_threshold = NULL,
  .presence_penalty = NULL,
  .frequency_penalty = NULL,
  .stop_sequences = NULL,
  .safety_settings = NULL,
  .json_schema = NULL,
  .tools = NULL,
  .thinking_budget = NULL,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .verbose = FALSE,
  .stream = FALSE,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An existing LLMMessage object or an initial text prompt.

- .model:

  The model identifier (default: "gemini-1.5-flash").

- .fileid:

  Optional vector of file IDs uploaded via
  [`gemini_upload_file()`](https://edubruell.github.io/tidyllm/reference/gemini_upload_file.md)
  (default: NULL).

- .temperature:

  Controls randomness in generation (default: NULL, range: 0.0-2.0).

- .max_output_tokens:

  Maximum tokens in the response (default: NULL).

- .top_p:

  Controls nucleus sampling (default: NULL, range: 0.0-1.0).

- .top_k:

  Controls diversity in token selection (default: NULL, range: 0 or
  more).

- .grounding_threshold:

  A grounding threshold between 0 and 1. With lower grounding thresholds
  Gemini will use Google to search for relevant information before
  answering. (default: NULL).

- .presence_penalty:

  Penalizes new tokens (default: NULL, range: -2.0 to 2.0).

- .frequency_penalty:

  Penalizes frequent tokens (default: NULL, range: -2.0 to 2.0).

- .stop_sequences:

  Optional character sequences to stop generation (default: NULL, up to
  5).

- .safety_settings:

  A list of safety settings (default: NULL).

- .json_schema:

  A schema to enforce an output structure

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .thinking_budget:

  Token budget for internal reasoning (default: NULL). Works with
  `gemini-2.5-flash` and `gemini-2.5-pro`.

- .timeout:

  When should our connection time out (default: 120 seconds).

- .dry_run:

  If TRUE, perform a dry run and return the request object.

- .max_tries:

  Maximum retries to perform request (default: 3).

- .verbose:

  Should additional information be shown after the API call.

- .stream:

  Should the response be streamed (default: FALSE).

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
