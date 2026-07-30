# Interact with Claude AI models via the Anthropic API

Interact with Claude AI models via the Anthropic API

## Usage

``` r
claude_chat(
  .llm,
  .model = "claude-sonnet-5",
  .max_tokens = 2048,
  .temperature = NULL,
  .top_k = NULL,
  .top_p = NULL,
  .metadata = NULL,
  .stop_sequences = NULL,
  .tools = NULL,
  .json_schema = NULL,
  .file_ids = NULL,
  .api_url = "https://api.anthropic.com/",
  .verbose = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .stream = FALSE,
  .dry_run = FALSE,
  .thinking = FALSE,
  .thinking_budget = 1024,
  .effort = NULL,
  .cache = FALSE,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An LLMMessage object containing the conversation history and system
  prompt.

- .model:

  Character string specifying the Claude model version (default:
  "claude-sonnet-5").

- .max_tokens:

  Integer specifying the maximum number of tokens in the response
  (default: 1024).

- .temperature:

  Numeric between 0 and 1 controlling response randomness. Only
  supported on older models; Claude Sonnet 5 and Opus 4.7 or newer
  reject sampling parameters.

- .top_k:

  Integer controlling diversity by limiting the top K tokens. Only
  supported on older models.

- .top_p:

  Numeric between 0 and 1 for nucleus sampling. Only supported on older
  models.

- .metadata:

  List of additional metadata to include with the request.

- .stop_sequences:

  Character vector of sequences that will halt response generation.

- .tools:

  List of additional tools or functions the model can use.

- .json_schema:

  A schema to enforce an output structure

- .file_ids:

  Character; A vector of file IDs for files that were uploaded to
  Anthropics Servers

- .api_url:

  Base URL for the Anthropic API (default:
  "https://api.anthropic.com/").

- .verbose:

  Logical; if TRUE, displays additional information about the API call
  (default: FALSE).

- .max_tries:

  Maximum retries to peform request

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

- .stream:

  Logical; if TRUE, streams the response piece by piece (default:
  FALSE).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .thinking:

  Logical; if TRUE, enables Claude's thinking mode. On Claude Sonnet
  4.6, Opus 4.6 or newer this maps to adaptive thinking; on older models
  it uses a fixed thinking budget (default: FALSE).

- .thinking_budget:

  Integer specifying the maximum tokens Claude can spend on thinking
  (default: 1024). Must be at least 1024. Only used on older models;
  ignored when the model supports adaptive thinking, where `.effort`
  controls thinking depth instead.

- .effort:

  Character; one of "low", "medium", "high", "xhigh", or "max". Controls
  thinking depth and overall token spend on models that support the
  effort parameter (Claude Opus 4.5 or newer, Claude Sonnet 4.6 or
  newer). Default NULL uses the API default ("high").

- .cache:

  Logical or character; enables Anthropic prompt caching for the
  request. TRUE caches with the default 5-minute time to live; "1h"
  requests a one-hour time to live. Cache reads cost roughly a tenth of
  the base input price. Cache token counts are reported in
  [`get_metadata()`](https://edubruell.github.io/tidyllm/reference/get_metadata.md)
  in the `cached_tokens` and `cache_creation_tokens` columns, and in raw
  form in `api_specific` under `cache_creation_input_tokens` and
  `cache_read_input_tokens` (default: FALSE).

  Anthropic only caches prompts above a per-model minimum, and that
  minimum is higher on the cheaper models: 4096 tokens on Haiku 4.5,
  1024 on Sonnet 5 and Sonnet 4.6, 512 on Opus 5. Shorter prompts are
  not an error; they simply report zero cache tokens.

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new LLMMessage object containing the original messages plus Claude's
response.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
msg <- llm_message("What is R programming?")
result <- claude_chat(msg)

# With adaptive thinking and effort control
result2 <- claude_chat(msg,
                 .thinking = TRUE,
                 .effort = "low")
} # }
```
