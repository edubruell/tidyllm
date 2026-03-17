# Send LLM Messages to the OpenRouter Chat API

Sends a message history to the OpenRouter API, which provides access to
hundreds of models from different providers through a single
OpenAI-compatible endpoint.

## Usage

``` r
openrouter_chat(
  .llm,
  .model = "anthropic/claude-sonnet-4-6",
  .max_tokens = 2048,
  .temperature = NULL,
  .top_p = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stop = NULL,
  .stream = FALSE,
  .json_schema = NULL,
  .tools = NULL,
  .tool_choice = NULL,
  .provider = NULL,
  .route = NULL,
  .models = NULL,
  .api_url = "https://openrouter.ai",
  .timeout = 60,
  .verbose = FALSE,
  .dry_run = FALSE,
  .max_tries = 3,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .model:

  The model identifier to use (default: `"google/gemini-2.5-flash"`).
  Any model available on OpenRouter can be used.

- .max_tokens:

  Maximum number of tokens in the response (default: 2048).

- .temperature:

  Controls randomness (0–2, optional).

- .top_p:

  Nucleus sampling parameter (0–1, optional).

- .frequency_penalty:

  Penalizes repeated tokens (-2 to 2, optional).

- .presence_penalty:

  Encourages new topics (-2 to 2, optional).

- .stop:

  One or more stop sequences (optional).

- .stream:

  Logical; if TRUE, streams the response (default: FALSE).

- .json_schema:

  A JSON schema object for structured output (default: NULL).

- .tools:

  Either a single TOOL object or a list of TOOL objects for tool calls.

- .tool_choice:

  Tool-calling behavior: `"none"`, `"auto"`, or `"required"` (optional).

- .provider:

  A named list of OpenRouter provider preferences, e.g.
  `list(order = c("Anthropic", "AWS Bedrock"), allow_fallbacks = TRUE)`
  (optional).

- .route:

  OpenRouter routing strategy; `"fallback"` routes to the next model if
  the primary is unavailable (optional).

- .models:

  A character vector of model IDs to use as fallbacks when the primary
  model is unavailable (optional). Used together with
  `.route = "fallback"`.

- .api_url:

  Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  If TRUE, displays additional information after the API call (default:
  FALSE).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

- .max_tries:

  Maximum retries (default: 3).

- .max_tool_rounds:

  Maximum number of tool use iterations (default: 10).

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
