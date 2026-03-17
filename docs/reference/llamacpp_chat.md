# Send LLM Messages to a llama.cpp Server

Sends a message history to a local llama.cpp server using the
OpenAI-compatible Chat Completions API. Supports BNF grammar constraints
(a llama.cpp-specific feature that enforces output format at the token
sampling level) and token logprobs for uncertainty quantification.

## Usage

``` r
llamacpp_chat(
  .llm,
  .model = "local-model",
  .max_tokens = 1024,
  .temperature = NULL,
  .top_p = NULL,
  .stop = NULL,
  .stream = FALSE,
  .tools = NULL,
  .tool_choice = NULL,
  .json_schema = NULL,
  .grammar = NULL,
  .logprobs = FALSE,
  .top_logprobs = NULL,
  .seed = NULL,
  .thinking = NULL,
  .thinking_budget = NULL,
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
  .timeout = 120,
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

  The model name (default: `"local-model"`). llama.cpp ignores this
  value and serves whatever GGUF is loaded; the default is a clear
  placeholder.

- .max_tokens:

  Maximum tokens in the response (default: 1024).

- .temperature:

  Controls randomness (0–2, optional).

- .top_p:

  Nucleus sampling parameter (0–1, optional).

- .stop:

  One or more stop sequences (optional).

- .stream:

  Logical; if TRUE, streams the response (default: FALSE).

- .tools:

  Either a single TOOL object or a list of TOOL objects.

- .tool_choice:

  Tool-calling behavior: `"none"`, `"auto"`, or `"required"` (optional).

- .json_schema:

  A JSON schema as an R list to enforce structured output. Passed as
  `response_format = {type: "json_schema", ...}` — enforced at the
  sampler level.

- .grammar:

  A BNF grammar string to constrain sampling to any formal language
  (e.g. `"root ::= [0-9]{4}"` for a 4-digit year). Takes precedence over
  `.json_schema` when both are set; use one or the other.

- .logprobs:

  Logical; if TRUE, returns token log-probabilities (default: FALSE).

- .top_logprobs:

  Integer; number of top-alternative log-prob entries to return per
  token (1–20, optional). Requires `.logprobs = TRUE`.

- .seed:

  Integer; random seed for reproducible outputs (optional).

- .thinking:

  Logical; if TRUE, enables extended reasoning/thinking mode for models
  that support it (e.g. Qwen3). `NULL` (default) lets the server decide;
  `FALSE` explicitly disables it for faster responses; `TRUE` forces it
  on. The thinking trace is accessible via
  `get_metadata(result)$api_specific$thinking`.

- .thinking_budget:

  Integer; maximum tokens the model may use for thinking when
  `.thinking = TRUE`. `NULL` uses the server default (optional).

- .server:

  Base URL of the llama.cpp server. Defaults to the `LLAMACPP_SERVER`
  environment variable, falling back to `"http://localhost:8080"`.

- .api_key:

  API key for the llama.cpp server. Defaults to the `LLAMACPP_API_KEY`
  environment variable. Leave unset if the server was started without
  `--api-key`.

- .timeout:

  Request timeout in seconds (default: 120).

- .verbose:

  If TRUE, displays additional information (default: FALSE).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

- .max_tries:

  Maximum retries (default: 3).

- .max_tool_rounds:

  Maximum tool use iterations (default: 10).

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
