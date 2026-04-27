# Send LLM Messages to the Groq Chat API

This function sends a message history to the Groq Chat API and returns
the assistant's reply.

## Usage

``` r
groq_chat(
  .llm,
  .model = "openai/gpt-oss-120b",
  .max_tokens = 1024,
  .temperature = NULL,
  .top_p = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stop = NULL,
  .seed = NULL,
  .tools = NULL,
  .tool_choice = NULL,
  .api_url = "https://api.groq.com/",
  .json_schema = NULL,
  .timeout = 60,
  .verbose = FALSE,
  .stream = FALSE,
  .dry_run = FALSE,
  .max_tries = 3,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .model:

  The identifier of the model to use (default:
  "llama-3.2-11b-vision-preview").

- .max_tokens:

  The maximum number of tokens that can be generated in the response
  (default: 1024).

- .temperature:

  Controls the randomness in the model's response. Values between 0 and
  2 are allowed, where higher values increase randomness (optional).

- .top_p:

  Nucleus sampling parameter that controls the proportion of probability
  mass considered. Values between 0 and 1 are allowed (optional).

- .frequency_penalty:

  Number between -2.0 and 2.0. Positive values penalize repeated tokens,
  reducing likelihood of repetition (optional).

- .presence_penalty:

  Number between -2.0 and 2.0. Positive values encourage new topics by
  penalizing tokens that have appeared so far (optional).

- .stop:

  One or more sequences where the API will stop generating further
  tokens. Can be a string or a list of strings (optional).

- .seed:

  An integer for deterministic sampling. If specified, attempts to
  return the same result for repeated requests with identical parameters
  (optional).

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls (optional).

- .tool_choice:

  A character string specifying the tool-calling behavior; valid values
  are "none", "auto", or "required" (optional).

- .api_url:

  Base URL for the Groq API (default: "https://api.groq.com/").

- .json_schema:

  A list or tidyllm schema created with
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
  for structured JSON output (optional).

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  If TRUE, displays additional information after the API call, including
  rate limit details (default: FALSE).

- .stream:

  Logical; if TRUE, streams the response piece by piece (default:
  FALSE).

- .dry_run:

  If TRUE, performs a dry run and returns the constructed request object
  without executing it (default: FALSE).

- .max_tries:

  Maximum retries to peform request

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
msg <- llm_message("What is Groq?")
result <- groq_chat(msg)

# With custom parameters
result2 <- groq_chat(msg, 
               .model = "llama-3.2-vision",
               .temperature = 0.5, 
               .max_tokens = 512)
} # }
```
