# Send LLM Messages to the DeepSeek Chat API

This function sends a message history to the DeepSeek Chat API and
returns the assistant's reply. Currently tool calls cause problems on
the DeepSeek API

## Usage

``` r
deepseek_chat(
  .llm,
  .model = "deepseek-chat",
  .thinking = NULL,
  .max_tokens = 2048,
  .temperature = NULL,
  .top_p = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stop = NULL,
  .stream = FALSE,
  .logprobs = NULL,
  .top_logprobs = NULL,
  .tools = NULL,
  .tool_choice = NULL,
  .api_url = "https://api.deepseek.com/",
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

  The identifier of the model to use (default: "deepseek-chat").

- .max_tokens:

  The maximum number of tokens that can be generated in the response
  (default: 2048).

- .temperature:

  Controls the randomness in the model's response. Values between 0 and
  2 are allowed (optional).

- .top_p:

  Nucleus sampling parameter that controls the proportion of probability
  mass considered (optional).

- .frequency_penalty:

  Number between -2.0 and 2.0. Penalizes repeated tokens to reduce
  repetition (optional).

- .presence_penalty:

  Number between -2.0 and 2.0. Encourages new topics by penalizing
  tokens that have appeared so far (optional).

- .stop:

  One or more sequences where the API will stop generating further
  tokens (optional).

- .stream:

  Logical; if TRUE, streams the response piece by piece (default:
  FALSE).

- .logprobs:

  If TRUE, returns log probabilities of each output token (default:
  FALSE).

- .top_logprobs:

  Number between 0 and 5 specifying the number of top log probabilities
  to return (optional).

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .tool_choice:

  A character string specifying the tool-calling behavior; valid values
  are `"none"`, `"auto"`, or `"required"` (optional).

- .api_url:

  Base URL for the DeepSeek API (default:
  `"https://api.deepseek.com/"`).

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  If TRUE, displays additional information after the API call (default:
  FALSE).

- .dry_run:

  If TRUE, returns the constructed request object without executing it
  (default: FALSE).

- .max_tries:

  Maximum retries to perform the request (default: 3).

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
