# Send LLM Messages to the OpenAI Chat Completions API

This function sends a message history to the OpenAI Chat Completions API
and returns the assistant's reply.

## Usage

``` r
openai_chat(
  .llm,
  .model = "gpt-5.1-chat-latest",
  .max_completion_tokens = NULL,
  .reasoning_effort = NULL,
  .frequency_penalty = NULL,
  .logit_bias = NULL,
  .presence_penalty = NULL,
  .seed = NULL,
  .stop = NULL,
  .stream = FALSE,
  .temperature = NULL,
  .top_p = NULL,
  .api_url = "https://api.openai.com/",
  .timeout = 60,
  .verbose = FALSE,
  .json_schema = NULL,
  .max_tries = 3,
  .dry_run = FALSE,
  .compatible = FALSE,
  .api_path = "/v1/chat/completions",
  .logprobs = NULL,
  .top_logprobs = NULL,
  .tools = NULL,
  .tool_choice = NULL,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .model:

  The identifier of the model to use (default: "gpt-5.1-chat-latest").

- .max_completion_tokens:

  An upper bound for the number of tokens that can be generated for a
  completion.

- .reasoning_effort:

  How long should reasoning models reason (can either be "low","medium"
  or "high")

- .frequency_penalty:

  Number between -2.0 and 2.0. Positive values penalize new tokens based
  on their existing frequency.

- .logit_bias:

  A named list modifying the likelihood of specified tokens appearing in
  the completion.

- .presence_penalty:

  Number between -2.0 and 2.0. Positive values penalize new tokens based
  on whether they appear in the text so far.

- .seed:

  If specified, the system will make a best effort to sample
  deterministically.

- .stop:

  Up to 4 sequences where the API will stop generating further tokens.

- .stream:

  If set to TRUE, the answer will be streamed to console as it comes
  (default: FALSE).

- .temperature:

  What sampling temperature to use, between 0 and 2. Higher values make
  the output more random.

- .top_p:

  An alternative to sampling with temperature, called nucleus sampling.

- .api_url:

  Base URL for the API (default: "https://api.openai.com/").

- .timeout:

  Request timeout in seconds (default: 60).

- .verbose:

  Should additional information be shown after the API call (default:
  FALSE).

- .json_schema:

  A JSON schema object provided by tidyllm schema or ellmer schemata.

- .max_tries:

  Maximum retries to perform request

- .dry_run:

  If TRUE, perform a dry run and return the request object (default:
  FALSE).

- .compatible:

  If TRUE, skip API and rate-limit checks for OpenAI compatible APIs
  (default: FALSE).

- .api_path:

  The path relative to the base `.api_url` for the API (default:
  "/v1/chat/completions").

- .logprobs:

  If TRUE, get the log probabilities of each output token (default:
  NULL).

- .top_logprobs:

  If specified, get the top N log probabilities of each output token
  (0-5, default: NULL).

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .tool_choice:

  A character string specifying the tool-calling behavior; valid values
  are "none", "auto", or "required".

- .max_tool_rounds:

  Integer specifying the maximum number of tool use iterations (default:
  10). Set to 1 for single-round tool use, or higher for multi-turn
  agentic loops.

## Value

A new `LLMMessage` object containing the original messages plus the
assistant's response.
