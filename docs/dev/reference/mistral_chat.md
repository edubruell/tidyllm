# Send LLMMessage to Mistral API

Send LLMMessage to Mistral API

## Usage

``` r
mistral_chat(
  .llm,
  .model = "mistral-large-latest",
  .frequency_penalty = NULL,
  .logit_bias = NULL,
  .presence_penalty = NULL,
  .seed = NULL,
  .stop = NULL,
  .stream = FALSE,
  .temperature = 0.7,
  .top_p = 1,
  .min_tokens = NULL,
  .max_tokens = NULL,
  .json_schema = NULL,
  .safe_prompt = FALSE,
  .reasoning_effort = NULL,
  .timeout = 120,
  .max_tries = 3,
  .dry_run = FALSE,
  .verbose = FALSE,
  .tools = NULL,
  .tool_choice = NULL,
  .max_tool_rounds = 10
)
```

## Arguments

- .llm:

  An `LLMMessage` object.

- .model:

  The model identifier to use (default: `"mistral-large-latest"`).

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

- .min_tokens:

  The minimum number of tokens to generate in the completion. Must be
  `>= 0` (optional).

- .max_tokens:

  An upper bound for the number of tokens that can be generated for a
  completion.

- .json_schema:

  A JSON schema object provided by tidyllm schema or ellmer schemata.

- .safe_prompt:

  Whether to inject a safety prompt before all conversations (default:
  `FALSE`).

- .reasoning_effort:

  Controls the reasoning effort for Magistral thinking models; one of
  `"low"`, `"medium"`, or `"high"` (default: `NULL`, meaning the API
  default).

- .timeout:

  When should our connection time out in seconds (default: `120`).

- .max_tries:

  Maximum retries to peform request

- .dry_run:

  If `TRUE`, perform a dry run and return the request object (default:
  `FALSE`).

- .verbose:

  Should additional information be shown after the API call? (default:
  `FALSE`)

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

Returns an updated `LLMMessage` object.
