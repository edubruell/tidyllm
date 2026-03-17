# Send a Batch of Messages to Claude API

This function creates and submits a batch of messages to the Claude API
for asynchronous processing.

## Usage

``` r
send_claude_batch(
  .llms,
  .model = "claude-sonnet-4-6",
  .max_tokens = 1024,
  .temperature = NULL,
  .top_k = NULL,
  .top_p = NULL,
  .stop_sequences = NULL,
  .json_schema = NULL,
  .thinking = FALSE,
  .thinking_budget = 1024,
  .api_url = "https://api.anthropic.com/",
  .verbose = FALSE,
  .dry_run = FALSE,
  .overwrite = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .id_prefix = "tidyllm_claude_req_"
)
```

## Arguments

- .llms:

  A list of LLMMessage objects containing conversation histories.

- .model:

  Character string specifying the Claude model version (default:
  "claude-sonnet-4-6").

- .max_tokens:

  Integer specifying the maximum tokens per response (default: 1024).

- .temperature:

  Numeric between 0 and 1 controlling response randomness.

- .top_k:

  Integer for diversity by limiting the top K tokens.

- .top_p:

  Numeric between 0 and 1 for nucleus sampling.

- .stop_sequences:

  Character vector of sequences that halt response generation.

- .json_schema:

  A schema to enforce an output structure

- .thinking:

  Logical; if TRUE, enables Claude's thinking mode for complex reasoning
  tasks (default: FALSE).

- .thinking_budget:

  Integer specifying the maximum tokens Claude can spend on thinking
  (default: 1024). Must be at least 1024.

  Defaults to "tidyllm_claude_req\_".

- .api_url:

  Base URL for the Claude API (default: "https://api.anthropic.com/").

- .verbose:

  Logical; if TRUE, prints a message with the batch ID (default: FALSE).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .overwrite:

  Logical; if TRUE, allows overwriting an existing batch ID associated
  with the request (default: FALSE).

- .max_tries:

  Maximum number of retries to perform the request.

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

- .id_prefix:

  Character string to specify a prefix for generating custom IDs when
  names in `.llms` are missing.

## Value

An updated and named list of `.llms` with identifiers that align with
batch responses, including a `batch_id` attribute.
