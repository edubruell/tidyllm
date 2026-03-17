# Send a Batch of Messages to OpenAI Batch API

This function creates and submits a batch of messages to the OpenAI
Batch API for asynchronous processing.

## Usage

``` r
send_openai_batch(
  .llms,
  .model = "gpt-5.4",
  .max_completion_tokens = NULL,
  .reasoning_effort = NULL,
  .frequency_penalty = NULL,
  .logit_bias = NULL,
  .presence_penalty = NULL,
  .seed = NULL,
  .stop = NULL,
  .temperature = NULL,
  .top_p = NULL,
  .logprobs = NULL,
  .top_logprobs = NULL,
  .dry_run = FALSE,
  .overwrite = FALSE,
  .json_schema = NULL,
  .max_tries = 3,
  .timeout = 60,
  .verbose = FALSE,
  .id_prefix = "tidyllm_openai_req_"
)
```

## Arguments

- .llms:

  A list of LLMMessage objects containing conversation histories.

- .model:

  Character string specifying the OpenAI model version (default:
  "gpt-5.1-chat-latest").

- .max_completion_tokens:

  Integer specifying the maximum tokens per response (default: NULL).

- .reasoning_effort:

  How long should reasoning models reason (can either be "low","medium"
  or "high")

- .frequency_penalty:

  Number between -2.0 and 2.0. Positive values penalize new tokens based
  on their existing frequency in the text so far.

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

- .temperature:

  What sampling temperature to use, between 0 and 2. Higher values make
  the output more random.

- .top_p:

  An alternative to sampling with temperature, called nucleus sampling.

- .logprobs:

  If TRUE, get the log probabilities of each output token (default:
  NULL).

- .top_logprobs:

  If specified, get the top N log probabilities of each output token
  (0-5, default: NULL).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it (default: FALSE).

- .overwrite:

  Logical; if TRUE, allows overwriting an existing batch ID associated
  with the request (default: FALSE).

- .json_schema:

  A JSON schema object provided by tidyllm_schema or ellmer schemata
  (default: NULL).

- .max_tries:

  Maximum number of retries to perform the request (default: 3).

- .timeout:

  Integer specifying the request timeout in seconds (default: 60).

- .verbose:

  Logical; if TRUE, additional info about the requests is printed
  (default: FALSE).

- .id_prefix:

  Character string to specify a prefix for generating custom IDs when
  names in `.llms` are missing (default: "tidyllm_openai_req\_").

## Value

An updated and named list of `.llms` with identifiers that align with
batch responses, including a `batch_id` attribute.
