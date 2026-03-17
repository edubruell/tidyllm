# Send a Batch of Requests to the Mistral API

This function creates and submits a batch of messages to the Mistral API
for asynchronous processing.

## Usage

``` r
send_mistral_batch(
  .llms,
  .model = "mistral-small-latest",
  .endpoint = "/v1/chat/completions",
  .metadata = NULL,
  .temperature = 0.7,
  .top_p = 1,
  .max_tokens = 1024,
  .min_tokens = NULL,
  .frequency_penalty = NULL,
  .logit_bias = NULL,
  .presence_penalty = NULL,
  .seed = NULL,
  .stop = NULL,
  .safe_prompt = FALSE,
  .json_schema = NULL,
  .dry_run = FALSE,
  .overwrite = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .id_prefix = "tidyllm_mistral_req_"
)
```

## Arguments

- .llms:

  A list of LLMMessage objects containing conversation histories.

- .model:

  The Mistral model version (default: "mistral-small-latest").

- .endpoint:

  The API endpoint (default: "/v1/chat/completions").

- .metadata:

  Optional metadata for the batch.

- .temperature:

  Sampling temperature to use, between `0.0` and `1.5` (default: `0.7`).

- .top_p:

  Nucleus sampling parameter, between `0.0` and `1.0` (default: `1`).

- .max_tokens:

  The maximum number of tokens to generate in the completion (default:
  `1024`).

- .min_tokens:

  The minimum number of tokens to generate (optional).

- .frequency_penalty:

  Numeric value (or NULL) for frequency penalty.

- .logit_bias:

  A named list modifying the likelihood of specific tokens (or NULL).

- .presence_penalty:

  Numeric value (or NULL) for presence penalty.

- .seed:

  Random seed for deterministic outputs (optional).

- .stop:

  Sequence(s) at which to stop generation (optional).

- .safe_prompt:

  Logical; if TRUE, injects a safety prompt (default: FALSE).

- .json_schema:

  A JSON schema object for structured output (optional).

- .dry_run:

  Logical; if TRUE, returns the prepared request without executing it
  (default: FALSE).

- .overwrite:

  Logical; if TRUE, allows overwriting existing custom IDs (default:
  FALSE).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .timeout:

  Request timeout in seconds (default: 60).

- .id_prefix:

  Prefix for generating custom IDs (default: "tidyllm_mistral_req\_").

## Value

The prepared LLMMessage list with a batch_id attribute.
