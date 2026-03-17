# Send a Batch of Messages to the Groq API

This function creates and submits a batch of messages to the Groq API
for asynchronous processing.

## Usage

``` r
send_groq_batch(
  .llms,
  .model = "openai/gpt-oss-120b",
  .max_tokens = 1024,
  .temperature = NULL,
  .top_p = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stop = NULL,
  .seed = NULL,
  .api_url = "https://api.groq.com/",
  .json_schema = NULL,
  .completion_window = "24h",
  .verbose = FALSE,
  .dry_run = FALSE,
  .overwrite = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .id_prefix = "tidyllm_groq_req_"
)
```

## Arguments

- .llms:

  A list of LLMMessage objects containing conversation histories.

- .model:

  Character string specifying the model to use (default:
  "deepseek-r1-distill-llama-70b").

- .max_tokens:

  Integer specifying the maximum tokens per response (default: 1024).

- .temperature:

  Numeric between 0 and 2 controlling response randomness.

- .top_p:

  Numeric between 0 and 1 for nucleus sampling.

- .frequency_penalty:

  Number between -2.0 and 2.0 to penalize repetition.

- .presence_penalty:

  Number between -2.0 and 2.0 to encourage new topics.

- .stop:

  One or more sequences where the API will stop generating further
  tokens.

- .seed:

  An integer for deterministic sampling.

- .api_url:

  Base URL for the Groq API (default: "https://api.groq.com/").

- .json_schema:

  A list or tidyllm schema created with
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/reference/tidyllm_schema.md)
  for structured JSON output (optional).

- .completion_window:

  Character string for the batch completion window (default: "24h").

- .verbose:

  Logical; if TRUE, prints a message with the batch ID (default: FALSE).

- .dry_run:

  Logical; if TRUE, returns the prepared request objects without
  executing (default: FALSE).

- .overwrite:

  Logical; if TRUE, allows overwriting an existing batch ID (default:
  FALSE).

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
