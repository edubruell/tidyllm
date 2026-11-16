# Send a batch of messages to a batch API

The `send_batch()` function allows you to send a list of `LLMMessage`
objects to an API. It routes the input to the appropriate
provider-specific batch API function.

## Usage

``` r
send_batch(
  .llms,
  .provider = getOption("tidyllm_sbatch_default"),
  .dry_run = NULL,
  .temperature = NULL,
  .timeout = NULL,
  .top_p = NULL,
  .max_tries = NULL,
  .model = NULL,
  .verbose = NULL,
  .json_schema = NULL,
  .seed = NULL,
  .stop = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .id_prefix = NULL
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects containing conversation histories.

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/dev/reference/openai.md),
  [`claude()`](https://edubruell.github.io/tidyllm/dev/reference/claude.md),
  etc. You can also set a default provider function via the
  `tidyllm_sbatch_default` option.

- .dry_run:

  Logical; if `TRUE`, simulates the request without sending it to the
  provider. Useful for testing.

- .temperature:

  Numeric; controls the randomness of the model's output (0 =
  deterministic).

- .timeout:

  Numeric; the maximum time (in seconds) to wait for a response.

- .top_p:

  Numeric; nucleus sampling parameter, which limits the sampling to the
  top cumulative probability `p`.

- .max_tries:

  Integer; the maximum number of retries for failed requests.

- .model:

  Character; the model identifier to use (e.g., `"gpt-4"`).

- .verbose:

  Logical; if `TRUE`, prints additional information about the request
  and response.

- .json_schema:

  List; A JSON schema object as R list to enforce the output structure

- .seed:

  Integer; sets a random seed for reproducibility.

- .stop:

  Character vector; specifies sequences where the model should stop
  generating further tokens.

- .frequency_penalty:

  Numeric; adjusts the likelihood of repeating tokens (positive values
  decrease repetition).

- .presence_penalty:

  Numeric; adjusts the likelihood of introducing new tokens (positive
  values encourage novelty).

- .id_prefix:

  Character string to specify a prefix for generating custom IDs when
  names in `.llms` are missing

## Value

An updated and named list of `.llms` with identifiers that align with
batch responses, including a `batch_id` attribute.
