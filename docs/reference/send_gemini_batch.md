# Submit a list of LLMMessage objects to Gemini's batch API

Returns a named list (same as input) with batch_id and json attributes.

## Usage

``` r
send_gemini_batch(
  .llms,
  .model = "gemini-2.5-flash",
  .temperature = NULL,
  .max_output_tokens = NULL,
  .top_p = NULL,
  .top_k = NULL,
  .presence_penalty = NULL,
  .frequency_penalty = NULL,
  .stop_sequences = NULL,
  .safety_settings = NULL,
  .json_schema = NULL,
  .grounding_threshold = NULL,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .display = "tidyllm_batch",
  .id_prefix = "tidyllm_gemini_req_"
)
```

## Arguments

- .llms:

  List of LLMMessage objects (named or unnamed).

- .model:

  The model identifier (default: "gemini-1.5-flash").

- .temperature:

  Controls randomness (default: NULL, range: 0-2).

- .max_output_tokens:

  Maximum tokens in the response (default: NULL).

- .top_p:

  Nucleus sampling (default: NULL, range: 0-1).

- .top_k:

  Diversity in token selection (default: NULL).

- .presence_penalty:

  Penalizes new tokens (default: NULL, -2 to 2).

- .frequency_penalty:

  Penalizes frequent tokens (default: NULL, -2 to 2).

- .stop_sequences:

  Character vector or NULL of up to 5.

- .safety_settings:

  Optional list of safety settings (default: NULL).

- .json_schema:

  Optional schema to enforce output structure.

- .grounding_threshold:

  Optional grounding threshold (0-1) to enable Google Search.

- .timeout:

  Timeout in seconds (default: 120).

- .dry_run:

  If TRUE, returns the constructed request (default: FALSE).

- .max_tries:

  Maximum retry attempts (default: 3).

- .display:

  Display name for this batch (default: "tidyllm_batch").

- .id_prefix:

  Prefix for message IDs (default: "tidyllm_gemini_req\_").

## Value

Named list of LLMMessage objects with attributes `batch_id` and `json`
