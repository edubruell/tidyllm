# Check the Status of a Gemini Batch Operation

Retrieves processing status and metadata for a Gemini batch operation.

## Usage

``` r
check_gemini_batch(
  .llms = NULL,
  .batch_id = NULL,
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .llms:

  (Optional) List of LLMMessage objects with a `"batch_id"` attribute
  (as returned by
  [`send_gemini_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_gemini_batch.md)).

- .batch_id:

  (Optional) Character string: full batch operation name, e.g.
  `"batches/xyz123"`. If both `.llms` and `.batch_id` are provided,
  `.batch_id` is used.

- .timeout:

  Integer. Request timeout in seconds. Default: 60.

- .max_tries:

  Integer. Maximum retry attempts. Default: 3.

- .dry_run:

  Logical. If TRUE, return the request object instead of making the
  request (for debugging). Default: FALSE.

## Value

A tibble with the operation's metadata, including name, state, creation
time, completion time, and done status.

## Details

You can supply either the `.batch_id` string (e.g. `"batches/xyz..."`)
**or** a list of LLMMessage objects (`.llms`) with a `"batch_id"`
attribute as returned by
[`send_gemini_batch()`](https://edubruell.github.io/tidyllm/dev/reference/send_gemini_batch.md).
