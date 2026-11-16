# Fetch Results for a Gemini Batch

Retrieves the results of a completed Gemini batch and updates the
provided list of LLMMessage objects with the assistant's responses,
matching by original list order.

## Usage

``` r
fetch_gemini_batch(
  .llms,
  .batch_name = NULL,
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .llms:

  List of `LLMMessage` objects (as from
  [`send_gemini_batch()`](https://edubruell.github.io/tidyllm/reference/send_gemini_batch.md)),
  must have a `batch_id` attribute if `.batch_name` is not given.

- .batch_name:

  (Optional) Character; batch operation name (e.g. "batches/xyz123"). If
  not provided, is taken from `attr(.llms, "batch_id")`.

- .timeout:

  Integer; request timeout in seconds (default: 60).

- .max_tries:

  Integer; maximum retry attempts (default: 3).

- .dry_run:

  Logical; if `TRUE`, returns the GET request object (default: FALSE).

## Value

A list of updated LLMMessage objects with the assistant response
appended to each, in the same order.
