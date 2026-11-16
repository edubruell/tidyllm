# List Recent Gemini Batch Operations

Returns a tibble with recent Gemini batch operations and their metadata.

## Usage

``` r
list_gemini_batches(
  .filter = NULL,
  .page_size = 20,
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .filter:

  Optional filter expression for batch listing (see Gemini API docs).

- .page_size:

  Integer. Maximum number of results to return. Default: 20.

- .timeout:

  Integer. Request timeout in seconds. Default: 60.

- .max_tries:

  Integer. Maximum retry attempts. Default: 3.

- .dry_run:

  Logical. If TRUE, returns the request object (for debugging). Default:
  FALSE.

## Value

A tibble with columns: name, state, done, create_time, complete_time.
