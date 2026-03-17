# Cancel an In-Progress OpenAI Batch

This function cancels an in-progress batch created through the OpenAI
API. The batch will be moved to a "cancelling" state and, eventually,
"cancelled."

## Usage

``` r
cancel_openai_batch(.batch_id, .dry_run = FALSE, .max_tries = 3, .timeout = 60)
```

## Arguments

- .batch_id:

  Character; the unique identifier for the batch to cancel.

- .dry_run:

  Logical; if `TRUE`, returns the constructed request without executing
  it (default: `FALSE`).

- .max_tries:

  Integer; maximum number of retries if the request fails (default:
  `3`).

- .timeout:

  Integer; request timeout in seconds (default: `60`).

## Value

A list containing the response from the OpenAI API about the
cancellation status.
