# List Available Models from the Mistral API

List Available Models from the Mistral API

## Usage

``` r
mistral_list_models(
  .api_url = "https://api.mistral.ai",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE,
  .verbose = FALSE
)
```

## Arguments

- .api_url:

  Base URL for the API (default: "https://api.mistral.ai").

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum number of retries for the API request (default: 3).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it.

- .verbose:

  Logical; if TRUE, prints additional information about the request.

## Value

A tibble containing model information (columns include `id` and
`created`), or NULL if no models are found.
