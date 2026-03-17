# List Available Models from the OpenAI API

List Available Models from the OpenAI API

## Usage

``` r
openai_list_models(
  .api_url = "https://api.openai.com",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE,
  .verbose = FALSE
)
```

## Arguments

- .api_url:

  Base URL for the API (default: "https://api.openai.com").

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

A tibble containing model information (columns include `id`, `created`,
and `owned_by`), or NULL if no models are found.
