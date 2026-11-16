# List Available Models from the Anthropic Claude API

List Available Models from the Anthropic Claude API

## Usage

``` r
claude_list_models(
  .api_url = "https://api.anthropic.com",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE,
  .verbose = FALSE
)
```

## Arguments

- .api_url:

  Base URL for the API (default: "https://api.anthropic.com").

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

A tibble containing model information (columns include `type`,`id`,
`display_name`, and `created_at`), or NULL if no models are found.
