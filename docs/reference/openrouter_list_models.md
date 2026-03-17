# List Available Models on OpenRouter

Retrieves the list of models available through the OpenRouter API,
including pricing and context window information.

## Usage

``` r
openrouter_list_models(
  .api_url = "https://openrouter.ai",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE,
  .verbose = FALSE
)
```

## Arguments

- .api_url:

  Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum retries (default: 3).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

- .verbose:

  If TRUE, displays additional information (default: FALSE).

## Value

A tibble with columns `id`, `name`, `context_length`,
`prompt_price_per_million`, and `completion_price_per_million`, or NULL
if no models are found.
