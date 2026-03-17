# Get OpenRouter Credit Balance

Returns the total credits purchased and total usage so far for the
current API key.

## Usage

``` r
openrouter_credits(
  .api_url = "https://openrouter.ai",
  .timeout = 60,
  .max_tries = 3
)
```

## Arguments

- .api_url:

  Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum retries (default: 3).

## Value

A named list with `total_credits` (USD purchased) and `total_usage` (USD
consumed so far).
