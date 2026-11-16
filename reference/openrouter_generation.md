# Get Details for an OpenRouter Generation

Fetches cost, latency, and token details for a past generation using its
ID. The generation ID is available in
`get_metadata(result)$api_specific$id` after a
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) call.

## Usage

``` r
openrouter_generation(
  .id,
  .api_url = "https://openrouter.ai",
  .timeout = 60,
  .max_tries = 3
)
```

## Arguments

- .id:

  The generation ID string (e.g. `"gen-..."`) returned by OpenRouter.

- .api_url:

  Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum retries (default: 3).

## Value

A named list with generation details including `id`, `model`,
`total_cost`, `tokens_prompt`, `tokens_completion`, and `latency`.
