# List Models Loaded in the llama.cpp Server

Calls the `/v1/models` endpoint of a running llama.cpp server and
returns the currently loaded model(s) as a tibble. In normal operation
this is one row; two rows appear when speculative decoding is active
(main + draft model).

## Usage

``` r
llamacpp_list_models(
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
  .timeout = 30,
  .max_tries = 3
)
```

## Arguments

- .server:

  Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER` env
  var, falling back to `"http://localhost:8080"`.

- .api_key:

  API key for the server (default: `LLAMACPP_API_KEY` env var).

- .timeout:

  Request timeout in seconds (default: 30).

- .max_tries:

  Maximum retries (default: 3).

## Value

A tibble with columns `id`, `object`, and `created`.
