# Check Health of the llama.cpp Server

Calls the `/health` endpoint of a running llama.cpp server. Returns the
status string (`"ok"`, `"loading model"`, `"no model loaded"`, or
`"error"`) along with the full parsed response body as a named list.

## Usage

``` r
llamacpp_health(
  .server = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
  .timeout = 10
)
```

## Arguments

- .server:

  Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER` env
  var, falling back to `"http://localhost:8080"`.

- .timeout:

  Request timeout in seconds (default: 10).

## Value

A named list with at least a `status` element.
