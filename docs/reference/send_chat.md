# Start a chat without waiting for it

Sends one request and returns immediately with a job handle. The session
stays usable while the model works; use
[`check_job()`](https://edubruell.github.io/tidyllm/reference/check_job.md)
for its status,
[`get_partial()`](https://edubruell.github.io/tidyllm/reference/get_partial.md)
for the text so far, and
[`fetch_job()`](https://edubruell.github.io/tidyllm/reference/fetch_job.md)
for the finished `LLMMessage`, which is exactly what
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) would
have returned.

## Usage

``` r
send_chat(
  .llm,
  .provider = getOption("tidyllm_chat_default"),
  .on_chunk = NULL,
  .stream = TRUE,
  .dry_run = NULL,
  .temperature = NULL,
  .timeout = NULL,
  .top_p = NULL,
  .max_tries = NULL,
  .model = NULL,
  .verbose = NULL,
  .json_schema = NULL,
  .tools = NULL,
  .max_tool_rounds = NULL,
  .seed = NULL,
  .stop = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL
)
```

## Arguments

- .llm:

  An `LLMMessage` object.

- .provider:

  A provider function call, as in
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).

- .on_chunk:

  Optional function of one argument, called with each text delta as it
  arrives. This is the push form of a stream; `\(d) cat(d)` echoes to
  the console, and writing to a `reactiveVal` is all a Shiny app needs.
  Only meaningful with `.stream = TRUE`.

- .stream:

  Logical; whether the provider streams the reply. Streaming is what
  makes `.on_chunk` and
  [`get_partial()`](https://edubruell.github.io/tidyllm/reference/get_partial.md)
  show progress. A non-streaming job still runs without blocking; it
  simply has nothing to report until it finishes.

- .dry_run:

  Logical; if `TRUE`, simulates the request without sending it to the
  provider. Useful for testing.

- .temperature:

  Numeric; controls the randomness of the model's output (0 =
  deterministic).

- .timeout:

  Numeric; the maximum time (in seconds) to wait for a response.

- .top_p:

  Numeric; nucleus sampling parameter, which limits the sampling to the
  top cumulative probability `p`.

- .max_tries:

  Integer; the maximum number of retries for failed requests.

- .model:

  Character; the model identifier to use (e.g., `"gpt-4"`).

- .verbose:

  Logical; if `TRUE`, prints additional information about the request
  and response.

- .json_schema:

  List; A JSON schema object as R list to enforce the output structure

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

- .max_tool_rounds:

  Integer; the maximum number of tool use iterations for multi-turn tool
  calling (default varies by provider).

- .seed:

  Integer; sets a random seed for reproducibility.

- .stop:

  Character vector; specifies sequences where the model should stop
  generating further tokens.

- .frequency_penalty:

  Numeric; adjusts the likelihood of repeating tokens (positive values
  decrease repetition).

- .presence_penalty:

  Numeric; adjusts the likelihood of introducing new tokens (positive
  values encourage novelty).

## Value

A `tidyllm_chat_job`.

## Details

The request is driven from R's event loop rather than from a thread or a
second process, so progress happens whenever the session yields, which
the accessors do on your behalf. The consequence worth knowing is the
other side of that: a blocking call of your own, a long
[`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) or another
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md),
pauses the job for its duration.

A streamed job is not retried after a transient 429 or 503 the way
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) is,
because it is read from an open connection; a non-streamed one keeps the
usual retries.

Requires the `later` package, and `promises` as well when
`.stream = FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
job <- llm_message("Summarise the history of R in 500 words") |>
  send_chat(claude(), .stream = TRUE)

while (check_job(job) == "running") {
  cat("\r", nchar(get_partial(job)), "characters so far")
}

reply <- fetch_job(job)
} # }
```
