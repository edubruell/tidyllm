# Send many chats at once

Performs a list of `LLMMessage`s against one provider concurrently and
returns their replies in the same order, with the same names. It is the
middle ground between
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) in a
loop, which waits for each answer before starting the next, and
[`send_batch()`](https://edubruell.github.io/tidyllm/reference/send_batch.md),
which is cheaper still but returns hours later.

## Usage

``` r
parallel_chat(
  .llms,
  .provider = getOption("tidyllm_chat_default"),
  .max_active = 4,
  .throttle = NULL,
  .on_error = "continue",
  .temperature = NULL,
  .timeout = NULL,
  .top_p = NULL,
  .max_tries = NULL,
  .model = NULL,
  .verbose = NULL,
  .json_schema = NULL,
  .seed = NULL,
  .stop = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stream = FALSE,
  .tools = NULL
)
```

## Arguments

- .llms:

  A list of `LLMMessage` objects. Names are preserved.

- .provider:

  A provider function call, as in
  [`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md).

- .max_active:

  Maximum number of requests in flight at once. Keep it modest against a
  rate-limited provider: `httr2` applies retries across the whole set
  rather than per request, so a high number is a good way to collect
  429s.

- .throttle:

  Optional maximum number of requests per second, applied across the
  set. The straightforward defence against a rate limit.

- .on_error:

  `"continue"` (default) keeps going and puts the failure in that
  element's slot; `"stop"` aborts the whole set on the first failure.

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

- .stream:

  Logical; if `TRUE`, streams the response from the provider in
  real-time.

- .tools:

  Either a single TOOL object or a list of TOOL objects representing the
  available functions for tool calls.

## Value

A list the same length as `.llms`. Successful elements are `LLMMessage`
objects; failed ones, under `.on_error = "continue"`, are the condition
that failed, so nothing is silently dropped and a downstream
[`get_reply()`](https://edubruell.github.io/tidyllm/reference/get_reply.md)
fails loudly on exactly the elements that have no reply.

## Details

Neither streaming nor tool calls are supported here, and both are
refused rather than quietly ignored. A tool call is a conversation, not
a request: its rounds would run one after another after the parallel
phase, which is a surprising performance cliff rather than a feature.
Use
[`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md)
for those, which can have several conversations in flight at once.

## Examples

``` r
if (FALSE) { # \dontrun{
questions <- list(
  physics = llm_message("What is a photon?"),
  biology = llm_message("What is a ribosome?")
)
answers <- parallel_chat(questions, claude())
purrr::map_chr(answers, get_reply)
} # }
```
